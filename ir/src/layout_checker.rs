use rssl_text::*;
use std::collections::HashSet;

use crate::*;

/// Check that buffers have similar layout between different platforms
pub fn check_layout(module: &Module) -> Result<(), LayoutError> {
    let mut types_to_check = Vec::new();
    let mut types_seen = HashSet::new();

    for global in &module.global_registry {
        let ty = module.type_registry.remove_modifier(global.type_id);
        let tyl = module.type_registry.get_type_layer(ty);
        let o = match tyl {
            TypeLayer::Object(o) => o,
            _ => continue,
        };
        match o {
            ObjectType::StructuredBuffer(st) | ObjectType::RWStructuredBuffer(st) => {
                if types_seen.insert(st) {
                    types_to_check.push((st, global.name.location));
                }
            }
            _ => {}
        }
    }

    for i in 0..module.function_registry.get_function_count() {
        let id = FunctionId(i);

        let intrinsic_data = match module.function_registry.get_intrinsic_data(id) {
            Some(intrinsic_data) => intrinsic_data,
            None => continue,
        };

        if !matches!(
            intrinsic_data,
            Intrinsic::ByteAddressBufferLoadT
                | Intrinsic::RWByteAddressBufferLoadT
                | Intrinsic::RWByteAddressBufferStore
                | Intrinsic::BufferAddressLoad
                | Intrinsic::RWBufferAddressLoad
                | Intrinsic::RWBufferAddressStore
        ) {
            continue;
        }

        let template_data = match module.function_registry.get_template_instantiation_data(id) {
            Some(template_data) => template_data,
            None => continue,
        };

        if template_data.template_args.len() != 1 {
            panic!("invalid {:?} intrinsic", intrinsic_data);
        }

        let ty = match template_data.template_args[0] {
            TypeOrConstant::Type(ty) => ty,
            TypeOrConstant::Constant(_) => panic!("invalid {:?} intrinsic", intrinsic_data),
        };

        if types_seen.insert(ty) {
            types_to_check.push((ty, module.get_type_location(ty)));
        }
    }

    for (ty, loc) in types_to_check {
        let mut layout_hlsl = match get_type_layout(module, ty, PackingMode::HlslStructuredBuffer) {
            Some(layout) => layout,
            None => return Err(LayoutError::UnknownLayout(loc)),
        };
        let mut layout_metal = match get_type_layout(module, ty, PackingMode::Metal) {
            Some(layout) => layout,
            None => return Err(LayoutError::UnknownLayout(loc)),
        };

        layout_hlsl.size = layout_hlsl.size.next_multiple_of(layout_hlsl.align);
        layout_metal.size = layout_metal.size.next_multiple_of(layout_metal.align);

        if layout_hlsl.size != layout_metal.size {
            return Err(LayoutError::MismatchedLayout(
                loc,
                layout_hlsl,
                layout_metal,
            ));
        }
    }

    Ok(())
}

pub enum LayoutError {
    UnknownLayout(SourceLocation),
    MismatchedLayout(SourceLocation, Layout, Layout),
}

impl CompileError for LayoutError {
    fn print(&self, w: &mut rssl_text::MessagePrinter) -> std::fmt::Result {
        match self {
            LayoutError::UnknownLayout(loc) => w.write_message(
                &|f| write!(f, "struct has unknown size"),
                *loc,
                Severity::Error,
            ),
            LayoutError::MismatchedLayout(loc, lhs, rhs) => w.write_message(
                &|f| {
                    write!(
                        f,
                        "struct has size={} align={} on HLSL but size={} align={} on Metal",
                        lhs.size, lhs.align, rhs.size, rhs.align,
                    )
                },
                *loc,
                Severity::Error,
            ),
        }
    }
}

#[derive(Copy, Clone)]
enum PackingMode {
    HlslStructuredBuffer,
    Metal,
}

#[derive(PartialEq, Debug)]
pub struct Layout {
    size: u32,
    align: u32,
}

fn get_type_layout(module: &Module, ty: TypeId, mode: PackingMode) -> Option<Layout> {
    let tyl = module.type_registry.get_type_layer(ty);
    match tyl {
        TypeLayer::Void => None,
        TypeLayer::Scalar(ScalarType::Bool) => None,
        TypeLayer::Scalar(st) => match st.get_size() {
            Some(size) => Some(Layout {
                size,
                // Assume all scalars have the same size and alignment
                align: size,
            }),
            None => panic!("unexpected unsized scalar"),
        },
        TypeLayer::Vector(ty, mut x) => {
            let mut layout = get_type_layout(module, ty, mode)?;
            match mode {
                PackingMode::HlslStructuredBuffer => layout.size *= x,
                PackingMode::Metal => {
                    x = x.next_power_of_two();
                    layout.size *= x;
                    layout.align = layout.size;
                }
            }
            Some(layout)
        }
        // Matrix not currently supported
        TypeLayer::Matrix(_, _, _) => None,
        TypeLayer::Struct(sid) => {
            let def = &module.struct_registry[sid.0 as usize];
            let mut layout = Layout { size: 0, align: 1 };
            for member in &def.members {
                let member_layout = get_type_layout(module, member.type_id, mode)?;
                layout.size = layout.size.next_multiple_of(member_layout.align);
                layout.size += member_layout.size;
                layout.align = layout.align.max(member_layout.align);
            }
            Some(layout)
        }
        TypeLayer::StructTemplate(_) => panic!("unexpected struct template"),
        TypeLayer::Enum(enum_id) => {
            let underlying = module.enum_registry.get_underlying_type_id(enum_id);
            get_type_layout(module, underlying, mode)
        }
        TypeLayer::Object(_) => None,
        TypeLayer::Array(ty, Some(count)) => {
            let mut layout = get_type_layout(module, ty, mode)?;
            layout.size *= u32::try_from(count).unwrap();
            Some(layout)
        }
        TypeLayer::Array(_, None) => None,
        TypeLayer::TemplateParam(_) => panic!("unexpected template param"),
        TypeLayer::Modifier(_, ty) => get_type_layout(module, ty, mode),
    }
}
