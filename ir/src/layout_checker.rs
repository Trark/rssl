use rssl_text::*;

use crate::*;

/// Check that buffers have similar layout between different platforms
pub fn check_layout(module: &Module) -> Result<(), LayoutError> {
    for global in &module.global_registry {
        let ty = module.type_registry.remove_modifier(global.type_id);
        let tyl = module.type_registry.get_type_layer(ty);
        let o = match tyl {
            TypeLayer::Object(o) => o,
            _ => continue,
        };
        match o {
            ObjectType::StructuredBuffer(st) | ObjectType::RWStructuredBuffer(st) => {
                let mut layout_hlsl =
                    match get_type_layout(module, st, PackingMode::HlslStructuredBuffer) {
                        Some(layout) => layout,
                        None => return Err(LayoutError::UnknownLayout(global.name.location)),
                    };
                let mut layout_metal = match get_type_layout(module, st, PackingMode::Metal) {
                    Some(layout) => layout,
                    None => return Err(LayoutError::UnknownLayout(global.name.location)),
                };

                layout_hlsl.size = layout_hlsl.size.next_multiple_of(layout_hlsl.align);
                layout_metal.size = layout_metal.size.next_multiple_of(layout_metal.align);

                if layout_hlsl.size != layout_metal.size {
                    return Err(LayoutError::MismatchedLayout(
                        global.name.location,
                        layout_hlsl,
                        layout_metal,
                    ));
                }
            }
            _ => {}
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
