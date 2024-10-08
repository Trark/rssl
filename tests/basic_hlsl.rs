mod shared;
use shared::*;

#[test]
fn check_byte_buffer() {
    check_hlsl(
        include_str!("basic/byte_buffer.rssl"),
        include_str!("basic/byte_buffer.hlsl"),
    );
}

#[test]
fn check_buffer_address() {
    check_hlsl(
        include_str!("basic/buffer_address.rssl"),
        include_str!("basic/buffer_address.hlsl"),
    );

    check_hlsl_vk(
        include_str!("basic/buffer_address.rssl"),
        include_str!("basic/buffer_address.vk.hlsl"),
    );
}

#[test]
fn check_structured_buffer() {
    check_hlsl(
        include_str!("basic/structured_buffer.rssl"),
        include_str!("basic/structured_buffer.hlsl"),
    );
}

#[test]
fn check_constant_buffer() {
    check_hlsl(
        include_str!("basic/constant_buffer.rssl"),
        include_str!("basic/constant_buffer.hlsl"),
    );
}

#[test]
fn check_texel_buffer() {
    check_hlsl(
        include_str!("basic/texel_buffer.rssl"),
        include_str!("basic/texel_buffer.hlsl"),
    );
}

#[test]
fn check_texture2d() {
    check_hlsl(
        include_str!("basic/texture2d.rssl"),
        include_str!("basic/texture2d.hlsl"),
    );
}

#[test]
fn check_texture2darray() {
    check_hlsl(
        include_str!("basic/texture2darray.rssl"),
        include_str!("basic/texture2darray.hlsl"),
    );
}

#[test]
fn check_texturecube() {
    check_hlsl(
        include_str!("basic/texturecube.rssl"),
        include_str!("basic/texturecube.hlsl"),
    );
}

#[test]
fn check_texture3d() {
    check_hlsl(
        include_str!("basic/texture3d.rssl"),
        include_str!("basic/texture3d.hlsl"),
    );
}

#[test]
fn check_bindless() {
    check_hlsl(
        include_str!("basic/bindless.rssl"),
        include_str!("basic/bindless.hlsl"),
    );

    check_hlsl_vk(
        include_str!("basic/bindless.rssl"),
        include_str!("basic/bindless.vk.hlsl"),
    );
}

#[test]
fn check_wave_intrinsics() {
    check_hlsl(
        include_str!("basic/wave_intrinsics.rssl"),
        include_str!("basic/wave_intrinsics.hlsl"),
    );
}

#[test]
fn check_compute_pipeline() {
    check_hlsl(
        include_str!("basic/compute_pipeline.rssl"),
        include_str!("basic/compute_pipeline.hlsl"),
    );
}

#[test]
fn check_vertex_pixel_pipeline() {
    check_hlsl(
        include_str!("basic/vertex_pixel_pipeline.rssl"),
        include_str!("basic/vertex_pixel_pipeline.hlsl"),
    );
}

#[test]
fn check_mesh_pixel_pipeline() {
    check_hlsl(
        include_str!("basic/mesh_pixel_pipeline.rssl"),
        include_str!("basic/mesh_pixel_pipeline.hlsl"),
    );
}

#[test]
fn check_task_mesh_pipeline() {
    check_hlsl(
        include_str!("basic/task_mesh_pipeline.rssl"),
        include_str!("basic/task_mesh_pipeline.hlsl"),
    );
}

#[test]
fn check_inline_raytracing() {
    check_hlsl(
        include_str!("basic/inline_raytracing.rssl"),
        include_str!("basic/inline_raytracing.hlsl"),
    );
}
