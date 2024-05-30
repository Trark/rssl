mod shared;
use shared::*;

#[test]
fn check_byte_buffer() {
    check_msl(
        include_str!("basic/byte_buffer.rssl"),
        include_str!("basic/byte_buffer.metal"),
    );
}

#[test]
fn check_buffer_address() {
    check_msl(
        include_str!("basic/buffer_address.rssl"),
        include_str!("basic/buffer_address.metal"),
    );
}

#[test]
fn check_structured_buffer() {
    check_msl(
        include_str!("basic/structured_buffer.rssl"),
        include_str!("basic/structured_buffer.metal"),
    );
}

#[test]
fn check_constant_buffer() {
    check_msl(
        include_str!("basic/constant_buffer.rssl"),
        include_str!("basic/constant_buffer.metal"),
    );
}

#[test]
fn check_texel_buffer() {
    check_msl(
        include_str!("basic/texel_buffer.rssl"),
        include_str!("basic/texel_buffer.metal"),
    );
}

#[test]
fn check_texture2d() {
    check_msl(
        include_str!("basic/texture2d.rssl"),
        include_str!("basic/texture2d.metal"),
    );
}

#[test]
fn check_texture2darray() {
    check_msl(
        include_str!("basic/texture2darray.rssl"),
        include_str!("basic/texture2darray.metal"),
    );
}

#[test]
fn check_texturecube() {
    check_msl(
        include_str!("basic/texturecube.rssl"),
        include_str!("basic/texturecube.metal"),
    );
}

#[test]
fn check_texture3d() {
    check_msl(
        include_str!("basic/texture3d.rssl"),
        include_str!("basic/texture3d.metal"),
    );
}

#[test]
fn check_bindless() {
    check_msl(
        include_str!("basic/bindless.rssl"),
        include_str!("basic/bindless.metal"),
    );
}

#[test]
fn check_wave_intrinsics() {
    check_msl(
        include_str!("basic/wave_intrinsics.rssl"),
        include_str!("basic/wave_intrinsics.metal"),
    );
}

#[test]
fn check_compute_pipeline() {
    check_msl(
        include_str!("basic/compute_pipeline.rssl"),
        include_str!("basic/compute_pipeline.metal"),
    );
}

#[test]
fn check_vertex_pixel_pipeline() {
    check_msl(
        include_str!("basic/vertex_pixel_pipeline.rssl"),
        include_str!("basic/vertex_pixel_pipeline.metal"),
    );
}

#[test]
fn check_mesh_pixel_pipeline() {
    check_msl(
        include_str!("basic/mesh_pixel_pipeline.rssl"),
        include_str!("basic/mesh_pixel_pipeline.metal"),
    );
}
