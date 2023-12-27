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
