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
