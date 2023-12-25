mod shared;
use shared::*;

#[test]
fn check_byte_buffer() {
    check_msl(
        include_str!("basic/byte_buffer.rssl"),
        include_str!("basic/byte_buffer.metal"),
    );
}
