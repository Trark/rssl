mod shared;
use shared::*;

#[test]
fn check_byte_buffer() {
    check_hlsl(
        include_str!("basic/byte_buffer.rssl"),
        include_str!("basic/byte_buffer.hlsl"),
    );
}
