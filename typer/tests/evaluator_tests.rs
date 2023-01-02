mod shared;
use shared::*;

#[test]
fn check_constexpr_literal_untyped_int() {
    // Literal untyped int directly into array parameter
    check_types("float x[1];");
}

#[test]
fn check_constexpr_literal_uint() {
    // Literal uint directly into array parameter
    check_types("float x[1u];");
}

#[test]
fn check_constexpr_static_const_uint() {
    // Literal uint into s
    // Static const into array parameter
    check_types("static const uint s = 1; float x[s];");
}

#[test]
fn check_constexpr_static_const_int() {
    // Literal int into s - the untyped int is promoted to signed int before evaluation
    // Static const into array parameter
    check_types("static const int s = 1; float x[s];");
}

#[test]
fn check_constexpr_static_const_bool() {
    // literal bool into static const
    // static bool casted to uint 1 for array parameter
    check_types("static const bool s = true; float x[s];");
    // static bool casted to uint 0 for array parameter - which is an invalid array size
    check_fail("static const bool s = false; float x[s];");
}

#[test]
fn check_constexpr_cast_integers() {
    // literal untyped int to uint
    check_types("float x[(uint)1];");

    // literal untyped int to int
    check_types("float x[(int)1];");

    // literal uint to uint
    check_types("float x[(uint)1u];");

    // literal uint to int
    check_types("float x[(int)1u];");
}

#[test]
fn check_constexpr_add_integer() {
    // add two literal untyped ints - which creates a signed int add
    check_types("float x[1 + 1];");

    // add two literal uints
    check_types("float x[1u + 1u];");
}

#[test]
fn check_constexpr_subtract_integer() {
    check_types("float x[2 - 1];");
    check_fail("float x[1 - 1];");

    check_types("float x[2u - 1u];");
    check_fail("float x[1u - 1u];");
}

#[test]
fn check_constexpr_multiple_integer() {
    check_types("float x[1 * 1];");
    check_fail("float x[1 * 0];");

    check_types("float x[1u * 1u];");
    check_fail("float x[1u * 0u];");
}

#[test]
fn check_constexpr_divide_integer() {
    check_types("float x[1 / 1];");
    check_fail("float x[1 / 0];");
    check_fail("float x[1 / 2];");

    check_types("float x[1u / 1u];");
    check_fail("float x[1u / 0u];");
    check_fail("float x[1u / 2u];");
}

#[test]
fn check_constexpr_modulus_integer() {
    check_types("float x[1 % 2];");
    check_fail("float x[1 % 1];");
    check_fail("float x[1 % 0];");

    check_types("float x[1u % 2u];");
    check_fail("float x[1u % 1u];");
    check_fail("float x[1u % 0u];");
}

#[test]
fn check_constexpr_left_shift() {
    check_types("float x[1 << 1];");
    check_fail("float x[0 << 1];");

    check_types("float x[1u << 1u];");
    check_fail("float x[0u << 1u];");
}

#[test]
fn check_constexpr_right_shift() {
    check_types("float x[2 >> 1];");
    check_fail("float x[1 >> 1];");

    check_types("float x[2u >> 1u];");
    check_fail("float x[1u >> 1u];");
}