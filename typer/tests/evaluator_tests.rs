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

    // Adding a type assert doesn't break the evaluation chain
    check_types("float x[assert_type<uint>(1u)];");
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
fn check_constexpr_plus() {
    check_types("static const uint x = 3; void f() { assert_eval<uint>(+x, 3u); }");
    check_types("static const int x = -3; void f() { assert_eval<int>(+x, (int)-3); }");
    check_types("static const bool x = true; void f() { assert_eval<bool>(+x, true); }");
    check_types("static const half x = 1.0; void f() { assert_eval<half>(+x, 1.0H); }");
    check_types("static const float x = 1.0; void f() { assert_eval<float>(+x, 1.0); }");
    check_types("static const double x = 1.0; void f() { assert_eval<double>(+x, 1.0l); }");
}

#[test]
fn check_constexpr_minus() {
    check_types("static const int x = -3; void f() { assert_eval<int>(-x, (int)3); }");
    check_types("static const half x = 1.0; void f() { assert_eval<half>(-x, -1.0h); }");
    check_types("static const float x = 1.0; void f() { assert_eval<float>(-x, -1.0); }");
    check_types("static const double x = 1.0; void f() { assert_eval<double>(-x, -1.0L); }");
}

#[test]
fn check_constexpr_logical_not() {
    check_types("static const bool x = false; void f() { assert_eval<bool>(!x, true); }");
    check_types("static const uint x = 0; void f() { assert_eval<bool>(!x, true); }");
    check_types("static const int x = 0; void f() { assert_eval<bool>(!x, true); }");
    check_types("static const float x = 0; void f() { assert_eval<bool>(!x, true); }");
}

#[test]
fn check_constexpr_bitwise_not() {
    check_types("static const uint x = 3; void f() { assert_eval<uint>(~x, 0xFFFFFFFCu); }");
    check_types("static const int x = 3; void f() { assert_eval<int>(~x, (int)0xFFFFFFFC); }");
    check_types("static const bool x = false; void f() { assert_eval<int>(~x, (int)0xFFFFFFFF); }");
}

#[test]
fn check_constexpr_add_integer() {
    // add two literal untyped ints - which creates a signed int add
    check_types("float x[assert_eval(1 + 1, (int)2)];");

    // add two literal uints
    check_types("float x[assert_eval(1u + 1u, 2u)];");
}

#[test]
fn check_constexpr_subtract_integer() {
    check_types("float x[assert_eval(2 - 1, (int)1)];");
    check_fail("float x[1 - 1];");

    check_types("float x[assert_eval(2u - 1u, 1u)];");
    check_fail("float x[1u - 1u];");
}

#[test]
fn check_constexpr_multiple_integer() {
    check_types("float x[assert_eval(1 * 1, (int)1)];");
    check_fail("float x[1 * 0];");

    check_types("float x[assert_eval(1u * 1u, 1u)];");
    check_fail("float x[1u * 0u];");
}

#[test]
fn check_constexpr_divide_integer() {
    check_types("float x[assert_eval(1 / 1, (int)1)];");
    check_fail("float x[1 / 0];");
    check_fail("float x[1 / 2];");

    check_types("float x[assert_eval(1u / 1u, 1u)];");
    check_fail("float x[1u / 0u];");
    check_fail("float x[1u / 2u];");
}

#[test]
fn check_constexpr_modulus_integer() {
    check_types("float x[assert_eval(1 % 2, (int)1)];");
    check_fail("float x[1 % 1];");
    check_fail("float x[1 % 0];");

    check_types("float x[assert_eval(1u % 2u, 1u)];");
    check_fail("float x[1u % 1u];");
    check_fail("float x[1u % 0u];");
}

#[test]
fn check_constexpr_left_shift() {
    check_types("float x[assert_eval(1 << 1, (int)2)];");
    check_fail("float x[0 << 1];");

    check_types("float x[assert_eval(1u << 1u, 2u)];");
    check_fail("float x[0u << 1u];");
}

#[test]
fn check_constexpr_right_shift() {
    check_types("float x[assert_eval(2 >> 1, (int)1)];");
    check_fail("float x[1 >> 1];");

    check_types("float x[assert_eval(2u >> 1u, 1u)];");
    check_fail("float x[1u >> 1u];");
}

#[test]
fn check_constexpr_boolean_and() {
    check_types("void f() { assert_eval<bool>(true && true, true); }");
    check_types("void f() { assert_eval<bool>(true && false, false); }");
    check_types("void f() { assert_eval<bool>(false && true, false); }");
    check_types("void f() { assert_eval<bool>(false && false, false); }");
    check_types("void f() { assert_eval<bool>(0 && 0, false); }");
    check_types("void f() { assert_eval<bool>(0 && 1, false); }");
    check_types("void f() { assert_eval<bool>(1 && 0, false); }");
    check_types("void f() { assert_eval<bool>(1 && 1, true); }");
}

#[test]
fn check_constexpr_boolean_or() {
    check_types("void f() { assert_eval<bool>(true || true, true); }");
    check_types("void f() { assert_eval<bool>(true || false, true); }");
    check_types("void f() { assert_eval<bool>(false || true, true); }");
    check_types("void f() { assert_eval<bool>(false || false, false); }");
    check_types("void f() { assert_eval<bool>(0 || 0, false); }");
    check_types("void f() { assert_eval<bool>(0 || 1, true); }");
    check_types("void f() { assert_eval<bool>(1 || 0, true); }");
    check_types("void f() { assert_eval<bool>(1 || 1, true); }");
}

#[test]
fn check_constexpr_less_than() {
    check_types("void f() { assert_eval<bool>(true < true, false); }");
    check_types("void f() { assert_eval<bool>(0 < 1, true); }");
    check_types("void f() { assert_eval<bool>(0u < 1u, true); }");
    check_types("void f() { assert_eval<bool>(0.0 < 1.0, true); }");
}

#[test]
fn check_constexpr_less_equal() {
    check_types("void f() { assert_eval<bool>(true <= true, true); }");
    check_types("void f() { assert_eval<bool>(0 <= 1, true); }");
    check_types("void f() { assert_eval<bool>(0u <= 1u, true); }");
    check_types("void f() { assert_eval<bool>(0.0 <= 1.0, true); }");
}

#[test]
fn check_constexpr_greater_than() {
    check_types("void f() { assert_eval<bool>(true > true, false); }");
    check_types("void f() { assert_eval<bool>(0 > 1, false); }");
    check_types("void f() { assert_eval<bool>(0u > 1u, false); }");
    check_types("void f() { assert_eval<bool>(0.0 > 1.0, false); }");
}

#[test]
fn check_constexpr_greater_equal() {
    check_types("void f() { assert_eval<bool>(true >= true, true); }");
    check_types("void f() { assert_eval<bool>(0 >= 1, false); }");
    check_types("void f() { assert_eval<bool>(0u >= 1u, false); }");
    check_types("void f() { assert_eval<bool>(0.0 >= 1.0, false); }");
}

#[test]
fn check_constexpr_equality() {
    check_types("void f() { assert_eval<bool>(true == true, true); }");
    check_types("void f() { assert_eval<bool>(0 == 1, false); }");
    check_types("void f() { assert_eval<bool>(0u == 1u, false); }");
    check_types("void f() { assert_eval<bool>(0.0 == 1.0, false); }");
}

#[test]
fn check_constexpr_inequality() {
    check_types("void f() { assert_eval<bool>(true != true, false); }");
    check_types("void f() { assert_eval<bool>(0 != 1, true); }");
    check_types("void f() { assert_eval<bool>(0u != 1u, true); }");
    check_types("void f() { assert_eval<bool>(0.0 != 1.0, true); }");
}

#[test]
fn check_constexpr_sizeof() {
    check_types("void f() { assert_eval<uint>(sizeof(int), 4u); }");
    check_types("void f() { assert_eval<uint>(sizeof(uint), 4u); }");
    check_types("void f() { assert_eval<uint>(sizeof(bool), 4u); }");
    check_types("void f() { assert_eval<uint>(sizeof(half), 2u); }");
    check_types("void f() { assert_eval<uint>(sizeof(float), 4u); }");
    check_types("void f() { assert_eval<uint>(sizeof(double), 8u); }");
    check_types("enum E { A = false, B = 0u }; void f() { assert_eval<uint>(sizeof(E), 4u); }");
}
