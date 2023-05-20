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
fn check_constexpr_function_local_const_uint() {
    check_types("void f() { const uint s = 1; float x[s]; }");
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
    check_types("enum E { ONE = 1 }; void f() { assert_eval<E>(+ONE, ONE); }");
}

#[test]
fn check_constexpr_minus() {
    check_types("static const int x = -3; void f() { assert_eval<int>(-x, (int)3); }");
    check_types("static const half x = 1.0; void f() { assert_eval<half>(-x, -1.0h); }");
    check_types("static const float x = 1.0; void f() { assert_eval<float>(-x, -1.0); }");
    check_types("static const double x = 1.0; void f() { assert_eval<double>(-x, -1.0L); }");
    check_types("enum E { ONE = 1 }; void f() { assert_eval<E>(-ONE, (E)-1); }");
}

#[test]
fn check_constexpr_logical_not() {
    check_types("static const bool x = false; void f() { assert_eval<bool>(!x, true); }");
    check_types("static const uint x = 0; void f() { assert_eval<bool>(!x, true); }");
    check_types("static const int x = 0; void f() { assert_eval<bool>(!x, true); }");
    check_types("static const float x = 0; void f() { assert_eval<bool>(!x, true); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(!ZERO, true); assert_eval<bool>(!ONE, false); }");
}

#[test]
fn check_constexpr_bitwise_not() {
    check_types("static const uint x = 3; void f() { assert_eval<uint>(~x, 0xFFFFFFFCu); }");
    check_types("static const int x = 3; void f() { assert_eval<int>(~x, (int)0xFFFFFFFC); }");
    check_types("static const bool x = false; void f() { assert_eval<int>(~x, (int)0xFFFFFFFF); }");

    check_types("void f() { assert_eval(~2, -3); }");
    check_types("void f() { assert_eval<int>(~(int)2, (int)-3); }");
    check_types("void f() { assert_eval<uint>(~2u, 0xFFFFFFFDu); }");
    check_types("void f() { assert_eval<int>(~true, (int)-2); }");
    check_types("enum E { A = 1 }; void f() { assert_eval<E>(~A, (E)0xFFFFFFFE); }");

    check_types("enum E { NONE, ALL = 0xFFFFFFFFu }; float x[~NONE];");
    check_fail("enum E { NONE, ALL = 0xFFFFFFFFu }; float x[~ALL];");
}

#[test]
fn check_constexpr_add() {
    check_types("float x[assert_eval(1 + 1, 2)];");
    check_types("float x[assert_eval(1u + 1u, 2u)];");

    check_types("void f() { assert_eval(2 + 1, 3); }");
    check_types("void f() { assert_eval<int>((int)2 + (int)1, (int)3); }");
    check_types("void f() { assert_eval<uint>(2u + 1u, 3u); }");
    check_types("void f() { assert_eval<int>(true + false, (int)1); }");
    check_types("enum E { A = 1 }; void f() { assert_eval<E>(A + A, (E)2); }");

    check_types("enum E { ZERO, ONE }; float x[ONE + ZERO];");
    check_fail("enum E { ZERO, ONE }; float x[ZERO + ZERO];");
}

#[test]
fn check_constexpr_subtract() {
    check_types("float x[assert_eval(2 - 1, 1)];");
    check_fail("float x[1 - 1];");

    check_types("float x[assert_eval(2u - 1u, 1u)];");
    check_fail("float x[1u - 1u];");

    check_types("void f() { assert_eval(2 - 1, 1); }");
    check_types("void f() { assert_eval<int>((int)2 - (int)1, (int)1); }");
    check_types("void f() { assert_eval<uint>(2u - 1u, 1u); }");
    check_types("void f() { assert_eval<int>(true - false, (int)1); }");
    check_types("enum E { A = 1 }; void f() { assert_eval<E>(A - A, (E)0); }");

    check_types("enum E { ZERO, ONE }; float x[ONE - ZERO];");
    check_fail("enum E { ZERO, ONE }; float x[ONE - ONE];");
}

#[test]
fn check_constexpr_multiple() {
    check_types("float x[assert_eval(1 * 1, 1)];");
    check_fail("float x[1 * 0];");

    check_types("float x[assert_eval(1u * 1u, 1u)];");
    check_fail("float x[1u * 0u];");

    check_types("void f() { assert_eval(2 * 3, 6); }");
    check_types("void f() { assert_eval<int>((int)2 * (int)3, (int)6); }");
    check_types("void f() { assert_eval<uint>(2u * 3u, 6u); }");
    check_types("void f() { assert_eval<int>(true * false, (int)0); }");
    check_types("enum E { TWO = 2, FOUR = 4 }; void f() { assert_eval<E>(TWO * TWO, FOUR); }");
}

#[test]
fn check_constexpr_divide() {
    check_types("float x[assert_eval(1 / 1, 1)];");
    check_fail("float x[1 / 0];");
    check_fail("float x[1 / 2];");

    check_types("float x[assert_eval(1u / 1u, 1u)];");
    check_fail("float x[1u / 0u];");
    check_fail("float x[1u / 2u];");

    check_types("void f() { assert_eval(12 / 2, 6); }");
    check_types("void f() { assert_eval<int>((int)12 / (int)2, (int)6); }");
    check_types("void f() { assert_eval<uint>(12u / 2u, 6u); }");
    check_types("void f() { assert_eval<int>(true / true, (int)1); }");
    check_types("enum E { TWO = 2, FOUR = 4 }; void f() { assert_eval<E>(FOUR / TWO, TWO); }");
}

#[test]
fn check_constexpr_modulus() {
    check_types("float x[assert_eval(1 % 2, 1)];");
    check_fail("float x[1 % 1];");
    check_fail("float x[1 % 0];");

    check_types("float x[assert_eval(1u % 2u, 1u)];");
    check_fail("float x[1u % 1u];");
    check_fail("float x[1u % 0u];");

    check_types("void f() { assert_eval(9 % 5, 4); }");
    check_types("void f() { assert_eval<int>((int)9 % (int)5, (int)4); }");
    check_types("void f() { assert_eval<uint>(19u % 5u, 4u); }");
    check_types("void f() { assert_eval<int>(true % true, (int)0); }");
    check_types(
        "enum E { TWO = 2, FOUR = 4, SIX = 6 }; void f() { assert_eval<E>(SIX % FOUR, TWO); }",
    );
}

#[test]
fn check_constexpr_left_shift() {
    check_types("float x[assert_eval(1 << 1, 2)];");
    check_fail("float x[0 << 1];");

    check_types("float x[assert_eval(1u << 1u, 2u)];");
    check_fail("float x[0u << 1u];");

    check_types("void f() { assert_eval(2 << 2, 8); }");
    check_types("void f() { assert_eval<int>((int)2 << (int)2, (int)8); }");
    check_types("void f() { assert_eval<uint>(2u << 2u, 8u); }");
    check_types("void f() { assert_eval<int>(true << true, (int)2); }");
    check_types("enum E { A = 1 }; void f() { assert_eval<E>(A << A, (E)2); }");

    check_types("enum E { ZERO, ONE }; float x[ONE << ONE];");
    check_fail("enum E { ZERO, ONE }; float x[ZERO << ONE];");
}

#[test]
fn check_constexpr_right_shift() {
    check_types("float x[assert_eval(2 >> 1, 1)];");
    check_fail("float x[1 >> 1];");

    check_types("float x[assert_eval(2u >> 1u, 1u)];");
    check_fail("float x[1u >> 1u];");

    check_types("void f() { assert_eval(2 >> 1, 1); }");
    check_types("void f() { assert_eval<int>((int)2 >> (int)1, (int)1); }");
    check_types("void f() { assert_eval<uint>(2u >> 1u, 1u); }");
    check_types("void f() { assert_eval<int>(true >> false, (int)1); }");
    check_types("enum E { A = 1 }; void f() { assert_eval<E>(A >> A, (E)0); }");

    check_types("enum E { ZERO, ONE }; float x[ONE >> ZERO];");
    check_fail("enum E { ZERO, ONE }; float x[ONE >> ONE];");
}

#[test]
fn check_constexpr_bitwise_and() {
    check_types("void f() { assert_eval(0 & 0, 0); }");
    check_types("void f() { assert_eval(0 & 1, 0); }");
    check_types("void f() { assert_eval(1 & 0, 0); }");
    check_types("void f() { assert_eval(1 & 1, 1); }");
    check_types("void f() { assert_eval<int>((int)0 & (int)0, (int)0); }");
    check_types("void f() { assert_eval<int>((int)0 & (int)1, (int)0); }");
    check_types("void f() { assert_eval<int>((int)1 & (int)0, (int)0); }");
    check_types("void f() { assert_eval<int>((int)1 & (int)1, (int)1); }");
    check_types("void f() { assert_eval<uint>(0u & 0u, 0u); }");
    check_types("void f() { assert_eval<uint>(0u & 1u, 0u); }");
    check_types("void f() { assert_eval<uint>(1u & 0u, 0u); }");
    check_types("void f() { assert_eval<uint>(1u & 1u, 1u); }");
    check_types("void f() { assert_eval<int>(true & true, (int)1); }");
    check_types("void f() { assert_eval<int>(true & false, (int)0); }");
    check_types("void f() { assert_eval<int>(false & true, (int)0); }");
    check_types("void f() { assert_eval<int>(false & false, (int)0); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<E>(ZERO & ZERO, ZERO); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<E>(ZERO & ONE, ZERO); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<E>(ONE & ZERO, ZERO); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<E>(ONE & ONE, ONE); }");
}

#[test]
fn check_constexpr_bitwise_or() {
    check_types("void f() { assert_eval(0 | 0, 0); }");
    check_types("void f() { assert_eval(0 | 1, 1); }");
    check_types("void f() { assert_eval(1 | 0, 1); }");
    check_types("void f() { assert_eval(1 | 1, 1); }");
    check_types("void f() { assert_eval<int>((int)0 | (int)0, (int)0); }");
    check_types("void f() { assert_eval<int>((int)0 | (int)1, (int)1); }");
    check_types("void f() { assert_eval<int>((int)1 | (int)0, (int)1); }");
    check_types("void f() { assert_eval<int>((int)1 | (int)1, (int)1); }");
    check_types("void f() { assert_eval<uint>(0u | 0u, 0u); }");
    check_types("void f() { assert_eval<uint>(0u | 1u, 1u); }");
    check_types("void f() { assert_eval<uint>(1u | 0u, 1u); }");
    check_types("void f() { assert_eval<uint>(1u | 1u, 1u); }");
    check_types("void f() { assert_eval<int>(true | true, (int)1); }");
    check_types("void f() { assert_eval<int>(true | false, (int)1); }");
    check_types("void f() { assert_eval<int>(false | true, (int)1); }");
    check_types("void f() { assert_eval<int>(false | false, (int)0); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<E>(ZERO | ZERO, ZERO); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<E>(ZERO | ONE, ONE); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<E>(ONE | ZERO, ONE); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<E>(ONE | ONE, ONE); }");
}

#[test]
fn check_constexpr_bitwise_xor() {
    check_types("void f() { assert_eval(0 ^ 0, 0); }");
    check_types("void f() { assert_eval(0 ^ 1, 1); }");
    check_types("void f() { assert_eval(1 ^ 0, 1); }");
    check_types("void f() { assert_eval(1 ^ 1, 0); }");
    check_types("void f() { assert_eval<int>((int)0 ^ (int)0, (int)0); }");
    check_types("void f() { assert_eval<int>((int)0 ^ (int)1, (int)1); }");
    check_types("void f() { assert_eval<int>((int)1 ^ (int)0, (int)1); }");
    check_types("void f() { assert_eval<int>((int)1 ^ (int)1, (int)0); }");
    check_types("void f() { assert_eval<uint>(0u ^ 0u, 0u); }");
    check_types("void f() { assert_eval<uint>(0u ^ 1u, 1u); }");
    check_types("void f() { assert_eval<uint>(1u ^ 0u, 1u); }");
    check_types("void f() { assert_eval<uint>(1u ^ 1u, 0u); }");
    check_types("void f() { assert_eval<int>(true ^ true, (int)0); }");
    check_types("void f() { assert_eval<int>(true ^ false, (int)1); }");
    check_types("void f() { assert_eval<int>(false ^ true, (int)1); }");
    check_types("void f() { assert_eval<int>(false ^ false, (int)0); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<E>(ZERO ^ ZERO, ZERO); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<E>(ZERO ^ ONE, ONE); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<E>(ONE ^ ZERO, ONE); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<E>(ONE ^ ONE, ZERO); }");
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
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(ZERO && ZERO, false); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(ZERO && ONE, false); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(ONE && ZERO, false); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(ONE && ONE, true); }");
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
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(ZERO || ZERO, false); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(ZERO || ONE, true); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(ONE || ZERO, true); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(ONE || ONE, true); }");
}

#[test]
fn check_constexpr_less_than() {
    check_types("void f() { assert_eval<bool>(true < true, false); }");
    check_types("void f() { assert_eval<bool>(0 < 1, true); }");
    check_types("void f() { assert_eval<bool>(0u < 1u, true); }");
    check_types("void f() { assert_eval<bool>(0.0 < 1.0, true); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(ZERO < ONE, true); }");
}

#[test]
fn check_constexpr_less_equal() {
    check_types("void f() { assert_eval<bool>(true <= true, true); }");
    check_types("void f() { assert_eval<bool>(0 <= 1, true); }");
    check_types("void f() { assert_eval<bool>(0u <= 1u, true); }");
    check_types("void f() { assert_eval<bool>(0.0 <= 1.0, true); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(ZERO <= ONE, true); }");
}

#[test]
fn check_constexpr_greater_than() {
    check_types("void f() { assert_eval<bool>(true > true, false); }");
    check_types("void f() { assert_eval<bool>(0 > 1, false); }");
    check_types("void f() { assert_eval<bool>(0u > 1u, false); }");
    check_types("void f() { assert_eval<bool>(0.0 > 1.0, false); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(ZERO > ONE, false); }");
}

#[test]
fn check_constexpr_greater_equal() {
    check_types("void f() { assert_eval<bool>(true >= true, true); }");
    check_types("void f() { assert_eval<bool>(0 >= 1, false); }");
    check_types("void f() { assert_eval<bool>(0u >= 1u, false); }");
    check_types("void f() { assert_eval<bool>(0.0 >= 1.0, false); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(ZERO >= ONE, false); }");
}

#[test]
fn check_constexpr_equality() {
    check_types("void f() { assert_eval<bool>(true == true, true); }");
    check_types("void f() { assert_eval<bool>(0 == 1, false); }");
    check_types("void f() { assert_eval<bool>(0u == 1u, false); }");
    check_types("void f() { assert_eval<bool>(0.0 == 1.0, false); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(ZERO == ONE, false); }");
}

#[test]
fn check_constexpr_inequality() {
    check_types("void f() { assert_eval<bool>(true != true, false); }");
    check_types("void f() { assert_eval<bool>(0 != 1, true); }");
    check_types("void f() { assert_eval<bool>(0u != 1u, true); }");
    check_types("void f() { assert_eval<bool>(0.0 != 1.0, true); }");
    check_types("enum E { ZERO, ONE }; void f() { assert_eval<bool>(ZERO != ONE, true); }");
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
