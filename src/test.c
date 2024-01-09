#include <check.h>
#include <math.h>
#include <stdlib.h>

#include "s21_math.h"

START_TEST(abs_tests) {
  ck_assert_int_eq(abs(-13), s21_abs(-13));
  ck_assert_int_eq(abs(13), s21_abs(13));
  ck_assert_int_eq(abs(-1000), s21_abs(-1000));
  ck_assert_int_eq(abs(32346234), s21_abs(32346234));
  ck_assert_int_eq(abs(0), s21_abs(0));
  ck_assert_int_eq(abs(1), s21_abs(1));
  ck_assert_int_eq(abs(-1), s21_abs(-1));
  ck_assert_int_eq(abs(1000), s21_abs(1000));
  ck_assert_int_eq(abs(2147483647), s21_abs(2147483647));
  ck_assert_int_eq(abs(-2147483646), s21_abs(-2147483646));
}
END_TEST

START_TEST(fabs_tests) {
  ck_assert_ldouble_eq_tol(fabs(-13.21313), s21_fabs(-13.21313), 1e-6);
  ck_assert_ldouble_eq_tol(fabs(13.21313), s21_fabs(13.21313), 1e-6);
  ck_assert_ldouble_eq_tol(fabs(-1000.21313), s21_fabs(-1000.21313), 1e-6);
  ck_assert_ldouble_eq_tol(fabs(32346234.21313), s21_fabs(32346234.21313),
                           1e-6);
  ck_assert_ldouble_eq_tol(fabs(0.0), s21_fabs(0.0), 1e-6);
  ck_assert_ldouble_eq_tol(fabs(1.1), s21_fabs(1.1), 1e-6);
  ck_assert_ldouble_eq_tol(fabs(-1.1), s21_fabs(-1.1), 1e-6);
  ck_assert_ldouble_eq_tol(fabs(2.5), s21_fabs(2.5), 1e-6);
  ck_assert_ldouble_eq_tol(fabs(DBL_MAX), s21_fabs(DBL_MAX), 1e-6);
  ck_assert_ldouble_eq_tol(fabs(DBL_MIN), s21_fabs(DBL_MIN), 1e-6);
}
END_TEST

START_TEST(fmod_tests) {
  ck_assert_ldouble_eq_tol(fmod(-13.21313, 2.5), s21_fmod(-13.21313, 2.5),
                           1e-6);
  ck_assert_ldouble_eq_tol(fmod(13.21313, 2.5), s21_fmod(13.21313, 2.5), 1e-6);
  ck_assert_ldouble_eq_tol(fmod(-1000.21313, 2.5), s21_fmod(-1000.21313, 2.5),
                           1e-6);
  ck_assert_ldouble_eq_tol(fmod(32346234.21313, 2.5),
                           s21_fmod(32346234.21313, 2.5), 1e-6);
  ck_assert_ldouble_eq_tol(fmod(0, 2.5), s21_fmod(0, 2.5), 1e-6);
  ck_assert_ldouble_eq_tol(fmod(2.5, 2.5), s21_fmod(2.5, 2.5), 1e-6);
  ck_assert_ldouble_eq_tol(fmod(0.0, 1), s21_fmod(0.0, 1), 1e-6);
  ck_assert_ldouble_eq_tol(fmod(2.5, 2.5), s21_fmod(2.5, 2.5), 1e-6);
  ck_assert_ldouble_eq_tol(fmod(2.5, -2.5), s21_fmod(2.5, -2.5), 1e-6);
  ck_assert_ldouble_eq_tol(fmod(-2.5, 2.5), s21_fmod(-2.5, 2.5), 1e-6);
  ck_assert_ldouble_eq_tol(fmod(-2.5, -2.5), s21_fmod(-2.5, -2.5), 1e-6);
  ck_assert_ldouble_eq_tol(fmod(13.21313, -2.5), s21_fmod(13.21313, -2.5),
                           1e-6);
  ck_assert_ldouble_eq_tol(fmod(-13.21313, -2.5), s21_fmod(-13.21313, -2.5),
                           1e-6);
  ck_assert_int_eq(fmod(2.5, S21_NAN), s21_fmod(2.5, S21_NAN));
  ck_assert_int_eq(fmod(S21_NAN, 2.5), s21_fmod(S21_NAN, 2.5));
  ck_assert_int_eq(fmod(-2.5, S21_NAN), s21_fmod(-2.5, S21_NAN));
  ck_assert_int_eq(fmod(S21_NAN, -2.5), s21_fmod(S21_NAN, -2.5));
  ck_assert_int_eq(fmod(S21_NAN, S21_NAN), s21_fmod(S21_NAN, S21_NAN));
  ck_assert_int_eq(fmod(S21_INF, 2.5), s21_fmod(S21_INF, 2.5));
  ck_assert_int_eq(fmod(S21_INF, -2.5), s21_fmod(S21_INF, -2.5));
  ck_assert_int_eq(fmod(S21_INF, S21_INF), s21_fmod(S21_INF, S21_INF));
  ck_assert_ldouble_eq_tol(fmod(2.5, S21_INF), s21_fmod(2.5, S21_INF), 1e-6);
  ck_assert_ldouble_eq_tol(fmod(-2.5, S21_INF), s21_fmod(-2.5, S21_INF), 1e-6);
}
END_TEST

START_TEST(pow_tests) {
  // ck_assert_ldouble_eq_tol(pow(-13.21313, 2.5), s21_pow(-13.21313, 2.5),
  // 1e-6);
  ck_assert_ldouble_eq_tol(pow(13.21313, 2.5), s21_pow(13.21313, 2.5), 1e-6);
  // ck_assert_ldouble_eq_tol(pow(-1000.21313, 2.5), s21_pow(-1000.21313, 2.5),
  // 1e-6);
  ck_assert_float_eq(pow(32346234.21313, 2.5), s21_pow(32346234.21313, 2.5));
  ck_assert_ldouble_eq_tol(pow(0, 2.5), s21_pow(0, 2.5), 1e-6);
  ck_assert_ldouble_eq_tol(pow(123, -2), s21_pow(123, -2), 1e-6);
  ck_assert_ldouble_eq_tol(pow(0.0, 1), s21_pow(0.0, 1), 1e-6);
  ck_assert_ldouble_eq_tol(pow(2.5, 2.5), s21_pow(2.5, 2.5), 1e-6);
  ck_assert_ldouble_eq_tol(pow(2.5, -2.5), s21_pow(2.5, -2.5), 1e-6);
  // ck_assert_ldouble_eq_tol(pow(-2.5, 2.5), s21_pow(-2.5, 2.5), 1e-6);
  // ck_assert_ldouble_eq_tol(pow(-2.5, -2.5), s21_pow(-2.5, -2.5), 1e-6);
  ck_assert_ldouble_eq_tol(pow(13.21313, -2.5), s21_pow(13.21313, -2.5), 1e-6);
  // ck_assert_ldouble_eq_tol(pow(-13.21313, -2.5), s21_pow(-13.21313, -2.5),
  // 1e-6);
  ck_assert_int_eq(pow(2.5, S21_NAN), s21_pow(2.5, S21_NAN));
  ck_assert_int_eq(pow(S21_NAN, 2.5), s21_pow(S21_NAN, 2.5));
  // ck_assert_ldouble_eq_tol(pow(-2.5, S21_NAN), s21_pow(-2.5, S21_NAN), 1e-6);
  ck_assert_int_eq(pow(S21_NAN, -2.5), s21_pow(S21_NAN, -2.5));
  ck_assert_int_eq(pow(S21_NAN, S21_NAN), s21_pow(S21_NAN, S21_NAN));
  ck_assert_ldouble_eq(pow(2.5, S21_INF), s21_pow(2.5, S21_INF));
  ck_assert_ldouble_eq(pow(S21_INF, 2.5), s21_pow(S21_INF, 2.5));
  ck_assert_ldouble_eq(pow(-2.5, S21_INF), s21_pow(-2.5, S21_INF));
  ck_assert_ldouble_eq(pow(S21_INF, -2.5), s21_pow(S21_INF, -2.5));
  ck_assert_ldouble_eq(pow(S21_INF, S21_INF), s21_pow(S21_INF, S21_INF));
}
END_TEST

START_TEST(ceil_tests) {
  ck_assert_ldouble_eq_tol(ceil(-13.54), s21_ceil(-13.54), 1e-6);
  ck_assert_ldouble_eq_tol(ceil(13.54), s21_ceil(13.54), 1e-6);
  ck_assert_ldouble_eq_tol(ceil(1000), s21_ceil(1000), 1e-6);
  ck_assert_ldouble_eq_tol(ceil(102.0000002), s21_ceil(102.0000002), 1e-6);
  ck_assert_ldouble_eq_tol(ceil(0.0), s21_ceil(0.0), 1e-6);
  ck_assert_ldouble_eq_tol(ceil(0.5), s21_ceil(0.5), 1e-6);
  ck_assert_ldouble_eq_tol(ceil(-0.5), s21_ceil(-0.5), 1e-6);
  ck_assert_ldouble_eq_tol(ceil(1.0), s21_ceil(1.0), 1e-6);
  ck_assert_ldouble_eq_tol(ceil(-1.0), s21_ceil(-1.0), 1e-6);
  ck_assert_ldouble_eq_tol(ceil(1000), s21_ceil(1000), 1e-6);
  ck_assert_ldouble_eq_tol(ceil(-1000), s21_ceil(-1000), 1e-6);
  ck_assert_ldouble_eq_tol(ceil(1001), s21_ceil(1001), 1e-6);
  ck_assert_ldouble_eq_tol(ceil(-1001), s21_ceil(-1001), 1e-6);
  ck_assert_ldouble_eq(ceil(S21_INF), s21_ceil(S21_INF));
  ck_assert_ldouble_eq(ceil(S21_DBL_MAX), s21_ceil(S21_DBL_MAX));
}
END_TEST

START_TEST(floor_tests) {
  ck_assert_ldouble_eq_tol(floor(-13.54), s21_floor(-13.54), 1e-6);
  ck_assert_ldouble_eq_tol(floor(13.54), s21_floor(13.54), 1e-6);
  ck_assert_ldouble_eq_tol(floor(-0.54), s21_floor(-0.54), 1e-6);
  ck_assert_ldouble_eq_tol(floor(1000), s21_floor(1000), 1e-6);
  ck_assert_ldouble_eq_tol(floor(102.0000002), s21_floor(102.0000002), 1e-6);
  ck_assert_ldouble_eq_tol(floor(0.0), s21_floor(0.0), 1e-6);
  ck_assert_ldouble_eq_tol(floor(0.5), s21_floor(0.5), 1e-6);
  ck_assert_ldouble_eq_tol(floor(-0.5), s21_floor(-0.5), 1e-6);
  ck_assert_ldouble_eq_tol(floor(1.0), s21_floor(1.0), 1e-6);
  ck_assert_ldouble_eq_tol(floor(-1.0), s21_floor(-1.0), 1e-6);
  ck_assert_ldouble_eq_tol(floor(-1000), s21_floor(-1000), 1e-6);
  ck_assert_ldouble_eq_tol(floor(1001), s21_floor(1001), 1e-6);
  ck_assert_ldouble_eq_tol(floor(-1001), s21_floor(-1001), 1e-6);
  ck_assert_ldouble_eq(floor(S21_INF), s21_floor(S21_INF));
  ck_assert_ldouble_eq(floor(S21_DBL_MAX), s21_floor(S21_DBL_MAX));
}
END_TEST

START_TEST(exp_tests) {
  ck_assert_ldouble_eq_tol(exp(-13.54), s21_exp(-13.54), 1e-6);
  ck_assert_ldouble_eq_tol(exp(13.54), s21_exp(13.54), 1e-6);
  ck_assert_ldouble_eq_tol(exp(-0.54), s21_exp(-0.54), 1e-6);
  ck_assert_ldouble_eq(exp(1000), s21_exp(1000));
  ck_assert_ldouble_eq_tol(exp(1), s21_exp(1), 1e-6);
  ck_assert_ldouble_eq_tol(exp(2), s21_exp(2), 1e-6);
  ck_assert_ldouble_eq_tol(exp(-1), s21_exp(-1), 1e-6);
  ck_assert_ldouble_eq_tol(exp(0), s21_exp(0), 1e-6);
  ck_assert_int_eq(exp(S21_NAN), s21_exp(S21_NAN));
  ck_assert_ldouble_eq_tol(exp(1.2345), s21_exp(1.2345), 1e-6);
  ck_assert_ldouble_eq_tol(exp(1.23454656768), s21_exp(1.23454656768), 1e-6);
  ck_assert_float_eq(exp(DBL_MAX), s21_exp(DBL_MAX));
  ck_assert_float_eq(exp(102.0000002), s21_exp(102.0000002));
  ck_assert_ldouble_eq_tol(exp(0.5), s21_exp(0.5), 1e-6);
  ck_assert_ldouble_eq_tol(exp(-0.5), s21_exp(-0.5), 1e-6);
  ck_assert_ldouble_eq_tol(exp(0.9), s21_exp(0.9), 1e-6);
  ck_assert_ldouble_eq_tol(exp(-0.9), s21_exp(-0.9), 1e-6);
  ck_assert_ldouble_eq_tol(exp(1.0), s21_exp(1.0), 1e-6);
  ck_assert_ldouble_eq_tol(exp(-1.0), s21_exp(-1.0), 1e-6);
  ck_assert_ldouble_eq_tol(exp(3.14), s21_exp(3.14), 1e-6);
  ck_assert_ldouble_eq_tol(exp(-3.14), s21_exp(-3.14), 1e-6);
  ck_assert_ldouble_eq_tol(exp(6.28), s21_exp(6.28), 1e-6);
  ck_assert_ldouble_eq_tol(exp(2.2343), s21_exp(2.2343), 1e-6);
  ck_assert_ldouble_eq_tol(exp(0.1234), s21_exp(0.1234), 1e-6);
  ck_assert_ldouble_eq_tol(exp(3.876543), s21_exp(3.876543), 1e-6);
  ck_assert_ldouble_eq_tol(exp(21.123456), s21_exp(21.123456), 1e-6);
  ck_assert_ldouble_eq_tol(exp(-21.123456), s21_exp(-21.123456), 1e-6);
  ck_assert_float_eq(exp(25), s21_exp(25));
  ck_assert_ldouble_eq_tol(exp(-25), s21_exp(-25), 1e-6);
  ck_assert_float_eq(exp(123), s21_exp(123));
  ck_assert_ldouble_eq_tol(exp(-123), s21_exp(-123), 1e-6);
  ck_assert_ldouble_eq(exp(S21_INF), s21_exp(S21_INF));
}
END_TEST

START_TEST(sin_tests) {
  ck_assert_ldouble_eq_tol(sin(-13.54), s21_sin(-13.54), 1e-6);
  ck_assert_ldouble_eq_tol(sin(13.54), s21_sin(13.54), 1e-6);
  ck_assert_ldouble_eq_tol(sin(0.0), s21_sin(0.0), 1e-6);
  ck_assert_ldouble_eq_tol(sin(1.0), s21_sin(1.0), 1e-6);
  ck_assert_ldouble_eq_tol(sin(3.14), s21_sin(3.14), 1e-6);
  ck_assert_ldouble_eq_tol(sin(6.28), s21_sin(6.28), 1e-6);
  ck_assert_ldouble_eq_tol(sin(2.2343), s21_sin(2.2343), 1e-6);
  ck_assert_ldouble_eq_tol(sin(0.1234), s21_sin(0.1234), 1e-6);
  ck_assert_ldouble_eq_tol(sin(0.5), s21_sin(0.5), 1e-6);
  ck_assert_ldouble_eq_tol(sin(-0.5), s21_sin(-0.5), 1e-6);
  ck_assert_ldouble_eq_tol(sin(1.0), s21_sin(1.0), 1e-6);
  ck_assert_ldouble_eq_tol(sin(-1.0), s21_sin(-1.0), 1e-6);
  ck_assert_ldouble_eq_tol(sin(3.14), s21_sin(3.14), 1e-6);
  ck_assert_ldouble_eq_tol(sin(6.28), s21_sin(6.28), 1e-6);
  ck_assert_ldouble_eq_tol(sin(2.2343), s21_sin(2.2343), 1e-6);
  ck_assert_ldouble_eq_tol(sin(0.1234), s21_sin(0.1234), 1e-6);
  ck_assert_ldouble_eq_tol(sin(3.876543), s21_sin(3.876543), 1e-6);
  ck_assert_ldouble_eq_tol(sin(21.123456), s21_sin(21.123456), 1e-6);
  ck_assert_ldouble_eq_tol(sin(-21.123456), s21_sin(-21.123456), 1e-6);
  ck_assert_int_eq(sin(S21_NAN), s21_sin(S21_NAN));
  ck_assert_int_eq(sin(S21_INF), s21_sin(S21_INF));
}
END_TEST

START_TEST(cos_tests) {
  ck_assert_ldouble_eq_tol(cos(-13.54), s21_cos(-13.54), 1e-6);
  ck_assert_ldouble_eq_tol(cos(13.54), s21_cos(13.54), 1e-6);
  ck_assert_ldouble_eq_tol(cos(0.0), s21_cos(0.0), 1e-6);
  ck_assert_ldouble_eq_tol(cos(1.0), s21_cos(1.0), 1e-6);
  ck_assert_ldouble_eq_tol(cos(3.14), s21_cos(3.14), 1e-6);
  ck_assert_ldouble_eq_tol(cos(6.28), s21_cos(6.28), 1e-6);
  ck_assert_ldouble_eq_tol(cos(2.2343), s21_cos(2.2343), 1e-6);
  ck_assert_ldouble_eq_tol(cos(0.1234), s21_cos(0.1234), 1e-6);
  ck_assert_ldouble_eq_tol(cos(0.5), s21_cos(0.5), 1e-6);
  ck_assert_ldouble_eq_tol(cos(-0.5), s21_cos(-0.5), 1e-6);
  ck_assert_ldouble_eq_tol(cos(-1.0), s21_cos(-1.0), 1e-6);
  ck_assert_ldouble_eq_tol(cos(3.876543), s21_cos(3.876543), 1e-6);
  ck_assert_ldouble_eq_tol(cos(21.123456), s21_cos(21.123456), 1e-6);
  ck_assert_ldouble_eq_tol(cos(-21.123456), s21_cos(-21.123456), 1e-6);
  ck_assert_int_eq(cos(S21_NAN), s21_cos(S21_NAN));
  ck_assert_int_eq(cos(S21_INF), s21_cos(S21_INF));
}
END_TEST

START_TEST(tan_tests) {
  ck_assert_ldouble_eq_tol(tan(-13.54), s21_tan(-13.54), 1e-6);
  ck_assert_ldouble_eq_tol(tan(13.54), s21_tan(13.54), 1e-6);
  ck_assert_ldouble_eq_tol(tan(1), s21_tan(1), 1e-6);
  ck_assert_ldouble_eq_tol(tan(0.0), s21_tan(0.0), 1e-6);
  ck_assert_ldouble_eq_tol(tan(0.5), s21_tan(0.5), 1e-6);
  ck_assert_ldouble_eq_tol(tan(-0.5), s21_tan(-0.5), 1e-6);
  ck_assert_ldouble_eq_tol(tan(1.0), s21_tan(1.0), 1e-6);
  ck_assert_ldouble_eq_tol(tan(-1.0), s21_tan(-1.0), 1e-6);
  ck_assert_ldouble_eq_tol(tan(3.14), s21_tan(3.14), 1e-6);
  ck_assert_ldouble_eq_tol(tan(6.28), s21_tan(6.28), 1e-6);
  ck_assert_ldouble_eq_tol(tan(2.2343), s21_tan(2.2343), 1e-6);
  ck_assert_ldouble_eq_tol(tan(0.1234), s21_tan(0.1234), 1e-6);
  ck_assert_ldouble_eq_tol(tan(3.876543), s21_tan(3.876543), 1e-6);
  ck_assert_ldouble_eq_tol(tan(21.123456), s21_tan(21.123456), 1e-6);
  ck_assert_ldouble_eq_tol(tan(-21.123456), s21_tan(-21.123456), 1e-6);
  ck_assert_int_eq(tan(S21_NAN), s21_tan(S21_NAN));
  ck_assert_int_eq(tan(S21_INF), s21_tan(S21_INF));
  ck_assert_int_eq(tan(S21_mINF), s21_tan(S21_mINF));
}
END_TEST

START_TEST(sqrt_tests) {
  ck_assert_ldouble_eq_tol(sqrt(0.0), s21_sqrt(0.0), 1e-6);
  ck_assert_ldouble_eq_tol(sqrt(1.0), s21_sqrt(1.0), 1e-6);
  ck_assert_ldouble_eq_tol(sqrt(6.28), s21_sqrt(6.28), 1e-6);
  ck_assert_ldouble_eq_tol(sqrt(2.2343), s21_sqrt(2.2343), 1e-6);
  ck_assert_int_eq(sqrt(-123), s21_sqrt(-123));
  ck_assert_ldouble_eq_tol(sqrt(0.9), s21_sqrt(0.9), 1e-6);
  ck_assert_ldouble_eq_tol(sqrt(0.5), s21_sqrt(0.5), 1e-6);
  ck_assert_int_eq(sqrt(-0.5), s21_sqrt(-0.5));
  ck_assert_ldouble_eq_tol(sqrt(0.9), s21_sqrt(0.9), 1e-6);
  ck_assert_int_eq(sqrt(-0.9), s21_sqrt(-0.9));
  ck_assert_ldouble_eq_tol(sqrt(1.0), s21_sqrt(1.0), 1e-6);
  ck_assert_int_eq(sqrt(-1.0), s21_sqrt(-1.0));
  ck_assert_ldouble_eq_tol(sqrt(3.14), s21_sqrt(3.14), 1e-6);
  ck_assert_int_eq(sqrt(-3.14), s21_sqrt(-3.14));
  ck_assert_ldouble_eq_tol(sqrt(0.1234), s21_sqrt(0.1234), 1e-6);
  ck_assert_ldouble_eq_tol(sqrt(3.876543), s21_sqrt(3.876543), 1e-6);
  ck_assert_ldouble_eq_tol(sqrt(21.123456), s21_sqrt(21.123456), 1e-6);
  ck_assert_int_eq(sqrt(-21.123456), s21_sqrt(-21.123456));
  ck_assert_ldouble_eq_tol(sqrt(25), s21_sqrt(25), 1e-6);
  ck_assert_int_eq(sqrt(-25), s21_sqrt(-25));
  ck_assert_ldouble_eq_tol(sqrt(123), s21_sqrt(123), 1e-6);
  ck_assert_int_eq(sqrt(S21_NAN), s21_sqrt(S21_NAN));
  ck_assert_ldouble_eq(sqrt(S21_INF), s21_sqrt(S21_INF));
}

START_TEST(log_tests) {
  ck_assert_int_eq(log(-13.54), s21_log(-13.54));
  ck_assert_ldouble_eq_tol(log(13.54), s21_log(13.54), 1e-6);
  ck_assert_ldouble_eq(log(0.0), s21_log(0.0));
  ck_assert_ldouble_eq_tol(log(1.0), s21_log(1.0), 1e-6);
  ck_assert_ldouble_eq_tol(log(3.14), s21_log(3.14), 1e-6);
  ck_assert_ldouble_eq_tol(log(6.28), s21_log(6.28), 1e-6);
  ck_assert_ldouble_eq_tol(log(2.2343), s21_log(2.2343), 1e-6);
  ck_assert_ldouble_eq_tol(log(0.1234), s21_log(0.1234), 1e-6);
  ck_assert_ldouble_eq_tol(log(0.5), s21_log(0.5), 1e-6);
  ck_assert_int_eq(log(-0.5), s21_log(-0.5));
  ck_assert_int_eq(log(-1.0), s21_log(-1.0));
  ck_assert_int_eq(log(-3.14), s21_log(-3.14));
  ck_assert_ldouble_eq_tol(log(3.876543), s21_log(3.876543), 1e-6);
  ck_assert_ldouble_eq_tol(log(21.123456), s21_log(21.123456), 1e-6);
  ck_assert_int_eq(log(-21.123456), s21_log(-21.123456));
  ck_assert_int_eq(log(S21_NAN), s21_log(S21_NAN));
  ck_assert_ldouble_eq(log(S21_INF), s21_log(S21_INF));
}
END_TEST

START_TEST(asin_tests) {
  ck_assert_int_eq(asin(-13.54), s21_asin(-13.54));
  ck_assert_int_eq(asin(13.54), s21_asin(13.54));
  ck_assert_int_eq(asin(6.28), s21_asin(6.28));
  ck_assert_ldouble_eq_tol(asin(S21_EPS), s21_asin(S21_EPS), 1e-6);
  ck_assert_ldouble_eq_tol(asin(0.1234), s21_asin(0.1234), 1e-6);
  ck_assert_ldouble_eq_tol(asin(1), s21_asin(1), 1e-6);
  ck_assert_ldouble_eq_tol(asin(-1), s21_asin(-1), 1e-6);
  ck_assert_ldouble_eq_tol(asin(0.5), s21_asin(0.5), 1e-6);
  ck_assert_ldouble_eq_tol(asin(-0.5), s21_asin(-0.5), 1e-6);
  ck_assert_ldouble_eq_tol(asin(0.0), s21_asin(0.0), 1e-6);
  ck_assert_ldouble_eq_tol(asin(1.0), s21_asin(1.0), 1e-6);
  ck_assert_ldouble_eq_tol(asin(-1.0), s21_asin(-1.0), 1e-6);
  ck_assert_int_eq(asin(3.14), s21_asin(3.14));
  ck_assert_int_eq(asin(-3.14), s21_asin(-3.14));
  ck_assert_int_eq(asin(S21_NAN), s21_asin(S21_NAN));
  ck_assert_int_eq(asin(S21_INF), s21_asin(S21_INF));
  ck_assert_int_eq(asin(DBL_MAX), s21_asin(DBL_MAX));
  ck_assert_ldouble_eq_tol(asin(DBL_MIN), s21_asin(DBL_MIN), 1e-6);
}
END_TEST

START_TEST(acos_tests) {
  ck_assert_int_eq(acos(-13.54), s21_acos(-13.54));
  ck_assert_int_eq(acos(13.54), s21_acos(13.54));
  ck_assert_int_eq(acos(2.2343), s21_acos(2.2343));
  ck_assert_ldouble_eq_tol(acos(0.1234), s21_acos(0.1234), 1e-6);
  ck_assert_ldouble_eq_tol(acos(0.0), s21_acos(0.0), 1e-6);
  ck_assert_ldouble_eq_tol(acos(0.5), s21_acos(0.5), 1e-6);
  ck_assert_ldouble_eq_tol(acos(-0.5), s21_acos(-0.5), 1e-6);
  ck_assert_ldouble_eq_tol(acos(1.0), s21_acos(1.0), 1e-6);
  ck_assert_ldouble_eq_tol(acos(-1.0), s21_acos(-1.0), 1e-6);
  ck_assert_int_eq(acos(3.14), s21_acos(3.14));
  ck_assert_int_eq(acos(-3.14), s21_acos(-3.14));
  ck_assert_int_eq(acos(S21_NAN), s21_acos(S21_NAN));
  ck_assert_int_eq(acos(S21_INF), s21_acos(S21_INF));
}
END_TEST

START_TEST(atan_tests) {
  ck_assert_ldouble_eq_tol(atan(-13.54), s21_atan(-13.54), 1e-6);
  ck_assert_ldouble_eq_tol(atan(13.54), s21_atan(13.54), 1e-6);
  ck_assert_ldouble_eq_tol(atan(3.14), s21_atan(3.14), 1e-6);
  ck_assert_ldouble_eq_tol(atan(6.28), s21_atan(6.28), 1e-6);
  ck_assert_ldouble_eq_tol(atan(2.2343), s21_atan(2.2343), 1e-6);
  ck_assert_ldouble_eq_tol(atan(0.1234), s21_atan(0.1234), 1e-6);
  ck_assert_ldouble_eq_tol(atan(0), s21_atan(0), 1e-6);
  ck_assert_ldouble_eq_tol(atan(1), s21_atan(1), 1e-6);
  ck_assert_ldouble_eq_tol(atan(3.5), s21_atan(3.5), 1e-6);
  ck_assert_ldouble_eq_tol(atan(-3.5), s21_atan(-3.5), 1e-6);
  ck_assert_ldouble_eq_tol(atan(0.0), s21_atan(0.0), 1e-6);
  ck_assert_ldouble_eq_tol(atan(0.5), s21_atan(0.5), 1e-6);
  ck_assert_ldouble_eq_tol(atan(-0.5), s21_atan(-0.5), 1e-6);
  ck_assert_ldouble_eq_tol(atan(1.0), s21_atan(1.0), 1e-6);
  ck_assert_ldouble_eq_tol(atan(-1.0), s21_atan(-1.0), 1e-6);
  ck_assert_ldouble_eq_tol(atan(-3.14), s21_atan(-3.14), 1e-6);
  ck_assert_ldouble_eq_tol(atan(3.876543), s21_atan(3.876543), 1e-6);
  ck_assert_ldouble_eq_tol(atan(21.123456), s21_atan(21.123456), 1e-6);
  ck_assert_ldouble_eq_tol(atan(-21.123456), s21_atan(-21.123456), 1e-6);
  ck_assert_int_eq(atan(S21_NAN), s21_atan(S21_NAN));
  ck_assert_ldouble_eq_tol(atan(S21_INF), s21_atan(S21_INF), 1e-6);
}
END_TEST

Suite *s21_math_tests_create() {
  Suite *s21_math = suite_create("s21_math");
  TCase *s21_math_tests = tcase_create("S21_MATH");
  tcase_set_timeout(s21_math_tests, 20);
  tcase_add_test(s21_math_tests, ceil_tests);
  tcase_add_test(s21_math_tests, floor_tests);
  tcase_add_test(s21_math_tests, exp_tests);
  tcase_add_test(s21_math_tests, sin_tests);
  tcase_add_test(s21_math_tests, cos_tests);
  tcase_add_test(s21_math_tests, tan_tests);
  tcase_add_test(s21_math_tests, abs_tests);
  tcase_add_test(s21_math_tests, fabs_tests);
  tcase_add_test(s21_math_tests, fmod_tests);
  tcase_add_test(s21_math_tests, pow_tests);
  tcase_add_test(s21_math_tests, sqrt_tests);
  tcase_add_test(s21_math_tests, log_tests);
  tcase_add_test(s21_math_tests, asin_tests);
  tcase_add_test(s21_math_tests, acos_tests);
  tcase_add_test(s21_math_tests, atan_tests);
  suite_add_tcase(s21_math, s21_math_tests);
  return s21_math;
}

int main() {
  Suite *s21_math = s21_math_tests_create();
  SRunner *s21_math_runner = srunner_create(s21_math);
  int number_failed;
  srunner_run_all(s21_math_runner, CK_NORMAL);
  number_failed = srunner_ntests_failed(s21_math_runner);
  srunner_free(s21_math_runner);

  return number_failed == 0 ? 0 : 1;
}
