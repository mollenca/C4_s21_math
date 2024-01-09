#include "s21_math.h"

int s21_abs(int x) {
  if (x < 0) x *= -1;
  return x;
}

long double s21_acos(double x) {
  long double res = S21_NAN;
  if (x > 0 && x < 1)
    res = s21_atan(s21_sqrt(1 - x * x) / x);
  else if (x > -1 && x < 0)
    res = S21_PI + s21_atan(s21_sqrt(1 - x * x) / x);
  else if (x == 0)
    res = S21_PI / 2;
  else if (x == 1.0)
    res = 0.0;
  else if (x == -1)
    res = S21_PI;
  else if (x > 1 || x < -1) {
    res = S21_NAN;
    res = -res;
  } else if (x == S21_NAN || x == S21_INF || x == S21_mINF)
    res = S21_NAN;
  return res;
}

long double s21_asin(double x) {
  long double res = S21_NAN;
  if (x == S21_NAN || x == S21_INF || x == S21_mINF) {
    res = S21_NAN;
    res = -res;
  } else if (x == 1)
    res = S21_PI / 2.0;
  else if (x == -1)
    res = -1.0 * S21_PI / 2.0;
  else if (x > 1 || x < -1) {
    res = S21_NAN;
    res = -res;
  } else if (x > -1 && x < 1)
    res = s21_atan(x / (s21_sqrt(1 - x * x)));
  return res;
}

long double s21_atan(double x) {
  long double res = 0;
  if (x != x)
    res = x;
  else if (x == 0)
    res = x;
  else if (x == S21_mINF)
    res = -S21_PI / 2;
  else if (x == S21_INF)
    res = S21_PI / 2;
  else if (x == 1)
    res = S21_PI / 4;
  else if (x == -1)
    res = -S21_PI / 4;
  else if (s21_fabs(x) < 1) {
    for (long int i = 0; i < 300; i++) {
      res += (s21_pow(-1, i) * s21_pow(x, 1 + 2 * i)) / (1 + 2 * i);
    }
  } else if (s21_fabs(x) > 1) {
    for (long int i = 0; i < 301; i++)
      res += (s21_pow(-1, i) * s21_pow(x, -1 - 2 * i)) / (1 + 2 * i);
    res = ((S21_PI * s21_sqrt(x * x)) / (2 * x)) - res;
  }
  return res;
}

long double s21_ceil(double x) {
  long double res = (long long int)x;
  if (x == S21_INF || x == S21_mINF || x == S21_NAN || x == S21_DBL_MAX)
    res = x;
  else {
    if (s21_fabs(x) > 0. && x != res) {
      if (x > 0.) res += 1.;
    }
  }
  return res;
}

long double s21_cos(double x) {
  long double res = 0;
  if (x == S21_NAN) {
    res = S21_NAN;
  } else if (x == 0.) {
    res = 1.;
  } else if (x == S21_INF || x == S21_mINF) {
    res = -S21_NAN;
  } else {
    for (; x < -2 * S21_PI || x > 2 * S21_PI;) {
      if (x > 2 * S21_PI) {
        x -= 2 * S21_PI;
      } else {
        x += 2 * S21_PI;
      }
    }
    res = s21_sin(x + S21_PI / 2.);
  }
  return res;
}

long double s21_exp(double x) {
  long double res = x;
  if (x == 0) {
    res = 1;
  } else if (x == S21_NAN) {
    res = -S21_NAN;
  } else if (x <= 0) {
    res = 1 / s21_exp(-x);
  } else if (s21_fabs(x) < S21_INF) {
    res = 1;
    long int count = 1;
    long double tmp = 1;
    while (tmp > S21_EPS && res != S21_INF) {
      tmp *= x / count;
      count++;
      res += tmp;
      if (res > S21_DBL_MAX) {
        res = S21_INF;
      }
    }
  }
  return res;
}

long double s21_fabs(double x) {
  long double res = x;
  if (x == S21_INF || x == S21_mINF) {
    res = S21_INF;
  } else {
    res *= (x < 0) ? -1 : 1;
  }
  return res;
}

long double s21_floor(double x) {
  long double res = (long long int)x;
  if (x == S21_INF || x == S21_mINF || x == S21_NAN || x == 0. ||
      x == S21_DBL_MAX) {
    res = x;
  } else {
    if (s21_fabs(x) > 0. && x != res) {
      if (x < 0.) res -= 1.;
    }
  }
  return res;
}

long double s21_fmod(double x, double y) {
  long long int mod = 0;
  long double res = 0, copy_x = (long double)x, cope_y = (long double)y;
  mod = x / y;
  if (y == S21_INF || y == S21_mINF || x == S21_INF || x == S21_mINF) {
    if (x == S21_INF || x == S21_mINF) {
      res = -S21_NAN;
    } else if (y == S21_INF || y == S21_mINF) {
      res = x;
    }
  } else if (x == S21_NAN || y == S21_NAN) {
    res = S21_NAN;
  } else {
    res = copy_x - mod * cope_y;
  }
  return res;
}

long double s21_log(double x) {
  int exp_count = 0;
  long double res = 0.0;
  double duplicate = 0.0;
  if (x == -S21_INF || x < 0. || x == S21_NAN) {
    if (x < 0.) {
      res = -S21_NAN;
    } else {
      res = S21_NAN;
    }
  } else if (x == 0.0) {
    res = -S21_INF;
  } else if (x == S21_INF) {
    res = S21_INF;
  } else if (x == 1.0) {
    res = 0;
  } else {
    for (; x >= S21_EXP; x /= S21_EXP, exp_count++) continue;
    for (int i = 0; i < 100; i++) {
      duplicate = res;
      res = duplicate + 2 * (x - s21_exp(duplicate)) / (x + s21_exp(duplicate));
    }
    res += exp_count;
  }
  return res;
}

long double s21_pow(double base, double exp) {
  long double res = 0;
  double copy_base = base;
  if (copy_base == 0 && exp > 0) {
    res = 0;
  } else if (copy_base == 0 && exp < 0) {
    res = S21_INF;
  } else if ((copy_base == 0 || copy_base != copy_base ||
              copy_base == S21_INF || copy_base == S21_mINF) &&
             exp == 0) {
    res = 1;
  } else if (copy_base < 0) {
    copy_base = -copy_base;
    res = s21_exp(exp * s21_log(copy_base));
    if (s21_fmod(exp, 2) != 0 && (exp != S21_mINF && exp != S21_INF) &&
        (copy_base != 1 || s21_fmod(exp, 2) != 0)) {
      res = -res;
    }
  } else {
    if ((copy_base != copy_base || exp == S21_mINF || exp == S21_INF) &&
        (copy_base == 1)) {
      res = 1;
    } else {
      res = s21_exp(exp * s21_log(base));
    }
  }
  return res;
}

long double s21_sin(double x) {
  long double res = 0, copy_x = x;
  copy_x = s21_fmod(x, 2 * S21_PI);
  for (int i = 0, sign = 1; i < 20; i++) {
    res += sign * s21_pow(copy_x, (2 * i + 1)) / s21_factorial(1 + 2 * i);
    sign *= -1;
  }
  return res;
}

long double s21_sqrt(double x) {
  long double first_value = 0, second_value, middle_value;
  if (x == S21_INF || x == S21_mINF) {
    middle_value = S21_INF;
  } else if (x == S21_NAN || x < 0) {
    if (x < 0) {
      middle_value = -S21_NAN;
    } else {
      middle_value = S21_NAN;
    }
  } else {
    if (x == 0) {
      middle_value = 0.0;
    } else {
      if (x < 1)
        second_value = 1;
      else
        second_value = x;
      middle_value = (first_value + second_value) / 2;
      while ((middle_value - first_value) > S21_EPS) {
        if (middle_value * middle_value > x)
          second_value = middle_value;
        else
          first_value = middle_value;
        middle_value = (first_value + second_value) / 2;
      }
    }
  }
  return middle_value;
}

long double s21_tan(double x) {
  long double res = x;
  if (x == 0) {
    res = x;
  } else if (x == S21_NAN || x == S21_INF || x == S21_mINF) {
    res = -S21_NAN;
  } else if (s21_fabs(x) < S21_INF) {
    res = s21_sin(x) / s21_cos(x);
  }
  return res;
}

long double s21_factorial(int x) {
  long double res = 0;
  if (x == 0) {
    res = 1;
  }
  if (x > 0) {
    res = x * s21_factorial(x - 1);
  }
  return res;
}
