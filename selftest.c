/**
 * Copyright (c) 2020, Artem V. Andreev
 *
 * SPDX-License-Identifier: MIT
 */

#include <float.h>
#include "cqc.h"

CQC_TESTCASE(test_simple, "A simple test")
{
    cqc_expect
    {
        assert(true);
    }
}

CQC_TESTCASE(test_boolean, "A boolean test")
{
    cqc_forall(bool, b)
    {
        cqc_expect
        {
            assert(b || !b);
        }
    }
}

CQC_TESTCASE(test_boolean_class, "A boolean test with classes")
{
    cqc_forall(bool, b)
    {
        cqc_classify(bc, bc = b ? 0 : 1, "true", "false")
        {
            cqc_expect
            {
                assert(b || !b);
            }
        }
    }
}

CQC_TESTCASE(test_expect_fail, "A failing test")
{
    cqc_once
    {
        cqc_expect_fail
        {
            cqc_assert_eq(int, 1, 2);
        }
    }
}

CQC_TESTCASE(test_expect_timeout, "A timed out test")
{
    cqc_once
    {
        cqc_expect_timeout(1)
        {
            sleep(10);
        }
    }
}

CQC_TESTCASE(test_oneof, "Select one of")
{
    cqc_forall_oneof(int, x, 1, 2, 3, 4)
    {
        cqc_expect
        {
            assert(x >= 1 && x <= 4);
        }
    }
}

CQC_TESTCASE(test_scaled_int,
             "Generating an integer up to the scale")
{
    cqc_forall_range(int, scale, 1, sizeof(uint64_t) * CHAR_BIT - 1)
    {
        cqc_forall_scaled(uint64_t, v, scale)
        {
            cqc_expect
            {
                assert(v < (1ull << scale));
            }
        }
    }
}

CQC_TESTCASE(test_scaled_double,
             "Generate scaled floating-point")
{
    cqc_forall_scaled(double, v, DBL_MAX_10_EXP)
    {
        cqc_expect
        {
            assert(isfinite(v));
        }
    }
}



CQC_TESTCASE(test_double_class,
             "Generate double with special values")
{
    cqc_forall_alt(double, v, 50, NAN, INFINITY, -INFINITY, 0.0, DBL_MIN / 2)
    {
        cqc_classify(fpc,
                     switch (fpclassify(v))
                     {
                         case FP_NAN:
                             fpc = 0;
                             break;
                         case FP_INFINITE:
                             fpc = 1;
                             break;
                         case FP_ZERO:
                             fpc = 2;
                             break;
                         case FP_SUBNORMAL:
                             fpc = 3;
                             break;
                         case FP_NORMAL:
                             fpc = 4;
                             break;
                         default:
                             assert(0);
                     },
                     "NaN", "infinite", "zero", "subnormal", "normal")
        {
            cqc_expect
            {
                assert(true);
            }
        }
    }
}

CQC_TESTCASE(test_char_class,
             "Generate ASCII character and classify them")
{
    cqc_forall(char, ch)
    {
        cqc_classify(cc,
                     if (isalpha(ch))
                         cc = 0;
                     else if (isdigit(ch))
                         cc = 1;
                     else if (ispunct(ch))
                         cc = 2;
                     else
                         cc = 3,
                     "alpha", "digit", "punct", "nongraph")
        {
            cqc_expect
            {
                assert(true);
            }
        }
    }
}

CQC_TESTCASE(test_string,
             "Generate a random string")
{
    cqc_forall_alt(cqc_string, str, 10, "")
    {
        cqc_classify(sc, sc = *str == '\0' ? 0 : 1,
                     "empty", "non-empty")
        {
            cqc_expect
            {
                cqc_assert_eq(cqc_string, str, str);
            }
        }
    }
}

CQC_TESTCASE(test_condition, "Generate an even integer")
{
    cqc_forall(int, v)
    {
        cqc_condition(v % 2 == 0)
        {
            cqc_expect
            {
                assert(v % 2 == 0);
            }
        }
    }
}

CQC_TESTCASE(test_pair, "Generate a pair of integers")
{
    cqc_forall_pair(int, v1, v2)
    {
        cqc_classify(oc, oc = v1 < v2,
                     "less", "not less")
        {
            cqc_expect
            {
                assert(v1 < v2 || v1 >= v2);
            }
        }
    }
}
