/**
 * Copyright (c) 2020, Artem V. Andreev
 *
 * SPDX-License-Identifier: MIT
 */

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
    cqc_forall_range(int, scale, 1, 63)
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
    cqc_forall_scaled(double, v, 35)
    {
        cqc_expect
        {
            assert(isfinite(v));
        }
    }
}

/*
CQC_TESTCASE(test_double_class,
             "Generate double with special values",
             CQC_FP_CLASSES,
             cqc_forall_cc(50, double, v,
                           cqc_classify(cqc_fp_class(v),
                                        cqc_expect(assert(true)))));

CQC_TESTCASE(test_char_class,
             "Generate ASCII character and classify them",
             CQC_CHAR_CLASSES,
             cqc_forall(char, ch,
                        cqc_classify(cqc_char_class(ch),
                                     cqc_expect(assert(true)))));

CQC_TESTCASE(test_string,
             "Generate a random string",
             CQC_NO_CLASSES,
             cqc_forall_cc(10, cqc_string, str,
                           cqc_expect(cqc_assert_eq(cqc_string, str, str))));

CQC_TESTCASE(test_condition,
             "Generate an even integer",
             CQC_NO_CLASSES,
             cqc_forall(int, v,
                        cqc_condition(v % 2 == 0,
                                      cqc_expect(assert(v % 2 == 0)))));
*/
