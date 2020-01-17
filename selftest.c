/**
 * Copyright (c) 2020, Artem V. Andreev
 *
 * SPDX-License-Identifier: MIT
 */

#include "cqc.h"

CQC_TESTCASE(test_simple,
             "A simple test", CQC_NO_CLASSES,
             cqc_expect(assert(true)));

CQC_TESTCASE(test_boolean,
             "A boolean test", CQC_NO_CLASSES,
             cqc_forall(bool, b, cqc_expect(assert(b || !b))));

CQC_TESTCASE(test_boolean_class,
             "A boolean test with classes", CQC_BOOL_CLASSES,
             cqc_forall(bool, b,
                        cqc_classify(b ? 1 : 0,
                                     cqc_expect(assert(b || !b)))));

CQC_TESTCASE_SINGLE(test_expect_fail,
             "A failing test",
             cqc_expect_fail(assert(false)));

CQC_TESTCASE_SINGLE(test_expect_timeout,
                    "A timed out test",
                    cqc_expect_timeout(1, sleep(10)));

CQC_TESTCASE(test_oneof,
             "Select one of", CQC_NO_CLASSES,
             cqc_forall_oneof(int, x, cqc_list(1, 2, 3, 4),
                              cqc_expect(assert(true))));

CQC_TESTCASE(test_scaled_int,
             "Generating an integer up to the scale", CQC_NO_CLASSES,
             cqc_forall_range(int, scale, 1, 63,
                              cqc_forall_scaled
                              (scale, uint64_t, v,
                               cqc_expect(assert(v < (1ull << scale))))));

CQC_TESTCASE(test_scaled_double,
             "Generate scaled floating-point", CQC_NO_CLASSES,
             cqc_forall_scaled(35, double, v,
                               cqc_expect(isfinite(v))));

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
                           cqc_expect(assert(true))));
