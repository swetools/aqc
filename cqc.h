/**
 * Copyright (c) 2020, Artem V. Andreev
 *
 * SPDX-License-Identifier: MIT
 */


#ifndef CQC_H
#define CQC_H 1

#ifdef __cplusplus
extern "C"
{
#endif

#include <stdbool.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdlib.h>
#include <getopt.h>
#include <stdio.h>
#include <assert.h>
#include <signal.h>
#include <inttypes.h>
#include <limits.h>
#include <math.h>
#include <float.h>
#include <ctype.h>
#include <string.h>

typedef char *cqc_string;
typedef const void *cqc_opaque;

typedef enum cqc_result {
    CQC_RESULT_FORCE_COMPLETE,
    CQC_RESULT_COMPLETE,
    CQC_RESULT_PENDING,
    CQC_RESULT_SKIPPED,
    CQC_RESULT_FAILED,
    CQC_RESULT_CRASHED,
} cqc_result;

typedef void cqc_testing_func(cqc_result *);

typedef struct cqc_testcase_list {
    const char *id;
    const char *name;
    cqc_testing_func *test;
    struct cqc_testcase_list *next;
} cqc_testcase_list;

static cqc_testcase_list *cqc_first_testcase;

__attribute__ ((unused))
static cqc_testcase_list **cqc_last_testcase = &cqc_first_testcase;

#define CQC_ARRAYSIZE(_array) ((sizeof(_array) / sizeof(*(_array))))

static unsigned cqc_max_iter = 1000;
static unsigned cqc_min_iter = 100;
static size_t cqc_scale = 100;
static unsigned cqc_verbose;
static bool cqc_debug;

#define CQC_TESTCASE(_id, _descr)                                       \
    static cqc_testing_func cqc_testfunc_##_id;                         \
    static cqc_testcase_list cqc_testcase_##_id = {                     \
        .id = #_id,                                                     \
        .name = _descr,                                                 \
        .test = cqc_testfunc_##_id                                      \
    };                                                                  \
    static __attribute__((constructor)) void                            \
    cqc_testcase_init_##_id(void)                                       \
    {                                                                   \
        *cqc_last_testcase = &cqc_testcase_##_id;                       \
        cqc_last_testcase = &cqc_testcase_##_id.next;                   \
    }                                                                   \
                                                                        \
    __attribute__ ((unused))                                            \
    static void cqc_testfunc_##_id(cqc_result *_cqc_result)

__attribute__ ((unused))
static void
cqc_generate__Bool(void *var, size_t scale __attribute__ ((unused)))
{
    *(bool *)var = (random() % 2) == 0;
}

#define cqc_release__Bool(_var) ((void)0)

#define cqc_typefmt__Bool "%s"
#define cqc_typeargs__Bool(_x) ((_x) ? "true" : "false")

__attribute__ ((unused))
static void
cqc_generate_bits(void *var, size_t bits, size_t scale)
{
    uint64_t r = 0;
    size_t i;

    for (i = 0; i < bits && i < scale; i += CHAR_BIT)
        r = (r << CHAR_BIT) | (random() % UINT8_MAX);

    if (scale < bits)
        r &= (1ull << scale) - 1;

    switch (bits)
    {
        case 8:
            *(uint8_t *)var = (uint8_t)r;
            break;
        case 16:
            *(uint16_t *)var = (uint16_t)r;
            break;
        case 32:
            *(uint32_t *)var = (uint32_t)r;
            break;
        case 64:
            *(uint64_t *)var = r;
            break;
        default:
            assert(0);
    }
}

#define cqc_generate_uint8_t(_var, _scale)      \
    cqc_generate_bits(_var, 8, _scale)
#define cqc_generate_int8_t(_var, _scale)       \
    cqc_generate_bits(_var, 8, _scale)
#define cqc_uint8_t_bounds 0, UINT8_MAX
#define cqc_int8_t_bounds 0, INT8_MAX, INT8_MIN

#define cqc_generate_uint16_t(_var, _scale)     \
    cqc_generate_bits(_var, 16, _scale)
#define cqc_generate_int16_t(_var, _scale)      \
    cqc_generate_bits(_var, 16, _scale)
#define cqc_uint16_t_bounds 0, UINT16_MAX
#define cqc_int16_t_bounds 0, INT16_MAX, INT16_MIN

#define cqc_generate_uint32_t(_var, _scale)     \
    cqc_generate_bits(_var, 32, _scale)
#define cqc_generate_int32_t(_var, _scale)      \
    cqc_generate_bits(_var, 32, _scale)
#define cqc_uint32_t_bounds 0, UINT32_MAX
#define cqc_int32_t_bounds 0, INT32_MAX, INT32_MIN

#define cqc_generate_uint64_t(_var, _scale)     \
    cqc_generate_bits(_var, 64, _scale)
#define cqc_generate_int64_t(_var, _scale)      \
    cqc_generate_bits(_var, 64, _scale)
#define cqc_uint64_t_bounds 0, UINT64_MAX
#define cqc_int64_t_bounds 0, INT64_MAX, INT64_MIN

#define cqc_generate_int(_var, _scale)                      \
    cqc_generate_bits(_var, sizeof(int) * CHAR_BIT, _scale)
#define cqc_int_bounds 0, INT_MAX, INT_MIN

#define cqc_generate_unsigned(_var, _scale)                         \
    cqc_generate_bits(_var, sizeof(unsigned) * CHAR_BIT, _scale)
#define cqc_unsigned_bounds 0, UINT_MAX

#define cqc_generate_size_t(_var, _scale)                       \
    cqc_generate_bits(_var, sizeof(size_t) * CHAR_BIT, _scale)
#define cqc_size_t_bounds 0, SIZE_MAX

#define cqc_generate_uintptr_t(_var, _scale)                        \
    cqc_generate_bits(_var, sizeof(uintptr_t) * CHAR_BIT, _scale)
#define cqc_uintptr_t_bounds 0, UINTPTR_MAX

#define cqc_generate_char(_var, _scale)             \
    cqc_generate_bits(_var, CHAR_BIT, CHAR_BIT - 1)

__attribute__ ((unused))
static void
cqc_generate_cqc_string(char **str, size_t scale)
{
    size_t len = random() % scale;
    size_t i;

    *str = malloc(len + 1);
    for (i = 0; i < len; i++)
        (*str)[i] = random() % (SCHAR_MAX - 1) + 1;
    (*str)[len] = '\0';
}

#define cqc_release_cqc_string(_var) free(_var)

static inline unsigned
cqc_char_class(char c)
{
    if (!isgraph(c))
        return 0;
    if (isalpha(c))
        return 1;
    if (isdigit(c))
        return 2;
    if (ispunct(c))
        return 3;
    assert(0);
    return 0;
}

__attribute__ ((unused))
static void
cqc_generate_double(double *var, size_t scale)
{
    int exp = random() % (2 * scale) - scale;
    double d = (double)random() / RAND_MAX;

    *var = ldexp(d, exp);
}

static inline unsigned
cqc_fp_class(double d)
{
    switch (fpclassify(d))
    {
        case FP_NAN:
            return 0;
        case FP_INFINITE:
            return 1;
        case FP_ZERO:
            return 2;
        case FP_SUBNORMAL:
            return 3;
        case FP_NORMAL:
            return 4;
        default:
            assert(0);
            return 0;
    }
}

#define cqc_release_uint8_t(_var) ((void)0)
#define cqc_release_int8_t(_var) ((void)0)
#define cqc_release_uint16_t(_var) ((void)0)
#define cqc_release_int16_t(_var) ((void)0)
#define cqc_release_uint32_t(_var) ((void)0)
#define cqc_release_int32_t(_var) ((void)0)
#define cqc_release_uint64_t(_var) ((void)0)
#define cqc_release_int64_t(_var) ((void)0)
#define cqc_release_int(_var) ((void)0)
#define cqc_release_unsigned(_var) ((void)0)
#define cqc_release_size_t(_var) ((void)0)
#define cqc_release_uintptr_t(_var) ((void)0)
#define cqc_release_char(_var) ((void)0)
#define cqc_release_double(_var) ((void)0)

#define cqc_equal_uint8_t(_v1, _v2) ((_v1) == (_v2))
#define cqc_equal_int8_t(_v1, _v2) ((_v1) == (_v2))
#define cqc_equal_uint16_t(_v1, _v2) ((_v1) == (_v2))
#define cqc_equal_int16_t(_v1, _v2) ((_v1) == (_v2))
#define cqc_equal_uint32_t(_v1, _v2) ((_v1) == (_v2))
#define cqc_equal_int32_t(_v1, _v2) ((_v1) == (_v2))
#define cqc_equal_uint64_t(_v1, _v2) ((_v1) == (_v2))
#define cqc_equal_int64_t(_v1, _v2) ((_v1) == (_v2))
#define cqc_equal_int(_v1, _v2) ((_v1) == (_v2))
#define cqc_equal_unsigned(_v1, _v2) ((_v1) == (_v2))
#define cqc_equal_size_t(_v1, _v2) ((_v1) == (_v2))
#define cqc_equal_uintptr_t(_v1, _v2) ((_v1) == (_v2))
#define cqc_equal_char(_v1, _v2) ((_v1) == (_v2))
#define cqc_equal_double(_v1, _v2) ((_v1) == (_v2))

#define cqc_typefmt_uint8_t "%" PRIu8
#define cqc_typeargs_uint8_t(_x) (_x)
#define cqc_typefmt_uint16_t "%" PRIu16
#define cqc_typeargs_uint16_t(_x) (_x)
#define cqc_typefmt_uint32_t "%" PRIu32
#define cqc_typeargs_uint32_t(_x) (_x)
#define cqc_typefmt_uint64_t "%" PRIu64
#define cqc_typeargs_uint64_t(_x) (_x)

#define cqc_typefmt_int8_t "%" PRId8
#define cqc_typeargs_int8_t(_x) (_x)
#define cqc_typefmt_int16_t "%" PRId16
#define cqc_typeargs_int16_t(_x) (_x)
#define cqc_typefmt_int32_t "%" PRId32
#define cqc_typeargs_int32_t(_x) (_x)
#define cqc_typefmt_int64_t "%" PRId64
#define cqc_typeargs_int64_t(_x) (_x)

#define cqc_typefmt_int "%d"
#define cqc_typeargs_int(_x) (_x)
#define cqc_typefmt_unsigned "%u"
#define cqc_typeargs_unsigned(_x) (_x)
#define cqc_typefmt_size_t "%zu"
#define cqc_typeargs_size_t(_x) (_x)
#define cqc_typefmt_uintptr_t "%" PRIxPTR
#define cqc_typeargs_uintptr_t(_x) (_x)

#define cqc_typefmt_char "'\\x%02x'"
#define cqc_typeargs_char(_x) (_x)

#define cqc_typefmt_double "%g"
#define cqc_typeargs_double(_x) (_x)

#define cqc_equal_cqc_string(_v1, _v2) \
    (strcmp((_v1), (_v2)) == 0)

#define cqc_typefmt_cqc_string "\"%s\""
#define cqc_typeargs_cqc_string(_x) cqc_string_escape(_x)

#define cqc_equal_cqc_opaque(_v1, _v2) ((_v1) == (_v2))

#define cqc_typefmt_cqc_opaque "%p"
#define cqc_typeargs_cqc_opaque(_x) (_x)

#define CQC_MAX_PRINT_STRING 16

__attribute__ ((unused))
static const char *
cqc_string_escape(const char *src)
{
    static char tmpbuf[4 * CQC_MAX_PRINT_STRING + 4];
    char *iter;
    size_t i;

    for (iter = tmpbuf, i = 0; src[i] != '\0' && i < CQC_MAX_PRINT_STRING; i++)
    {
        if (isprint(src[i]) && src[i] != '"' && src[i] != '\\')
            *iter++ = src[i];
        else
            iter += snprintf(iter, 5, "\\%03o", src[i]);
    }
    if (src[i] != 0)
        strcpy(iter, "...");
    else
        *iter = '\0';
    return tmpbuf;
}

#define cqc_log_value(_type, _var)                          \
    (fprintf(stderr, "[" #_var "=" cqc_typefmt_##_type "]", \
             cqc_typeargs_##_type(_var)))

#define cqc_generate_type(_type, _var, _scale)  \
    cqc_generate_##_type(&(_var), _scale)

#define cqc_cleanup_type(_type, _var)           \
    cqc_release_##_type(_var)

#define cqc_generate_oneof(_type, _var, ...)                            \
    const _type _var##_options[] = {__VA_ARGS__};                       \
    _var = _var##_options[random() % CQC_ARRAYSIZE(_var##_options)];    \

#define cqc_cleanup_oneof(_type, _var) ((void)0)

#define cqc_generate_alt(_type, _var, _ratio, _scale, ...)              \
    bool cqc_need_clean_##_var;                                         \
    if (random() % 100 < (_ratio))                                      \
    {                                                                   \
        cqc_generate_oneof(_type, _var, __VA_ARGS__);                   \
        cqc_need_clean_##_var = false;                                  \
    }                                                                   \
    else                                                                \
    {                                                                   \
        cqc_generate_##_type(&_var, _scale);                            \
        cqc_need_clean_##_var = true;                                   \
    }

#define cqc_cleanup_alt(_type, _var)            \
    (cqc_need_clean_##_var ?                    \
     cqc_cleanup_type(_type, _var) :            \
     cqc_cleanup_oneof(_type, _var))

#define cqc_forall_gen(_type, _var, _gen, _clean, ...)                  \
    _type _var;                                                         \
    bool cqc_guard_##_var;                                              \
    _gen(_type, _var, __VA_ARGS__);                                     \
    if (cqc_verbose > 1)                                                \
        cqc_log_value(_type, _var);                                     \
    for (cqc_guard_##_var = true;                                       \
         cqc_guard_##_var;                                              \
         cqc_guard_##_var = false,                                      \
             (void)(cqc_verbose == 1 &&                                 \
                    (*_cqc_result == CQC_RESULT_FAILED ||               \
                     *_cqc_result == CQC_RESULT_CRASHED) ?              \
                    cqc_log_value(_type, _var) : 0),                    \
             (_clean(_type, _var)))


#define cqc_forall_scaled(_type, _var, _scale)                          \
    cqc_forall_gen(_type, _var, cqc_generate_type, cqc_cleanup_type, _scale)

#define cqc_forall_oneof(_type, _var, ...)                              \
    cqc_forall_gen(_type, _var,                                         \
                   cqc_generate_oneof, cqc_cleanup_oneof, __VA_ARGS__)

#define cqc_forall_alt_scaled(_type, _var, _ratio, _scale, ...)         \
    cqc_forall_gen(_type, _var,                                         \
                   cqc_generate_alt,                                    \
                   cqc_cleanup_alt,                                     \
                   (_ratio), (_scale), __VA_ARGS__)

#define cqc_generate_range(_type, _var, _min, _max)         \
        (_var = random() % ((_max) - (_min) + 1) + (_min))

#define cqc_cleanup_range(_type, _var) ((void)0)

#define cqc_forall_range(_type, _var, _min, _max)                       \
        cqc_forall_gen(_type, _var,                                     \
                       cqc_generate_range, cqc_cleanup_range, _min, _max)

#define cqc_forall(_type, _var)                 \
    cqc_forall_scaled(_type, _var, cqc_scale)

#define cqc_forall_alt(_type, _var, _ratio, ...)                        \
    cqc_forall_alt_scaled(_type, _var, _ratio, cqc_scale, __VA_ARGS__)

#define cqc_condition(_cond)                    \
    if (!(_cond))                               \
        *_cqc_result = CQC_RESULT_SKIPPED;      \
    else

#define cqc_condition_neq(_type, _v1, _v2)              \
    cqc_condition(!cqc_equal_##_type(_v1, _v2))

__attribute__ ((unused))
static void
cqc_log_class_stats(size_t n, const char *const classes[],
                    const unsigned counts[])
{
    size_t i;
    unsigned total;

    for (i = 0, total = 0; i < n; i++)
        total += counts[i];

    for (i = 0; i < n; i++)
    {
        if (counts[i] > 0)
        {
            fprintf(stderr, " [%s %.2f%%]",
                    classes[i], (double)counts[i] * 100 / total);
        }
    }
}

#define cqc_classify(_clsvar, _classifier, ...)                         \
    static const char *const cqc_classes_##_clsvar[] = {__VA_ARGS__};   \
    static uint64_t cqc_observed_##_clsvar;                             \
    static unsigned cqc_classcnt_##_clsvar                              \
    [CQC_ARRAYSIZE(cqc_classes_##_clsvar)];                             \
    bool cqc_guard_##_clsvar;                                           \
    unsigned _clsvar;                                                   \
    _classifier;                                                        \
    assert(_clsvar < CQC_ARRAYSIZE(cqc_classes_##_clsvar));             \
    cqc_classcnt_##_clsvar[_clsvar]++;                                  \
    cqc_observed_##_clsvar |= 1ull << _clsvar;                          \
    if (*_cqc_result == CQC_RESULT_COMPLETE &&                          \
        (cqc_observed_##_clsvar <                                       \
         (1ull << CQC_ARRAYSIZE(cqc_classes_##_clsvar)) - 1))           \
        *_cqc_result = CQC_RESULT_PENDING;                              \
    for (cqc_guard_##_clsvar = true;                                    \
         cqc_guard_##_clsvar;                                           \
         cqc_guard_##_clsvar = false,                                   \
             (*_cqc_result == CQC_RESULT_COMPLETE && cqc_verbose > 0 ?  \
              cqc_log_class_stats(CQC_ARRAYSIZE(cqc_classes_##_clsvar), \
                                  cqc_classes_##_clsvar,                \
                                  cqc_classcnt_##_clsvar) : (void)0),   \
              (void)(cqc_verbose > 0 &&                                 \
                     (*_cqc_result == CQC_RESULT_FAILED ||              \
                      *_cqc_result == CQC_RESULT_CRASHED) ?             \
                     fprintf(stderr, " [%s]",                           \
                             cqc_classes_##_clsvar[_clsvar]) :          \
                     0))                                                \

#define cqc_once                                \
    *_cqc_result = CQC_RESULT_FORCE_COMPLETE;

#define cqc_expect_generic(_isok, _isfail, _setup)                      \
    pid_t _cqc_pid;                                                     \
                                                                        \
    _cqc_pid = fork();                                                  \
    assert(_cqc_pid != (pid_t)(-1));                                    \
    if (_cqc_pid != 0)                                                  \
    {                                                                   \
        int _cqc_status;                                                \
        pid_t _cqc_rc = waitpid(_cqc_pid, &_cqc_status, 0);             \
        assert(_cqc_rc == _cqc_pid);                                    \
        if (!(_isok))                                                   \
        {                                                               \
            *_cqc_result = (_isfail ? CQC_RESULT_FAILED :               \
                            CQC_RESULT_CRASHED);                        \
            if (cqc_verbose > 0)                                        \
                fputc('!', stderr);                                     \
        }                                                               \
    }                                                                   \
    else                                                                \
        for ((!cqc_debug ?                                              \
              setrlimit(RLIMIT_CORE,                                    \
                        &(const struct rlimit){0, 0}) : 0), (void)(_setup);; \
            exit(EXIT_SUCCESS))

#define cqc_expect                                          \
    cqc_expect_generic(WIFEXITED(_cqc_status) &&            \
                       WEXITSTATUS(_cqc_status) == 0,       \
                       WIFSIGNALED(_cqc_status) &&          \
                       WTERMSIG(_cqc_status) == SIGABRT, 0)

#define cqc_expect_crash(_setup, _signal)                   \
    cqc_expect_generic(WIFSIGNALED(_cqc_status) &&          \
                       WTERMSIG(_cqc_status) == _signal,    \
                       WIFEXITED(_cqc_status), _setup)

#define cqc_expect_fail cqc_expect_crash(0, SIGABRT)

#define cqc_expect_timeout(_timeout)            \
    cqc_expect_crash(alarm(_timeout), SIGALRM)

#define cqc_log(_fmt, ...)                          \
          fprintf(stderr, _fmt "\n", __VA_ARGS__)

#define cqc_assert(_expr) assert(_expr)

#define cqc_assert_eq(_type, _v1, _v2)                                  \
    do {                                                                \
        _type __v1 = (_v1);                                             \
        _type __v2 = (_v2);                                             \
        if (!cqc_equal_##_type(__v1, __v2))                             \
        {                                                               \
            cqc_log("Assertion " #_v1 " == " #_v2 " failed: expected "  \
                    cqc_typefmt_##_type ", got " cqc_typefmt_##_type,   \
                    cqc_typeargs_##_type(__v1), cqc_typeargs_##_type(__v2)); \
            abort();                                                    \
        }                                                               \
    } while (0)

#define cqc_assert_neq(_type, _v1, _v2)                                 \
    do {                                                                \
        _type __v1 = (_v1);                                             \
        _type __v2 = (_v2);                                             \
        if (cqc_equal_##_type(__v1, __v2))                              \
        {                                                               \
            cqc_log("Assertion " #_v1 " != " #_v2 " failed: both are "  \
                    cqc_typefmt_##_type,                                \
                    cqc_typeargs_##_type(__v1));                        \
            abort();                                                    \
        }                                                               \
    } while (0)


#define cqc_assert_vec_eq(_type, _n1, _v1, _n2, _v2)        \
    do {                                                    \
        unsigned __i;                                       \
        cqc_assert_eq(unsigned, _n1, _n2);                  \
        for (__i = 0; __i < (_n1); __i++)                   \
        {                                                   \
            cqc_assert_eq(_type, (_v1)[__i], (_v2)[__i]);   \
        }                                                   \
    } while (0)

#define cqc_assert_foreach_eq(_type, _n, _v, _vi,  _c)      \
    do {                                                    \
        unsigned __i;                                       \
        for (__i = 0; __i < (_n); __i++)                    \
        {                                                   \
            _type _vi = (_v)[__i];                          \
            cqc_assert_eq(_type, _vi, (_c));                \
        }                                                   \
    } while (0)

int main(int argc, char *argv[])
{
    cqc_testcase_list *iter;
    static const struct option options[] = {
        {"verbose", no_argument, NULL, 'v'},
        {"debug", no_argument, NULL, 'd'},
        {"min", required_argument, NULL, 'm'},
        {"limit", required_argument, NULL, 'l'},
        {"scale", required_argument, NULL, 's'},
        {"seed", required_argument, NULL, 'S'},
        {"list", no_argument, NULL, 't'},
        {NULL, no_argument, NULL, 0}
    };
    int opt;
    unsigned seed;
    struct timeval now;
    unsigned failed = 0;
    unsigned passed = 0;
    unsigned crashed = 0;
    unsigned unknown = 0;

    gettimeofday(&now, NULL);
    seed = now.tv_sec ^ now.tv_usec;
    while ((opt = getopt_long(argc, argv, "vdm:l:s:S:", options, NULL)) != -1)
    {
        switch (opt)
        {
            case 'v':
                cqc_verbose++;
                break;
            case 'd':
                cqc_debug = true;
                break;
            case 'm':
                cqc_min_iter = strtoul(optarg, NULL, 10);
                break;
            case 'l':
                cqc_max_iter = strtoul(optarg, NULL, 10);
                break;
            case 's':
                cqc_scale = strtoul(optarg, NULL, 10);
                break;
            case 'S':
                seed = strtoul(optarg, NULL, 0);
                break;
            case 't':
            {
                for (iter = cqc_first_testcase; iter != NULL; iter = iter->next)
                {
                    printf("%s %s\n", iter->id, iter->name);
                }
                return 0;
            }
            default:
                return EXIT_FAILURE;
        }
    }
    if (cqc_verbose > 0)
        fprintf(stderr, "Random seed is %u\n", seed);
    srandom(seed);

    for (iter = cqc_first_testcase; iter != NULL; iter = iter->next)
    {
        cqc_result current = CQC_RESULT_PENDING;
        unsigned attempts;
        unsigned probed = 0;

        if (optind < argc && strcmp(iter->id, argv[optind]) != 0)
            continue;

        fprintf(stderr, "%s ", iter->name);

        for (attempts = 0;
             attempts <= cqc_max_iter &&
                 current == CQC_RESULT_PENDING;
             attempts++)
        {
            if (cqc_verbose > 1)
                fputc('.', stderr);
            current = probed > cqc_min_iter ?
                CQC_RESULT_COMPLETE : CQC_RESULT_PENDING;
            iter->test(&current);
            if (current == CQC_RESULT_SKIPPED)
                current = CQC_RESULT_PENDING;
            else
                probed++;
        }
        switch (current)
        {
            case CQC_RESULT_COMPLETE:
            case CQC_RESULT_FORCE_COMPLETE:
                fputs(" OK\n", stderr);
                passed++;
                break;
            case CQC_RESULT_PENDING:
                fputs(" UNKNOWN\n", stderr);
                unknown++;
                break;
            case CQC_RESULT_FAILED:
                fputs(" FAIL\n", stderr);
                failed++;
                break;
            case CQC_RESULT_CRASHED:
                fputs(" CRASH\n", stderr);
                crashed++;
                break;
            default:
                assert(0);
        }
    }
    if (passed + failed + crashed == 0)
    {
        fputs("No tests have been run!!!\n", stderr);
        return EXIT_FAILURE;
    }
    fprintf(stderr, "Passed %u, failed %u, crashed %u, unknown %u\n",
            passed, failed, crashed, unknown);

    return failed + crashed == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* LIBCPEG_H */
