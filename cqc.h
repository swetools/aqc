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

typedef struct cqc_result {
    unsigned passed;
    unsigned failed;
    unsigned uncertain;
} cqc_result;

typedef cqc_result (*cqc_testing_func)(void);

typedef struct cqc_testcase_list {
    const char *name;
    cqc_testing_func test;
    struct cqc_testcase_list *next;
} cqc_testcase_list;

static cqc_testcase_list *cqc_first_testcase;
static cqc_testcase_list **cqc_last_testcase = &cqc_first_testcase;

#define CQC_ARRAYSIZE(_array) ((sizeof(_array) / sizeof(*(_array))))

static unsigned cqc_max_iter = 1000;
static unsigned cqc_min_iter = 100;
static size_t cqc_scale = 100;
static unsigned cqc_verbose;
static bool cqc_debug;

#define CQC_TESTCASE_INIT(_descr, _id)          \
    static cqc_testcase_list _id##_cqc_testcase = {                     \
        .name = _descr,                                                 \
        .test = _id                                                     \
    };                                                                  \
    static __attribute__((constructor)) void                            \
    _id##_cqc_testcase_init(void)                                       \
    {                                                                   \
        *cqc_last_testcase = &_id##_cqc_testcase;                       \
        cqc_last_testcase = &_id##_cqc_testcase.next;                   \
    }                                                                   \

#define CQC_TESTCASE_SINGLE(_id, _descr, _body)                         \
    static cqc_result                                                   \
    _id()                                                               \
    {                                                                   \
        struct {                                                        \
            unsigned attempts;                                          \
            cqc_result result;                                          \
            bool failed_iter;                                           \
        } _cqc_state = {.attempts = 0 };                                \
                                                                        \
        while ((_cqc_state.result.passed +                              \
                _cqc_state.result.failed < 1) &&                        \
               (_cqc_state.attempts++ < cqc_max_iter))                  \
        {                                                               \
            _cqc_state.failed_iter = false;                             \
            _body;                                                      \
        }                                                               \
        if (_cqc_state.result.passed +                                  \
            _cqc_state.result.failed < 1) {                             \
            fprintf(stderr, " Arguments exhausted after %u attempts! ", \
                    _cqc_state.attempts);                               \
            _cqc_state.result.uncertain = 1;                            \
        }                                                               \
        return _cqc_state.result;                                       \
    }                                                                   \
                                                                        \
    CQC_TESTCASE_INIT(_descr, _id);                                     \
    struct cqc_fake

#define CQC_TESTCASE(_id, _descr, _classes, _body)                      \
    static cqc_result                                                   \
    _id()                                                               \
    {                                                                   \
        static const char *_cqc_classes[] = _classes;                   \
        static const unsigned _cqc_max_mask =                           \
            ((1u << CQC_ARRAYSIZE(_cqc_classes)) - 1);                  \
        struct {                                                        \
            unsigned classcnt[CQC_ARRAYSIZE(_cqc_classes)];             \
            uint64_t classmask;                                         \
            unsigned attempts;                                          \
            cqc_result result;                                          \
            bool failed_iter;                                           \
        } _cqc_state = {.attempts = 0,                                  \
                        .classmask = _cqc_max_mask == 1 ? 1 : 0};       \
                                                                        \
        while (((_cqc_state.classmask != _cqc_max_mask) ||              \
                (_cqc_state.result.passed +                             \
                _cqc_state.result.failed < cqc_min_iter)) &&            \
               (_cqc_state.attempts++ < cqc_max_iter))                  \
        {                                                               \
            _cqc_state.failed_iter = false;                             \
            _body;                                                      \
        }                                                               \
        if ((_cqc_state.classmask != _cqc_max_mask) ||                  \
            (_cqc_state.result.passed +                                 \
             _cqc_state.result.failed < cqc_min_iter)) {                \
            fprintf(stderr, " Arguments exhausted after %u attempts! ", \
                    _cqc_state.attempts);                               \
            _cqc_state.result.uncertain = 1;                            \
        }                                                               \
        if (_cqc_max_mask != 1 && cqc_verbose > 0)                      \
        {                                                               \
            unsigned i;                                                 \
            for (i = 0; i < CQC_ARRAYSIZE(_cqc_classes); i++)           \
            {                                                           \
                fprintf(stderr, " [%u %s]", _cqc_state.classcnt[i],     \
                        _cqc_classes[i]);                               \
            }                                                           \
        }                                                               \
        return _cqc_state.result;                                       \
    }                                                                   \
                                                                        \
    CQC_TESTCASE_INIT(_descr, _id);                                     \
    struct cqc_fake

#define CQC_NO_CLASSES ((const char *[]){""})
#define CQC_BOOL_CLASSES                        \
    ((const char *[]){"false", "true"})

static inline void
cqc_generate__Bool(void *var, size_t scale __attribute__ ((unused)))
{
    *(bool *)var = (random() % 2) == 0;
}

#define cqc_release__Bool(_var) ((void)0)

#define cqc_typefmt__Bool "%s"
#define cqc_typeargs__Bool(_x) ((_x) ? "true" : "false")

static inline void
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
#define cqc_cc_list_uint8_t 0, UINT8_MAX
#define cqc_cc_list_int8_t 0, INT8_MAX, INT8_MIN

#define cqc_generate_uint16_t(_var, _scale)     \
    cqc_generate_bits(_var, 16, _scale)
#define cqc_generate_int16_t(_var, _scale)      \
    cqc_generate_bits(_var, 16, _scale)
#define cqc_cc_list_uint16_t 0, UINT16_MAX
#define cqc_cc_list_int16_t 0, INT16_MAX, INT16_MIN

#define cqc_generate_uint32_t(_var, _scale)     \
    cqc_generate_bits(_var, 32, _scale)
#define cqc_generate_int32_t(_var, _scale)      \
    cqc_generate_bits(_var, 32, _scale)
#define cqc_cc_list_uint32_t 0, UINT32_MAX
#define cqc_cc_list_int32_t 0, INT32_MAX, INT32_MIN

#define cqc_generate_uint64_t(_var, _scale)     \
    cqc_generate_bits(_var, 64, _scale)
#define cqc_generate_int64_t(_var, _scale)      \
    cqc_generate_bits(_var, 64, _scale)
#define cqc_cc_list_uint64_t 0, UINT64_MAX
#define cqc_cc_list_int64_t 0, INT64_MAX, INT64_MIN

#define cqc_generate_int(_var, _scale)                      \
    cqc_generate_bits(_var, sizeof(int) * CHAR_BIT, _scale)
#define cqc_cc_list_int 0, INT_MAX, INT_MIN

#define cqc_generate_unsigned(_var, _scale)                         \
    cqc_generate_bits(_var, sizeof(unsigned) * CHAR_BIT, _scale)
#define cqc_cc_list_unsigned 0, UINT_MAX

#define cqc_generate_size_t(_var, _scale)                       \
    cqc_generate_bits(_var, sizeof(size_t) * CHAR_BIT, _scale)
#define cqc_cc_list_size_t 0, SIZE_MAX

#define cqc_generate_char(_var, _scale)             \
    cqc_generate_bits(_var, CHAR_BIT, CHAR_BIT - 1)

static inline void
cqc_generate_double(double *var, size_t scale)
{
    int exp = random() % (2 * scale) - scale;
    double d = (double)random() / RAND_MAX;

    *var = ldexp(d, exp);
}
#define cqc_cc_list_double                                              \
    0.0, DBL_MAX, DBL_MIN, -DBL_MAX, -DBL_MIN, INFINITY, -INFINITY, NAN

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
#define cqc_release_char(_var) ((void)0)
#define cqc_release_double(_var) ((void)0)

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

#define cqc_typefmt_char "'\x%02x'"
#define cqc_typeargs_char(_x) (_x)

#define cqc_typefmt_double "%g"
#define cqc_typeargs_double(_x) (_x)

#define cqc_log_value(_type, _var)                          \
    (fprintf(stderr, "[" #_var "=" cqc_typefmt_##_type "]", \
             cqc_typeargs_##_type(_var)))

#define cqc_list(...) __VA_ARGS__

#define cqc_generate_oneof(_type, _var, ...)                            \
    static const _type _var##_options[] = {__VA_ARGS__};                \
    _var = _var##_options[random() % CQC_ARRAYSIZE(_var##_options)];    \

#define cqc_generate_cc(_ratio, _scale, _type, _var)            \
    if (random() % 100 < (_ratio))                              \
    {                                                           \
        cqc_generate_oneof(_type, _var, cqc_cclist_##_type);    \
    }                                                           \
    else                                                        \
    {                                                           \
        cqc_generate_##_type(&_var, _scale);                    \
    }

#define cqc_forall_gen(_gen, _type, _var, _body)                        \
    do {                                                                \
        _type _var;                                                     \
                                                                        \
        _gen;                                                           \
        if (cqc_verbose > 1)                                            \
            cqc_log_value(_type, _var);                                 \
        _body;                                                          \
        if (cqc_verbose == 1 && _cqc_state.failed_iter)                 \
            cqc_log_value(_type, _var);                                 \
        cqc_release_##_type(&_var);                                     \
    } while (0)


#define cqc_forall_scaled(_scale, _type, _var, _body)                   \
    cqc_forall_gen(cqc_generate_##_type(&_var, _scale), _type, _var, _body)

#define cqc_forall_cc_scaled(_ratio, _scale, _type, _var, _body)   \
    cqc_forall_gen(cqc_generate_cc(_ratio, _scale, _type, _var),   \
                   _type, _var, _body)

#define cqc_forall_oneof(_type, _var, _opts, _body)                     \
    cqc_forall_gen(cqc_generate_oneof(_type, _var, _opts),              \
                   _type, _var, _body)

#define cqc_forall_range(_type, _var, _min, _max, _body)            \
    cqc_forall_gen(_var = random() % ((_max) - (_min) + 1) + _min,  \
                   _type, _var, _body)

#define cqc_forall(_type, _var, _body)                  \
    cqc_forall_scaled(cqc_scale, _type, _var, _body)

#define cqc_forall_cc(_ratio, _type, _var, _body)               \
    cqc_forall_cc_scaled(_ratio, cqc_scale, _type, _var, _body)

#define cqc_condition(_cond, _body) \
    do {                            \
        if ((_cond))                \
        {                           \
            _body;                  \
        }                           \
    } while (0)

#define cqc_classify(_classifier, _body)                        \
    do {                                                        \
        unsigned _cqc_cls = (_classifier);                      \
        assert(_cqc_cls < CQC_ARRAYSIZE(_cqc_classes));         \
        _cqc_state.classmask |= (1u << _cqc_cls);               \
        _cqc_state.classcnt[_cqc_cls]++;                        \
        _body;                                                  \
        if (cqc_verbose > 0 && _cqc_state.failed_iter)              \
            fprintf(stderr, "[%s]", _cqc_classes[_cqc_cls]);    \
    } while (0)

#define cqc_scale(_scaler, _body)               \
    do {                                        \
        size_t _cqc_newscale = (_scaler);       \
        {                                       \
            size_t cqc_scale = _cqc_newscale;   \
            _body;                              \
        }                                       \
    } while (0);


#define cqc_expect_generic(_body, _chkstatus)                           \
    do {                                                                \
        pid_t _cqc_pid;                                                 \
                                                                        \
        _cqc_pid = fork();                                              \
        assert(_cqc_pid != (pid_t)(-1));                                \
        if (_cqc_pid == 0)                                              \
        {                                                               \
            if (!cqc_debug)                                             \
                setrlimit(RLIMIT_CORE, &(const struct rlimit){0, 0});   \
            _body;                                                      \
            exit(EXIT_SUCCESS);                                         \
        }                                                               \
        else                                                            \
        {                                                               \
            int _cqc_status;                                            \
            pid_t _cqc_rc = waitpid(_cqc_pid, &_cqc_status, 0);         \
            assert(_cqc_rc == _cqc_pid);                                \
            if (_chkstatus)                                             \
            {                                                           \
                _cqc_state.result.passed++;                             \
                if (cqc_verbose == 1)                                   \
                    fputc('.', stderr);                                 \
            }                                                           \
            else                                                        \
            {                                                           \
                _cqc_state.result.failed++;                             \
                _cqc_state.failed_iter = true;                          \
                if (cqc_verbose > 0)                                    \
                    fputc('!', stderr);                                 \
            }                                                           \
        }                                                               \
    } while (0)

#define cqc_expect(_body)                                               \
    cqc_expect_generic(_body,                                           \
                       WIFEXITED(_cqc_status) &&                        \
                       WEXITSTATUS(_cqc_status) == 0)

#define cqc_expect_crash(_signal, _body)                                \
    cqc_expect_generic(_body,                                           \
                       WIFSIGNALED(_cqc_status) &&                      \
                       WTERMSIG(_cqc_status) == _signal)

#define cqc_expect_fail(_body) cqc_expect_crash(SIGABRT, _body)

#define cqc_expect_timeout(_timeout, _body)             \
    cqc_expect_crash(SIGALRM, alarm(_timeout); _body)

#define cqc_log(_fmt, ...)                          \
          fprintf(stderr, _fmt "\n", __VA_ARGS__)

int main(int argc, char *argv[])
{
    cqc_testcase_list *iter;
    cqc_result total = {0, 0, 0};
    static const struct option options[] = {
        {"verbose", no_argument, NULL, 'v'},
        {"debug", no_argument, NULL, 'd'},
        {"min", required_argument, NULL, 'm'},
        {"limit", required_argument, NULL, 'l'},
        {"scale", required_argument, NULL, 's'},
        {"seed", required_argument, NULL, 'S'},
        {NULL, no_argument, NULL, 0}
    };
    int opt;
    unsigned seed;
    struct timeval now;

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
            default:
                assert(0);
        }
    }
    if (cqc_verbose > 0)
        fprintf(stderr, "Random seed is %u\n", seed);
    srandom(seed);

    for (iter = cqc_first_testcase; iter != NULL; iter = iter->next)
    {
        cqc_result current;
        fprintf(stderr, "%s ", iter->name);
        current = iter->test();
        total.passed += current.passed;
        total.failed += current.failed;
        total.uncertain += current.uncertain;

        if (current.failed + current.passed == 0)
            fputs(" SKIP\n", stderr);
        else if (current.failed != 0)
            fputs(" FAIL\n", stderr);
        else
            fputs(" OK\n", stderr);
    }
    if (total.passed + total.failed == 0)
    {
        fputs("No tests have been run!!!\n", stderr);
        return EXIT_FAILURE;
    }
    fprintf(stderr, "%u/%u tests failed, %u uncertain testcases\n",
            total.failed, total.passed + total.failed,
            total.uncertain);

    return total.failed == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}


#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* LIBCPEG_H */
