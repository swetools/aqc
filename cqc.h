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
#include <stdio.h>

typedef bool (*cqc_testing_func)(size_t scale);

typedef struct cqc_testcase_list {
    const char *name;
    cqc_testing_func test;
    struct cqc_testcase_list *next;
} cqc_testcase_list;

static cqc_testcase_list *cqc_first_testcase;
static cqc_testcase_list **cqc_last_testcase;

typedef struct cqc_result {
    unsigned passed;
    unsigned failed;
    unsigned uncertain;
} cqc_result;

#define CQC_ARRAYSIZE(_array) ((sizeof(_array) / sizeof(*(_array))))

static unsigned cqc_max_iter;
static unsigned cqc_min_iter;
static size_t cqc_scale;
static bool cqc_verbose;
static bool cqc_debug;

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
        while ((_cqc_state.classmask != _cqc_max_mask) &&               \
               (_cqc_state.result.passed +                              \
                _cqc_state.result.failed < cqc_min_iters) &&            \
               (_cqc_state.attempts++ < _cqc_max_iter))                 \
        {                                                               \
            _cqc_state.failed_iter = false;                             \
            _body;                                                      \
        }                                                               \
        if (_cqc_state.classmask != _cqc_max_mask) {                    \
            fprintf(stderr, " Arguments exhausted after %u attempts! ", \
                    _cqc_state.attempts);                               \
            _cqc_state.result.uncertain = 1;                            \
        }                                                               \
        if (_cqc_max_mask != 1)                                         \
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
    struct cqc_fake


#define cqc_forall(_type, _var, _body)                                  \
    do {                                                                \
        _type _var;                                                     \
        static unsigned _var##_cqc_serial;                              \
                                                                        \
        cqc_generate_##_type(&_var, cqc_scale, _var##_cqc_serial++);    \
        _body;                                                          \
        if (cqc_verbose && _cqc_state.failed_iter)                      \
        {                                                               \
            fprintf(stderr, "[" #_var "=" cqc_typefmt_##_type "]",      \
                    cqc_typeargs_##_type(_var));                        \
        }                                                               \
        cqc_release_##_type(&_var);                                     \
    } while (0)

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
        if (cqc_verbose && _cqc_state.failed_iter)              \
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
                if (cqc_verbose)                                        \
                    fputc('.', stderr);                                 \
            }                                                           \
            else                                                        \
            {                                                           \
                _cqc_state.result.failed++;                             \
                _cqc_state.failed_iter = true;                          \
                if (cqc_verbose)                                        \
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

int main(int argc, char *argv)
{
    cqc_testcase_list *iter;
    cqc_result total = {0, 0, 0};

    for (iter = cqc_first_casecase; iter != NULL; iter = iter->next)
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
