CC = gcc
CFLAGS = -Wall -Wextra -Werror -ggdb3 --coverage
GCOV = gcov
GCOVFLAGS = 
SELFTEST_FLAGS = --verbose --verbose

.PHONY : check

check : selftest
	./$< $(SELFTEST_FLAGS)
	$(GCOV) $(GCOVFLAGS) $(addsuffix .o,$<)

selftest : selftest.c cqc.h
	$(CC) -o $@ $(CPPFLAGS) $(CFLAGS) $< $(LDFLAGS)
