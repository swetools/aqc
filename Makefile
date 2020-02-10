CC = gcc
CFLAGS = -O3 -Wall -Wextra -Werror -ggdb3 -march=native -fsanitize=undefined
SELFTEST_FLAGS =
PROVE = prove

.PHONY : check

check : selftest
	$(PROVE) ./$< :: $(SELFTEST_FLAGS)

selftest : selftest.c cqc.h Makefile
	$(CC) -o $@ $(CPPFLAGS) $(CFLAGS) $< $(LDFLAGS)

.PHONY : clean

clean :
	rm -f selftest
	rm -f *.o
