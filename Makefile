GNATMAKE = gnatmake
GNATCLEAN = gnatclean
GNATFLAGS = -gnata -gnatVa -gnatwa -gnaty -we
CFLAGS = --coverage
LDFLAGS = --coverage
SELFTEST_FLAGS =
PROVE = prove
GCOV = gcov
GCOVFLAGS =

.PHONY : check

check : aqc-selftest-main
	$(PROVE) ./$< :: $(SELFTEST_FLAGS)
	$(GCOV) $(GCOVFLAGS) *.o

aqc-selftest-main:
	$(GNATMAKE) $(GNATFLAGS) $@ -cargs $(CFLAGS) -largs $(LDFLAGS)

.PHONY : clean

clean :
	$(GNATCLEAN) *.ads
	rm -f *.gcda *.gcno *.gcov
