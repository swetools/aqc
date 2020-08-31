SRCDIR = ../..
OCAMLC = ocamlcp -g -P a 
OCAML.O = cmo
OCAML.A = cma
VPATH = $(SRCDIR)
OCAMLPROF = ocamlprof
PROVE = prove

include $(SRCDIR)/Makefile.rules

.PHONY : profile

profile : coverage.txt

%.cov : %.ml ocamlprof.dump
	cp ocamlprof.dump ocamlprof.dump~
	-$(OCAMLPROF) $< | nl -ba >$@
	mv ocamlprof.dump~ ocamlprof.dump

coverage.txt : $(patsubst %.ml,%.cov,$(filter %.ml,$(SOURCES)))
	-rm -f $@
	for f in $^; do \
		if test -s $$f; then \
			exlines="$$(grep -c '(\* [0-9]* \*)' $$f)"; \
			zlines="$$(grep -c '(\* 0 \*)' $$f)"; \
			if test "$$zlines" -gt 0; then \
				echo "$$f: $$zlines of $$exlines lines uncovered" >>$@; \
			else \
				echo "$$f: completely covered" >>$@; \
			fi; \
		else \
			echo "$$f: totally uncovered" >>$@; \
		fi; \
	done

ocamlprof.dump : $(APPS)
	for i in $(APPS); do		 \
		$(PROVE) ./$$i; \
		$(PROVE) ./$$i :: -verbose ; \
		$(PROVE) ./$$i :: -dont-expect-failures || true; \
		$(PROVE) ./$$i :: -fatal-failures -dont-expect-failures || true; \
	done
