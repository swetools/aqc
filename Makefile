OCAMLC = ocamlopt
OCAMLCFLAGS = -principal -safe-string -w @a
OCAML.O = cmx
OCAML.A = cmxa
OCAMLDEP = ocamldep

LIBRARIES = oqc.$(OCAML.A)
APPS = selftest

SOURCES = oqc.mli oqc_tap.mli oqc_logic.mli oqc_iterate.mli \
		  oqc_domains.mli

oqc_SOURCES = oqc_tap.ml oqc.ml oqc_logic.ml oqc_iterate.ml \
			  oqc_domains.ml

selftest_SOURCES = selftest.ml

SOURCES += $(oqc_SOURCES)

.PHONY : all

all : $(LIBRARIES) $(APPS)

oqc.$(OCAML.A) : $(patsubst %.ml,%.$(OCAML.O),$(oqc_SOURCES))

selftest : oqc.$(OCAML.A) $(patsubst %.ml,%.$(OCAML.O),$(selftest_SOURCES))


%.cmi : %.mli
	$(OCAMLC) -c -o $@ $(OCAMLCFLAGS) $<

%.$(OCAML.O) : %.ml
	$(OCAMLC) -c -o $@ $(OCAMLCFLAGS) $<

$(LIBRARIES) : %.$(OCAML.A) :
	$(OCAMLC) -a -o $@ $(OCAMLCFLAGS) $^

$(APPS) : % :
	$(OCAMLC) -o $@ $(OCAMLCFLAGS) $^


.PHONY : clean

clean :
	rm -f *.cmi *.$(OCAML.A) *.$(OCAML.O) *.a *.o

.depends : $(SOURCES)
	$(OCAMLDEP) $^ >$@

include .depends
