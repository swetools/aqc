OCAMLC = ocamlopt
OCAMLCFLAGS = -principal -safe-string -w @a
OCAML.O = cmx
OCAML.A = cmxa
OCAMLDEP = ocamldep

LIBRARIES = oqc.$(OCAML.A)

SOURCES = oqc.mli oqc_tap.mli oqc_logic.mli oqc_iterate.mli \
		  oqc_domains.mli

oqc_SOURCES = oqc.ml oqc_tap.ml oqc_logic.ml oqc_iterate.ml \
			  oqc_domains.ml

SOURCES += $(oqc_SOURCES)

oqc.$(OCAML.A) : $(patsubst %.ml,%.$(OCAML.O),$(oqc_SOURCES))

.PHONY : all

all : $(LIBRARIES)

%.cmi : %.mli
	$(OCAMLC) -c -o $@ $(OCAMLCFLAGS) $<

%.$(OCAML.O) : %.ml
	$(OCAMLC) -c -o $@ $(OCAMLCFLAGS) $<

$(LIBRARIES) : %.$(OCAML.A) :
	$(OCAMLC) -a -o $@ $(OCAMLCFLAGS) $^

.PHONY : clean

clean :
	rm -f *.cmi *.$(OCAML.A) *.$(OCAML.O) *.a *.o

.depends : $(SOURCES)
	$(OCAMLDEP) $^ >$@

include .depends
