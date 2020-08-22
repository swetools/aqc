TARGETS = bytecode native native-profile

.PHONY : all

all : $(TARGETS)

.PHONY : $(TARGETS)

$(TARGETS) :
	make -C build/$@

.PHONY : clean

clean : $(addprefix clean-,$(TARGETS))

.PHONY : $(addprefix clean-,$(TARGETS))

$(addprefix clean-,$(TARGETS)) :
	make -C build/$(patsubst clean-%,%,$@) clean
