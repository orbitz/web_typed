OCAMLPATH:=$(PWD)/libs:$(OCAMLPATH)

.PHONY: all libs clean

all: libs

libs:
	$(MAKE) -C libs

clean:
	$(MAKE) -C libs clean
