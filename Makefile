OCAMLPATH:=$(PWD)/libs:$(OCAMLPATH)

.PHONY: all libs test clean

all: libs

libs:
	$(MAKE) -C libs

test: libs
	$(MAKE) -C libs test

clean:
	$(MAKE) -C libs clean
