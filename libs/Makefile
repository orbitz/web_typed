OCAMLPATH:=$(PWD):$(OCAMLPATH)

SUBDIRS=ort_prelude stomp

.PHONY: all clean $(SUBDIRS)

all: ort_prelude stomp

ort_prelude:
	$(MAKE) -C ort_prelude

stomp: ort_prelude
	$(MAKE) -C stomp

clean:
	for dir in $(SUBDIRS); do \
	  $(MAKE) -C $$dir clean; \
	done
