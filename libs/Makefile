SUBDIRS=ort_prelude stomp

.PHONY: all clean test pa_monad $(SUBDIRS)

all: $(SUBDIRS) pa_monad

ort_prelude:
	$(MAKE) -C ort_prelude

stomp: ort_prelude
	$(MAKE) -C stomp

pa_monad:
	$(MAKE) -C pa_monad

test: all
	for dir in $(SUBDIRS); do \
	  $(MAKE) -C $$dir test; \
	done


clean:
	for dir in $(SUBDIRS); do \
	  $(MAKE) -C $$dir clean; \
	done
