SUBDIRS=ort_prelude stomp pa_monad

.PHONY: all clean test $(SUBDIRS)

all: $(SUBDIRS) pa_monad

ort_prelude:
	$(MAKE) -C ort_prelude

stomp: ort_prelude pa_monad
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
