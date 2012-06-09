SUBDIRS=ort_prelude stomp pa_monad ort_async

.PHONY: all clean test $(SUBDIRS)

all: $(SUBDIRS) pa_monad

ort_prelude:
	$(MAKE) -C ort_prelude

stomp: ort_prelude pa_monad ort_async
	$(MAKE) -C stomp

pa_monad:
	$(MAKE) -C pa_monad

ort_async: pa_monad
	$(MAKE) -C ort_async

test: all
	for dir in $(SUBDIRS); do \
	  $(MAKE) -C $$dir test; \
	done


clean:
	for dir in $(SUBDIRS); do \
	  $(MAKE) -C $$dir clean; \
	done
