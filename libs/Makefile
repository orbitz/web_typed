SUBDIRS=ort_prelude stomp

.PHONY: all clean test $(SUBDIRS)

all: ort_prelude stomp

ort_prelude:
	$(MAKE) -C ort_prelude

stomp: ort_prelude
	$(MAKE) -C stomp

test: all
	for dir in $(SUBDIRS); do \
	  $(MAKE) -C $$dir test; \
	done


clean:
	for dir in $(SUBDIRS); do \
	  $(MAKE) -C $$dir clean; \
	done
