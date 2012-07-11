SUBDIRS=ort_prelude stomp pa_monad ort_async

subdirs_test=$(patsubst %,%_test,$(SUBDIRS))
subdirs_clean=$(patsubst %,%_clean,$(SUBDIRS))

.PHONY: all clean test $(SUBDIRS) $(subdirs_test) $(subdirs_clean)

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

stomp: ort_prelude pa_monad ort_async

ort_async: pa_monad

test: $(subdirs_test)

$(subdirs_test):
	$(MAKE) -C $(patsubst %_test,%,$@) test

clean: $(subdirs_clean)

$(subdirs_clean):
	-$(MAKE) -C $(patsubst %_clean,%,$@) clean
