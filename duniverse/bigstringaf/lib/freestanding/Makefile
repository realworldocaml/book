PKG_CONFIG_PATH := $(shell opam config var prefix)/lib/pkgconfig

EXISTS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --exists ocaml-freestanding; echo $$?)

.PHONY: all clean
all: libbigstringaf_freestanding_stubs.a

ifeq ($(EXISTS), 1)
libbigstringaf_freestanding_stubs.a:
	touch $@
else
CC ?= cc
FREESTANDING_CFLAGS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --cflags ocaml-freestanding)
CFLAGS := -O3 $(FREESTANDING_CFLAGS)

OBJS=bigstringaf_stubs.o

libbigstringaf_freestanding_stubs.a: $(OBJS)
	$(AR) r $@ $^
endif

clean:
	$(RM) $(OBJS) libbigstringaf_freestanding_stubs.a
