ifneq (, $(shell command -v opam))
  ifeq ($(origin PKG_CONFIG_PATH), undefined)
    PKG_CONFIG_PATH := $(shell opam var prefix)/lib/pkgconfig
  endif
endif

ifeq ($(OS), Windows_NT)
  ifneq (, $(shell command -v cygpath))
    PKG_CONFIG_PATH := $(shell cygpath --unix --path "$(PKG_CONFIG_PATH)")
  endif
endif

EXISTS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --exists ocaml-freestanding; echo $$?)

.PHONY: all clean
all: libmirage_crypto_ec_freestanding_stubs.a

ifeq ($(EXISTS), 1)
libmirage_crypto_ec_freestanding_stubs.a:
	touch $@
else
CC ?= cc
FREESTANDING_CFLAGS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --cflags ocaml-freestanding)
DISCOVER_CFLAGS := $(shell sed 's/^(\(.*\))$$/\1/' ../ec/cflags_optimized.sexp | tr -d '"')
CFLAGS := -DNDEBUG -O3 -I../ec/native -I../src/native $(DISCOVER_CFLAGS) $(FREESTANDING_CFLAGS)

OBJS=p224_stubs.o np224_stubs.o p256_stubs.o np256_stubs.o p384_stubs.o np384_stubs.o p521_stubs.o np521_stubs.o curve25519_stubs.o

libmirage_crypto_ec_freestanding_stubs.a: $(OBJS)
	$(AR) r $@ $^
endif

clean:
	$(RM) $(OBJS) libmirage_crypto_ec_freestanding_stubs.a
