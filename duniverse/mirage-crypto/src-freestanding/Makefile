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
all: libmirage_crypto_freestanding_stubs.a

ifeq ($(EXISTS), 1)
libmirage_crypto_freestanding_stubs.a:
	touch $@
else
CC ?= cc
FREESTANDING_CFLAGS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --cflags ocaml-freestanding)
MIRAGE_CRYPTO_OPT_CFLAGS := $(shell sed 's/^(\(.*\))$$/\1/' ../src/cflags_optimized.sexp | tr -d '"')
CFLAGS := -I../src/native $(MIRAGE_CRYPTO_OPT_CFLAGS) $(FREESTANDING_CFLAGS)

MIRAGE_CRYPTO_CFLAGS := $(shell sed 's/^(\(.*\))$$/\1/' ../src/cflags.sexp | tr -d '"')
UNOPT_CFLAGS := -I../src/native $(MIRAGE_CRYPTO_CFLAGS) $(FREESTANDING_CFLAGS)

chacha_generic.o: chacha_generic.c
	$(CC) $(UNOPT_CFLAGS) -c -o chacha_generic.o chacha_generic.c

UNOPT_OBJS=chacha_generic.o

OBJS=detect_cpu_features.o misc.o misc_sse.o hash_stubs.o md5.o sha1.o \
  sha256.o sha512.o aes_generic.o aes_aesni.o des_generic.o chacha.o \
  poly1305-donna.o ghash_pclmul.o ghash_generic.o ghash_ctmul.o \
  entropy_cpu_stubs.o

libmirage_crypto_freestanding_stubs.a: $(OBJS) $(UNOPT_OBJS)
	$(AR) r $@ $^
endif

clean:
	$(RM) $(OBJS) libmirage_crypto_freestanding_stubs.a
