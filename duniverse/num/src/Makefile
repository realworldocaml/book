OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLMKLIB=ocamlmklib
OCAMLFIND=ocamlfind
INSTALL_DATA=install -m 644
INSTALL_DLL=install
INSTALL_DIR=install -d
STDLIBDIR=$(shell $(OCAMLC) -where)
DESTDIR ?=

include $(STDLIBDIR)/Makefile.config

ifeq "$(filter i386 amd64 arm64 power,$(ARCH))" ""
# Unsupported architecture
BNG_ARCH=generic
else
BNG_ARCH=$(ARCH)
endif

CAMLCFLAGS=-w +a-4-9-41-42-44-45-48 -warn-error A -bin-annot -g \
          -safe-string -strict-sequence -strict-formats
CAMLOPTFLAGS=$(CAMLCFLAGS)
ifeq "$(FLAMBDA)" "true"
CAMLOPTFLAGS+=-O3
endif

CMIS=big_int.cmi nat.cmi num.cmi ratio.cmi arith_status.cmi
CMOS=int_misc.cmo nat.cmo big_int.cmo arith_flags.cmo \
  ratio.cmo num.cmo arith_status.cmo
CMXS=$(CMOS:.cmo=.cmx)
COBJS=bng.$(O) nat_stubs.$(O)

all:: libnums.$(A) nums.cma

ifneq "$(ARCH)" "none"
all:: nums.cmxa
endif

ifeq "$(NATDYNLINK)" "true"
all:: nums.cmxs
endif

libnums.$(A): $(COBJS)
	$(OCAMLMKLIB) -oc nums $(COBJS)

nums.cma: $(CMOS)
	$(OCAMLMKLIB) -o nums -oc nums -linkall $(CMOS)

nums.cmxa: $(CMXS)
	$(OCAMLMKLIB) -o nums -oc nums -linkall $(CMXS)

nums.cmxs: nums.cmxa libnums.$(A)
	$(OCAMLOPT) $(CAMLOPTFLAGS) -I . -shared -o nums.cmxs nums.cmxa

# We hard-code the C dependencies rather than having them generated
# because the dependencies are so simple.
bng.$(O): bng.h bng_*.c
nat_stubs.$(O): bng.h nat.h

%.cmi: %.mli
	$(OCAMLC) $(CAMLCFLAGS) -c $*.mli
%.cmo: %.ml
	$(OCAMLC) $(CAMLCFLAGS) -c $*.ml
%.cmx: %.ml
	$(OCAMLOPT) $(CAMLOPTFLAGS) -c $*.ml
%.$(O): %.c
	$(OCAMLC) -ccopt -DBNG_ARCH_$(BNG_ARCH) -c $*.c

# Legacy installation: files go into OCaml's stdlib directory; only META
# is installed via findlib

TOINSTALL=nums.cma libnums.$(A) $(CMIS) $(CMIS:.cmi=.mli) $(CMIS:.cmi=.cmti)
ifneq "$(ARCH)" "none"
TOINSTALL+=nums.cmxa nums.$(A) $(CMXS)
endif
ifeq "$(NATDYNLINK)" "true"
TOINSTALL+=nums.cmxs
endif
ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
TOINSTALL_STUBS=dllnums$(EXT_DLL)
else
TOINSTALL_STUBS=
endif

VERSION=$(shell sed -ne 's/^ *version *: *"\([^"]*\)".*$$/\1/p' ../num.opam)

install:
	$(INSTALL_DIR) $(DESTDIR)$(STDLIBDIR)
	sed -e 's/%%VERSION%%/$(VERSION)/g' META.in > META
	$(OCAMLFIND) install num META
	rm -f META
	$(INSTALL_DATA) $(TOINSTALL) $(DESTDIR)$(STDLIBDIR)
ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
	$(INSTALL_DIR) $(DESTDIR)$(STDLIBDIR)/stublibs
	$(INSTALL_DLL) $(TOINSTALL_STUBS) $(DESTDIR)$(STDLIBDIR)/stublibs
endif

findlib-install:
	sed -e '/\^/d' -e 's/%%VERSION%%/$(VERSION)/g' META.in > META
	$(OCAMLFIND) install num META $(TOINSTALL) $(TOINSTALL_STUBS)
	rm -f META

findlib-uninstall:
	$(OCAMLFIND) remove num

uninstall: findlib-uninstall
	cd $(DESTDIR)$(STDLIBDIR) && rm -f $(TOINSTALL)
ifeq "$(SUPPORTS_SHARED_LIBRARIES)" "true"
	cd $(DESTDIR)$(STDLIBDIR)/stublibs && rm -f $(TOINSTALL_STUBS)
endif

clean:
	rm -f *.cm[ioxta] *.cmx[as] *.cmti *.$(O) *.$(A) *$(EXT_DLL)

depend:
	$(OCAMLDEP) -slash *.mli *.ml > .depend

include .depend
