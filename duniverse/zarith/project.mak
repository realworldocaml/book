# This file is part of the Zarith library 
# http://forge.ocamlcore.org/projects/zarith .
# It is distributed under LGPL 2 licensing, with static linking exception.
# See the LICENSE file included in the distribution.
#   
# Copyright (c) 2010-2011 Antoine Miné, Abstraction project.
# Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
# a joint laboratory by:
# CNRS (Centre national de la recherche scientifique, France),
# ENS (École normale supérieure, Paris, France),
# INRIA Rocquencourt (Institut national de recherche en informatique, France).

ifeq "$(shell $(OCAMLC) -config |grep ccomp_type)" "ccomp_type: msvc"
OBJSUFFIX    := obj
LIBSUFFIX    := lib
DLLSUFFIX    := dll
EXE          := .exe
else
OBJSUFFIX    := o
LIBSUFFIX    := a
ifeq "$(findstring mingw,$(shell $(OCAMLC) -config |grep system))" "mingw"
DLLSUFFIX    := dll
EXE          := .exe
else
DLLSUFFIX    := so
EXE          :=
endif
endif


# project files
###############

CSRC = caml_z.c
SSRC = $(wildcard caml_z_$(ARCH).S)
MLSRC = z.ml q.ml big_int_Z.ml
MLISRC = z.mli q.mli big_int_Z.mli

AUTOGEN = z.ml z.mli z_features.h

CMIOBJ = $(MLISRC:%.mli=%.cmi)
CMXOBJ = $(MLISRC:%.mli=%.cmx)
CMIDOC = $(MLISRC:%.mli=%.cmti)

TOINSTALL := zarith.h zarith.cma libzarith.$(LIBSUFFIX) $(MLISRC) $(CMIOBJ) \
  zarith_top.cma

ifeq ($(HASOCAMLOPT),yes)
TOINSTALL += zarith.$(LIBSUFFIX) zarith.cmxa $(CMXOBJ)
endif

OCAMLFLAGS = -I +compiler-libs
OCAMLOPTFLAGS = -I +compiler-libs

ifeq ($(HASDYNLINK),yes)
TOINSTALL += zarith.cmxs
endif

ifeq ($(HASBINANNOT),yes)
TOINSTALL += $(CMIDOC)
OCAMLFLAGS += -bin-annot
endif

# build targets
###############

all: $(TOINSTALL)

tests:
	make -C tests test

zarith.cma: $(MLSRC:%.ml=%.cmo)
	$(OCAMLMKLIB) -failsafe -o zarith $+ $(LIBS)

zarith.cmxa zarith.$(LIBSUFFIX): $(MLSRC:%.ml=%.cmx)
	$(OCAMLMKLIB) -failsafe -o zarith $+ $(LIBS)

zarith.cmxs: zarith.cmxa libzarith.$(LIBSUFFIX)
	$(OCAMLOPT) -shared -o $@ -I . zarith.cmxa -linkall

libzarith.$(LIBSUFFIX) dllzarith.$(DLLSUFFIX): $(SSRC:%.S=%.$(OBJSUFFIX)) $(CSRC:%.c=%.$(OBJSUFFIX)) 
	$(OCAMLMKLIB) -failsafe -o zarith $+ $(LIBS)

zarith_top.cma: zarith_top.cmo
	$(OCAMLC) -o $@ -a $<

doc: $(MLISRC)
	mkdir -p html
	$(OCAMLDOC) -html -d html -charset utf8 $+



# install targets
#################

ifeq ($(INSTMETH),install)
install:
	install -d $(INSTALLDIR) $(INSTALLDIR)/zarith $(INSTALLDIR)/stublibs
	for i in $(TOINSTALL); do \
		if test -f $$i; then $(INSTALL) -m 0644 $$i $(INSTALLDIR)/zarith/$$i; fi; \
	done
	if test -f dllzarith.$(DLLSUFFIX); then $(INSTALL) -m 0755 dllzarith.$(DLLSUFFIX) $(INSTALLDIR)/stublibs/dllzarith.$(DLLSUFFIX); fi

uninstall:
	for i in $(TOINSTALL); do \
		rm -f $(INSTALLDIR)/zarith/$$i; \
	done
	if test -f $(INSTALLDIR)/stublibs/dllzarith.$(DLLSUFFIX); then rm -f $(INSTALLDIR)/stublibs/dllzarith.$(DLLSUFFIX); fi
endif

ifeq ($(INSTMETH),findlib)
install:
	$(OCAMLFIND) install -destdir "$(INSTALLDIR)" zarith META $(TOINSTALL) -optional dllzarith.$(DLLSUFFIX)

uninstall:
	$(OCAMLFIND) remove -destdir "$(INSTALLDIR)" zarith
endif


# rules
#######

$(AUTOGEN): z.mlp z.mlip $(SSRC) z_pp.pl
	./z_pp.pl $(ARCH)

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $<

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $<

%.$(OBJSUFFIX): %.c
	$(OCAMLC) -ccopt "$(CFLAGS)" -c $<

clean:
	/bin/rm -rf *.$(OBJSUFFIX) *.$(LIBSUFFIX) *.$(DLLSUFFIX) *.cmi *.cmo *.cmx *.cmxa *.cmxs *.cma  *.cmt *.cmti *~ \#* depend test $(AUTOGEN) tmp.c depend
	make -C tests clean

depend: $(AUTOGEN)
	$(OCAMLDEP) -native $(OCAMLINC) $(MLSRC) $(MLISRC) > depend

include depend

$(CSRC:%.c=%.$(OBJSUFFIX)): z_features.h zarith.h

.PHONY: clean
.PHONY: tests
