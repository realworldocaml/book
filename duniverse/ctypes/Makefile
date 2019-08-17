.SECONDEXPANSION:

BEST:=$(shell if ocamlopt > /dev/null 2>&1; then echo native; else echo byte; fi)
OPAQUE:=$(shell if ocamlopt -opaque 2>/dev/null; then echo -opaque; fi) 
NO_KEEP_LOCS:=$(shell if ocamlopt -no-keep-locs 2>/dev/null; then echo -no-keep-locs; fi) 
DEBUG=true
COVERAGE=false
OCAML=ocaml
OCAMLFIND=ocamlfind $(OCAMLFINDFLAGS)
HOSTOCAMLFIND=$(OCAMLFIND)
OCAMLDEP=$(OCAMLFIND) ocamldep
OCAMLMKLIB=$(OCAMLFIND) ocamlmklib
VPATH=src examples
BUILDDIR=_build
BASE_PROJECTS=configure libffi-abigen configured ctypes ctypes-top
FOREIGN_PROJECTS=test-libffi ctypes-foreign-base ctypes-foreign-threaded ctypes-foreign-unthreaded
STUB_PROJECTS=cstubs
PROJECTS=$(BASE_PROJECTS) $(FOREIGN_PROJECTS) $(STUB_PROJECTS)
DEP_DIRS=$(foreach project,$(PROJECTS),$($(project).dir))
GENERATED=src/ctypes/ctypes_primitives.ml	\
          src/ctypes-foreign-base/libffi_abi.ml \
          src/ctypes-foreign-base/dl.ml		\
          src/ctypes-foreign-base/dl_stubs.c	\
          libffi.config				\
          asneeded.config                       \
          discover				\
          gen_c_primitives			\
          gen_c_primitives.log			\
          gen_libffi_abi			\
	  src/configure/extract_from_c.cmi	\
	  src/configure/extract_from_c.cmo	\
	  src/configure/gen_c_primitives.cmi	\
	  src/configure/gen_c_primitives.cmo	\
	  src/configure/gen_libffi_abi.cmi	\
	  src/configure/gen_libffi_abi.cmo	\
	  src/discover/commands.cmi		\
	  src/discover/commands.cmo		\
	  src/discover/discover.cmi		\
	  src/discover/discover.cmo

OCAML_FFI_INCOPTS=$(libffi_opt)
export CFLAGS DEBUG

EXTDLL:=$(shell $(OCAMLFIND) ocamlc -config | awk '/^ext_dll:/{print $$2}')
OSYSTEM:=$(shell $(OCAMLFIND) ocamlc -config | awk '/^system:/{print $$2}')

ifneq (,$(filter mingw%,$(OSYSTEM)))
OS_ALT_SUFFIX=.win
else
OS_ALT_SUFFIX=.unix
endif

# public targets
all: libffi.config $(PROJECTS)

ctypes-base: $(BASE_PROJECTS)
ctypes-foreign: ctypes-base $(FOREIGN_PROJECTS)
ctypes-stubs: ctypes-base $(STUB_PROJECTS)

clean: clean-examples clean-tests
	rm -fr _build
	rm -f $(GENERATED)

# ctypes subproject
ctypes.cmi_only = ctypes_static ctypes_primitive_types ctypes_structs cstubs_internals
ctypes.public = lDouble complexL ctypes posixTypes ctypes_types
ctypes.dir = src/ctypes
ctypes.extra_mls = ctypes_primitives.ml
ctypes.deps = bigarray bytes integers
ctypes.linkdeps = integers_stubs
ctypes.install = yes
ctypes.install_native_objects = yes
ifeq ($(XEN),enable)
ctypes.xen = yes
endif

ctypes: PROJECT=ctypes
ctypes: $(ctypes.dir)/$(ctypes.extra_mls) $$(LIB_TARGETS)

# cstubs subproject
cstubs.public = cstubs_structs cstubs cstubs_inverted
cstubs.dir = src/cstubs
cstubs.subproject_deps = ctypes
cstubs.deps = str bytes integers
cstubs.install = yes
cstubs.install_native_objects = yes
cstubs.extra_hs = $(package_integers_path)/ocaml_integers.h

cstubs: PROJECT=cstubs
cstubs: $(cstubs.dir)/$(cstubs.extra_mls) $$(LIB_TARGETS)

# ctypes-foreign-base subproject
ctypes-foreign-base.public = dl libffi_abi
ctypes-foreign-base.install = yes
ctypes-foreign-base.install_native_objects = yes
ctypes-foreign-base.threads = no
ctypes-foreign-base.dir = src/ctypes-foreign-base
ctypes-foreign-base.deps = bytes integers
ctypes-foreign-base.subproject_deps = ctypes
ctypes-foreign-base.extra_mls = libffi_abi.ml dl.ml
ctypes-foreign-base.extra_cs = dl_stubs.c
ctypes-foreign-base.link_flags = $(libffi_lib) $(lib_process)
ctypes-foreign-base.cmo_opts = $(OCAML_FFI_INCOPTS:%=-ccopt %)
ctypes-foreign-base.cmx_opts = $(OCAML_FFI_INCOPTS:%=-ccopt %)

ctypes-foreign-base: PROJECT=ctypes-foreign-base
ctypes-foreign-base: $$(LIB_TARGETS)

# ctypes-foreign-threaded subproject
ctypes-foreign-threaded.public = foreign
ctypes-foreign-threaded.install = yes
ctypes-foreign-threaded.threads = yes
ctypes-foreign-threaded.dir = src/ctypes-foreign-threaded
ctypes-foreign-threaded.subproject_deps = ctypes ctypes-foreign-base
ctypes-foreign-threaded.link_flags = $(libffi_lib) $(lib_process)
ctypes-foreign-threaded.cmo_opts = $(OCAML_FFI_INCOPTS:%=-ccopt %)
ctypes-foreign-threaded.cmx_opts = $(OCAML_FFI_INCOPTS:%=-ccopt %)
ctypes-foreign-threaded.cmi_opts = $(OPAQUE) $(NO_KEEP_LOCS)
ctypes-foreign-threaded.install_native_objects = no

ctypes-foreign-threaded: PROJECT=ctypes-foreign-threaded
ctypes-foreign-threaded: $$(LIB_TARGETS)

# ctypes-foreign-unthreaded subproject
ctypes-foreign-unthreaded.public = foreign
ctypes-foreign-unthreaded.install = yes
ctypes-foreign-unthreaded.threads = no
ctypes-foreign-unthreaded.dir = src/ctypes-foreign-unthreaded
ctypes-foreign-unthreaded.subproject_deps = ctypes ctypes-foreign-base
ctypes-foreign-unthreaded.link_flags = $(libffi_lib) $(lib_process)
ctypes-foreign-unthreaded.cmo_opts = $(OCAML_FFI_INCOPTS:%=-ccopt %)
ctypes-foreign-unthreaded.cmx_opts = $(OCAML_FFI_INCOPTS:%=-ccopt %)
ctypes-foreign-unthreaded.cmi_opts = $(OPAQUE) $(NO_KEEP_LOCS)
ctypes-foreign-unthreaded.install_native_objects = no

ctypes-foreign-unthreaded: PROJECT=ctypes-foreign-unthreaded
ctypes-foreign-unthreaded: $$(LIB_TARGETS)

# ctypes-top subproject
ctypes-top.public = ctypes_printers
ctypes-top.dir = src/ctypes-top
ctypes-top.install = yes
ctypes-top.deps = compiler-libs integers
ctypes-top.subproject_deps = ctypes
ctypes-top.install_native_objects = yes

ctypes-top: PROJECT=ctypes-top
ctypes-top: $$(LIB_TARGETS)

# configuration
configured: src/ctypes/ctypes_primitives.ml src/ctypes-foreign-base/libffi_abi.ml src/ctypes-foreign-base/dl.ml src/ctypes-foreign-base/dl_stubs.c

src/ctypes-foreign-base/dl.ml: src/ctypes-foreign-base/dl.ml$(OS_ALT_SUFFIX)
	cp $< $@
src/ctypes-foreign-base/dl_stubs.c: src/ctypes-foreign-base/dl_stubs.c$(OS_ALT_SUFFIX)
	cp $< $@

src/ctypes/ctypes_primitives.ml: src/configure/extract_from_c.ml src/configure/gen_c_primitives.ml
	$(HOSTOCAMLFIND) ocamlc -o gen_c_primitives -package str,bytes -linkpkg $^ -I src/configure
	./gen_c_primitives > $@ 2> gen_c_primitives.log || (rm $@ && cat gen_c_primitives.log || false)

src/ctypes-foreign-base/libffi_abi.ml: src/configure/extract_from_c.ml src/configure/gen_libffi_abi.ml
	$(HOSTOCAMLFIND) ocamlc -o gen_libffi_abi -package str,bytes -linkpkg $^ -I src/configure
	./gen_libffi_abi > $@ 2> gen_c_primitives.log || (rm $@ && cat gen_c_primitives.log || false)

libffi.config: src/discover/commands.mli src/discover/commands.ml src/discover/discover.ml
	$(HOSTOCAMLFIND) ocamlc -o discover -package str,bytes -linkpkg $^ -I src/discover
	./discover -ocamlc "$(OCAMLFIND) ocamlc" > $@ || (rm $@ && false)

asneeded.config:
	./src/discover/determine_as_needed_flags.sh >> $@

# dependencies
depend: configure
	$(OCAMLDEP) -one-line $(foreach dir,$(DEP_DIRS),-I $(dir)) \
            $(shell find src examples -name '*.mli' -o -name '*.ml') \
           | sed "s!src/!_build/src/!g; s!examples/!_build/examples/!g" | sort > .depend

#installation
META-install:
	$(OCAMLFIND) install ctypes META CHANGES.md

install-%: PROJECT=$*
install-%:
	$(if $(filter yes,$($(PROJECT).install)),\
		$(OCAMLFIND) install -add ctypes -optional $^ \
                   $(LIB_TARGETS) $(LIB_TARGET_EXTRAS) \
                   $(INSTALL_MLIS) $(INSTALL_CMIS) \
                   $(INSTALL_CMTS) $(INSTALL_CMTIS) \
                   $(INSTALL_HEADERS) \
                   $(if $(filter yes,$($(PROJECT).install_native_objects)),$(NATIVE_OBJECTS)))

install: META-install $(PROJECTS:%=install-%)

uninstall:
	$(OCAMLFIND) remove ctypes

DOCFILES=$(foreach project,$(PROJECTS),\
           $(foreach mli,$($(project).public),\
            $($(project).dir)/$(mli).mli))
DOCFLAGS=-I $(shell ocamlfind query integers) $(foreach project,$(PROJECTS),-I $(BUILDDIR)/$($(project).dir))
# Avoid passing duplicate interfaces to ocamldoc.
DOCFILES:=$(filter-out src/ctypes-foreign-threaded/foreign.mli,$(DOCFILES))

doc:
	ocamldoc -html $(DOCFLAGS) $(DOCFILES)


.PHONY: depend clean configure all install doc $(PROJECTS)

include .depend Makefile.rules Makefile.examples Makefile.tests
-include libffi.config
-include asneeded.config

ifeq ($(libffi_available),false)
test-libffi:
	@echo "The following required C libraries are missing: libffi."
	@echo "Please install them and retry. If they are installed in a non-standard location"
	@echo "or need special flags, set the environment variables <LIB>_CFLAGS and <LIB>_LIBS"
	@echo "accordingly and retry."
	@echo
	@echo " For example, if libffi is installed in /opt/local, you can type:"
	@echo
	@echo "   export LIBFFI_CFLAGS=-I/opt/local/include"
	@echo "   export LIBFFI_LIBS=\"-L/opt/local/lib -lffi\""
	@exit 1
else:
test-libffi:
endif
