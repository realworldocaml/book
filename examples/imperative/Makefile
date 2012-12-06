OCAMLOPT = ocamlopt
OCAMLOPTFLAGS =\
	-thread

X=\
	-I $(HOME)/.opam/system/lib/core\
	-I $(HOME)/.opam/system/lib/async\
	-I $(HOME)/.opam/system/lib/async_extra\
	-I $(HOME)/.opam/system/lib/async_unix\
	-I $(HOME)/.opam/system/lib/async_core\
	-I $(HOME)/.opam/system/lib/variantslib\
	-I $(HOME)/.opam/system/lib/bin_prot\
	-I $(HOME)/.opam/system/lib/sexplib\
	-I $(HOME)/.opam/system/lib/res\
	-I $(HOME)/.opam/system/lib/fieldslib

OCAMLLIBS = unix.cmxa threads.cmxa

Y = nums.cmxa bigarray.cmxa variantslib.cmxa bin_prot.cmxa sexplib.cmxa res.cmxa fieldslib.cmxa core.cmxa async_core.cmxa async_unix.cmxa async_extra.cmxa async.cmxa

all: hashmap.cmx dlist.cmx lazy.cmx memo.cmx hashcons.cmx race2

race2: race2.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $(OCAMLLIBS) $< 

%.cmx %.cmi: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $*.ml

hashcons.cmx: hashmap.cmi
memo.cmx: hashmap.cmi
hashmap.cmx: dlist.cmi
