#!/bin/sh

ocamlbuild -use-ocamlfind -tag debug -package core -package core_bench -tag thread func_alloc.native
ocamlbuild -use-ocamlfind -tag debug -package core -package core_bench -tag thread bench_poly_and_mono.native
ocamlbuild -use-ocamlfind -tag debug -package core -package core_bench -tag thread pattern.native
ocamlbuild -use-ocamlfind -tag debug -package core -package core_bench -tag thread alloc_compare.native
ocamlbuild -use-ocamlfind -tag debug -package core -package core_bench -tag thread barrier.native
