#!/bin/sh -ex

mkdir -p _build
opam config -env > _build/env.sh
. ./_build/env.sh
ocamlbuild -tag short_paths -j 4 -cflags "-w @A-4-33-44" -use-ocamlfind get_chapter_files.native transform_pandocbook.native html_code_highlight.native dump_paragraph_fragments.native transform_markdown.native run_core_toplevel.byte syntax_highlight_code.native
