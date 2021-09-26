#!/bin/bash

dune build @docgen

git checkout gh-pages

odoc html-generate -o . _build/default/doc/odoc_parser.odocl --indent
odoc html-generate -o . _build/default/doc/page-index.odocl --indent
odoc html-generate -o . _build/default/doc/page-contributing.odocl --indent
odoc support-files -o .

