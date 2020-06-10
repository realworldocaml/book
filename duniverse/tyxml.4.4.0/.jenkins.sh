export ALCOTEST_SHOW_ERRORS=true

opam pin add --no-action tyxml .
opam pin add --no-action tyxml-ppx .

opam install -t --deps-only tyxml
opam install -t --verbose tyxml
opam install -t --verbose tyxml-ppx

do_build_doc () {
  make wikidoc
  cp -Rf doc/manual-wiki/*.wiki ${MANUAL_SRC_DIR}
  cp -Rf _build/tyxml-api.wikidocdir/*.wiki ${API_DIR}
}

do_remove () {
  opam remove --verbose tyxml
}
