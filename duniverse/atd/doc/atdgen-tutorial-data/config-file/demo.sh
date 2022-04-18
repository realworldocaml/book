#! /bin/sh -e

set -x

# Embed the contents of the .atd file into our OCaml program
echo 'let contents = "\' > config_atd.ml
sed -e 's/\([\\"]\)/\\\1/g' config.atd >> config_atd.ml
echo '"' >> config_atd.ml

# Derive OCaml type definitions from .atd file
atdgen -t config.atd

# Derive JSON-related functions from .atd file
atdgen -j -j-defaults -j-strict-fields config.atd

# Derive validator from .atd file
atdgen -v config.atd

# Compile the OCaml program
ocamlfind ocamlopt -o config \
  config_t.mli config_t.ml config_j.mli config_j.ml config_v.mli config_v.ml \
  config_atd.ml config.ml -package atdgen -linkpkg

# Output a sample config
./config -template

# Print the original type definitions
./config -format

# Fail to validate an invalid config file
./config -validate bad-config1.json || :

# Fail to validate another invalid config file (using custom validators)
./config -validate bad-config3.json || :

# Validate, inject missing defaults and pretty-print
./config -validate sample-config.json
