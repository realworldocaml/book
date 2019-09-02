#! /bin/sh

echo "Running script $0, look inside for comments."

# Exit on error
set -e

# Produce format_v1.mli and format_v1.ml from type definition
atdgen format_v1.atd

# Produce format_v2.mli and format_v2.ml from type definition
atdgen format_v2.atd

# Compile and link all OCaml code, producing upgrade_demo
ocamlfind ocamlopt -g -dtypes -package atdgen -linkpkg \
  format_v1.mli format_v1.ml \
  format_v2.mli format_v2.ml \
  upgrade_demo.ml -o upgrade_demo

# Save biniou sample in the old format
./upgrade_demo old > old_sample.dat

# Save the same data after conversion to the new format
./upgrade_demo new > new_sample.dat

# Use our sample data in the old format for the next test
cp old_sample.dat old_data.dat

# Read data in the old format with code assuming the new format
./upgrade_demo up < old_data.dat > new_data.dat

# Dump a text representation of old and new data.
# The -w option specifies a list of candidate field names required for
# converting hashed field names into the original names.
echo "Data in format v1:"
bdump old_data.dat -w a,b,c,d
echo "Converted to format v2:"
bdump new_data.dat -w a,c,d,e
echo "Same, displayed using incomplete name dictionary:"
bdump new_data.dat -w a,b
