#!/bin/sh

file=$1
lib=cow.syntax
bin=ocamlfind
args=`$bin query -predicates syntax,preprocessor -r -format '-I %d %a' $lib`
camlp4o -printer o $args $file

