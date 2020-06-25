ppx_tools
=========

Tools for authors of syntactic tools (such as ppx rewriters).

This package is licensed by LexiFi under the terms of the MIT license.

The tools are installed as a findlib package called 'ppx_tools'.
Executables are thus accessible through the ocamlfind driver (e.g.:
ocamlfind ppx_tools/dumpast).

Main contributors:

  - Alain Frisch
  - Peter Zotov (whitequark)
  - Gabriel Radanne (Drup)

[![Build Status](https://img.shields.io/travis/ocaml-ppx/ppx_tools_versioned?label=travis)](https://travis-ci.org/ocaml-ppx/ppx_tools_versioned)

ppx_metaquot
------------

A ppx filter to help writing programs which manipulate the Parsetree,
by allowing the programmer to use concrete syntax for expressions
creating Parsetree fragments and patterns deconstructing Parsetree
fragments.  See the top of ppx_metaquot.ml for a description of the
supported extensions.

Usage:

    ocamlfind ocamlc -c -package ppx_tools.metaquot my_ppx_code.ml


Ast_mapper_class
----------------

This module implements an API similar to Ast_mapper from the
compiler-libs, i.e. a generic mapper from Parsetree to Parsetree
implemeting a deep identity copy, which can be customized with a
custom behavior for each syntactic category.  The difference with
Ast_mapper is that Ast_mapper_class implements the open recursion
using a class.
