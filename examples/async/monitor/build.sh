#!/usr/bin/env bash

eval `opam config -env`

ocamlbuild -use-ocamlfind monitor_handle_errors.byte monitor_try_with.byte

