#!/bin/bash

nasm -felf64 asm_sleep.S
ocamlopt -c test_branch.ml
ocamlopt asm_sleep.o time.c test_branch.ml -o test_branch
