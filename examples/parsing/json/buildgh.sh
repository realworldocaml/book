#!/bin/sh

atdgen -t github_org.atd
atdgen -j github_org.atd
ocamlbuild -use-ocamlfind github_org_info.native
./github_org_info.native janestreet
