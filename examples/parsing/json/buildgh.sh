#!/bin/sh

curl -o js_org.json https://api.github.com/orgs/janestreet 
atdgen -t github_org.atd
atdgen -j github_org.atd
ocamlbuild -use-ocamlfind github_org_info.native
./github_org_info.native
