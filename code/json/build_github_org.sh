atdgen -t github_org.atd
atdgen -j github_org.atd
ocamlbuild -use-ocamlfind -tag thread -pkg core_extended,yojson,atdgen github_org_info.native
