open Migrate_parsetree

let () =
  Driver.register
    ~name:"tyxml" Versions.ocaml_408
    Tyxml_ppx.mapper
