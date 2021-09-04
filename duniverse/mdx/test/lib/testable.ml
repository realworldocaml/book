let sexp =
  let pp fmt s = Fmt.pf fmt "%s" (Mdx.Util.Csexp.to_string s) in
  Alcotest.testable pp ( = )

let msg =
  let pp fs = function `Msg s -> Fmt.pf fs "`Msg %S" s in
  Alcotest.testable pp ( = )

let ocaml_delimiter =
  let open Mdx.Ocaml_delimiter in
  let pp fs = function
    | Part_begin (src, { indent; payload }) ->
        Fmt.string fs "Part_begin";
        (match src with
        | Cmt -> Fmt.string fs "Cmt"
        | Attr -> Fmt.string fs "Attr");
        Fmt.fmt "indent:%s" fs indent;
        Fmt.fmt "payload:%s" fs payload
    | Part_end -> Fmt.string fs "Part_end"
  in
  Alcotest.testable pp ( = )

let library_set =
  let open Mdx.Library in
  let equal = Set.equal in
  let pp fmt set =
    let l = Set.elements set in
    Fmt.string fmt "[ ";
    Fmt.(list ~sep:(const string "; ") pp) fmt l;
    Fmt.string fmt " ]"
  in
  Alcotest.testable pp equal

let block = Alcotest.testable Mdx.Block.dump ( = )

let header = Alcotest.testable Mdx.Block.Header.pp ( = )
