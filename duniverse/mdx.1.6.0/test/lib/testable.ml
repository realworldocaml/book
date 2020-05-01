let sexp =
  let rec pp fmt s =
    match (s : Mdx.Util.Sexp.t) with
    | Atom s -> Fmt.pf fmt "Atom %S" s
    | List l ->
      let sep fmt () = Fmt.pf fmt "; " in
      Fmt.pf fmt "List [%a]" Fmt.(list ~sep pp) l
  in
  Alcotest.testable pp Mdx.Util.Sexp.equal
