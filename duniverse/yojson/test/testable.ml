let yojson = Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal

let variant_kind_pp fmt = function
  | `Edgy_bracket -> Format.fprintf fmt "`Edgy_bracket"
  | `Square_bracket -> Format.fprintf fmt "`Square_bracket"
  | `Double_quote -> Format.fprintf fmt "`Double_quote"

let variant_kind_equal a b =
  match a, b with
  | `Edgy_bracket, `Edgy_bracket -> true
  | `Square_bracket, `Square_bracket -> true
  | `Double_quote, `Double_quote -> true
  | _ -> false

let variant_kind = Alcotest.testable variant_kind_pp variant_kind_equal
