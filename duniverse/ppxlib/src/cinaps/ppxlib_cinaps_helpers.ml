open Re

let str_to_sig =
  let re =
    Str.regexp
      {|\(_?[sS]tructure\|impl\(ementation\)?\|str_\|_str\|\b\(st\|Str\)\b\)|}
  in
  let map s =
    match Str.matched_string s with
    | "st" -> "sg"
    | "Str" -> "Sig"
    | "structure" -> "signature"
    | "Structure" -> "Signature"
    | "_structure" -> "_signature"
    | "_Structure" -> "_Signature"
    | "str_" -> "sig_"
    | "_str" -> "_sig"
    | "implementation" -> "interface"
    | "impl" -> "intf"
    | _ -> assert false
  in
  fun s -> print_string (Str.global_substitute re map s)
