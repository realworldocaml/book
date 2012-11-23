let xml_id i o =
  let rec pull i o depth =
    Xmlm.output o (Xmlm.peek i);
    match Xmlm.input i with
    | `El_start _ -> pull i o (depth + 1)
    | `El_end -> if depth > 1 then pull i o (depth - 1)
    | `Data _ -> pull i o depth
    | `Dtd _ -> assert false
  in
  Xmlm.output o (Xmlm.input i); (* `Dtd *)
  pull i o 0

let _ =
  let i = Xmlm.make_input (`Channel (open_in "ddg.xml")) in
  let o = Xmlm.make_output (`Channel stdout) in
  xml_id i o
