module J = Yojson.Safe

let () =
  let j = J.from_file "sample.json" in
  Format.printf "%a@." (J.pretty_print ?std:None) j;
  ()

