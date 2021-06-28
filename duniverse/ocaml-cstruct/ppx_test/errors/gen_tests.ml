let output_stanzas name =
  Printf.printf
    {|
(rule
  (deps pp.exe (:input %s))
  (targets %s.errors)
  (action
    (with-stderr-to
      %%{targets}
      (run ./pp.exe --impl %%{input}))))

(alias
  (name runtest)
  (package ppx_cstruct)
  (action
    (diff %s.expected %s.errors)))
|}
    name name name name

let is_test = function
  | "pp.ml" -> false
  | "pp.pp.ml" -> false
  | "gen_tests.ml" -> false
  | e -> Filename.check_suffix e ".ml"

let () =
  Sys.readdir "."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter is_test
  |> List.iter output_stanzas
