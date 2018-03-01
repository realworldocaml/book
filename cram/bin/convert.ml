let file = Sys.argv.(1)

let inputs =
  let ic = open_in file in
  let r = ref [] in
  try while true do r := input_line ic :: !r done; assert false
  with End_of_file ->
    close_in ic;
    List.rev !r

let outputs =
  let l =
    List.map (fun str ->
        if String.length str < 2 then str
        else match String.sub str 0 2 with
          | "  " -> str
          | _     -> "  $ " ^ str
      ) inputs
  in
  let oc = open_out file in
  List.iter (fun str ->
      output_string oc (str ^ "\n")
    ) l;
  close_out oc
