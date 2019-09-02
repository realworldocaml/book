open StdLabels

let units fn =
  (* The cma format is documented in typing/cmo_format.mli in the compiler sources *)
  let ic = open_in_bin fn in
  let len_magic_number = String.length Config.cma_magic_number in
  let magic_number = really_input_string ic len_magic_number in
  assert (magic_number = Config.cma_magic_number);
  let toc_pos = input_binary_int ic in
  seek_in ic toc_pos;
  let toc = (input_value ic : Cmo_format.library) in
  close_in ic;

  List.map toc.lib_units ~f:(fun cu -> cu.Cmo_format.cu_name)
  |> List.sort ~cmp:String.compare
