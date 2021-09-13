module Location = struct
  open Location

  let print_loc ppf loc =
    (*setup_colors ();*)
    let file_valid = function
      | "_none_" ->
          (* This is a dummy placeholder, but we print it anyway to please editors
             that parse locations in error messages (e.g. Emacs). *)
          true
      | "" | "//toplevel//" -> false
      | _ -> true
    in
    let line_valid line = line > 0 in
    let file =
      (* According to the comment in location.mli, if [pos_fname] is "", we must
         use [!input_name]. *)
      if loc.loc_start.pos_fname = "" then !input_name
      else loc.loc_start.pos_fname
    in
    let startline = loc.loc_start.pos_lnum in
    let endline = loc.loc_end.pos_lnum in
    let first = ref true in
    let capitalize s =
      if !first then (
        first := false;
        String.capitalize_ascii s)
      else s
    in
    let comma () = if !first then () else Format.fprintf ppf ", " in
    Format.fprintf ppf "@{<loc>";
    if file_valid file then
      Format.fprintf ppf "%s \"%a\"" (capitalize "file") print_filename file;
    (* Print "line 1" in the case of a dummy line number. This is to please the
       existing setup of editors that parse locations in error messages (e.g.
       Emacs). *)
    comma ();
    let startline = if line_valid startline then startline else 1 in
    let endline = if line_valid endline then endline else startline in
    if startline = endline then
      Format.fprintf ppf "%s %i" (capitalize "line") startline
    else Format.fprintf ppf "%s %i-%i" (capitalize "lines") startline endline;
    Format.fprintf ppf "@}"
end
