open Sexplib

let%test_unit _ =
  let orig_sexps =
    Random.init 10;
    let a =
      Array.init 5 (fun i ->
        Array.init 10 (fun j ->
          String.init (i + j) (fun k ->
            Char.chr (Char.code 'a' + k))))
    in
    let open Sexplib.Conv in
    match [%sexp_of: string array array] a with
    | List l -> l
    | Atom _ -> assert false
  in
  let hum_file = Filename.temp_file "hum" ".sexp" in
  let hum_oc = open_out hum_file in
  let hum_ppf = Format.formatter_of_out_channel hum_oc in
  List.iter
    (fun sexp -> Format.fprintf hum_ppf "%a@\n" Sexp.pp_hum sexp)
    orig_sexps;
  Format.pp_print_flush hum_ppf ();
  close_out hum_oc;

  let mach_file = Filename.temp_file "mach" ".sexp" in
  let mach_oc = open_out mach_file in
  List.iter
    (fun sexp -> Printf.fprintf mach_oc "%a\n" Sexp.output_mach sexp)
    orig_sexps;
  close_out mach_oc;

  let hum_ic = open_in hum_file in
  let hum_sexps = Sexp.input_sexps hum_ic in
  close_in hum_ic;
  assert (hum_sexps = orig_sexps);
  Sys.remove hum_file;

  let mach_ic = open_in mach_file in
  let mach_sexps = Sexp.input_sexps mach_ic in
  close_in mach_ic;
  assert (mach_sexps = orig_sexps);
  Sys.remove mach_file;
;;
