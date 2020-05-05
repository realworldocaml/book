(* Main *)

open Atd.Import
open Atds_env

let args_spec env = Arg.align
    [ "-package",
      Arg.String (fun x -> env := { !env with package = x }),
      " Package name of generated files";
      "-o",
      Arg.String (fun x -> env := { !env with output = open_out x }),
      " File name for Scala output"
    ]

let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> <file>\nOptions are:"

let main () =
  Printexc.record_backtrace true;
  let env = ref default_env in

  (* Parse command line options *)
  let args_spec' = args_spec env in
  Arg.parse args_spec'
    (fun x -> env := { !env with input_file = Some x }) usage_msg;
  let env = !env in

  (* Check for input file *)
  let input_file =
    match env.input_file with
      | None   ->
          prerr_endline "No input file specified";
          Arg.usage args_spec' usage_msg;
          exit 1
      | Some x ->
          if not (Filename.check_suffix x ".atd") then (
            prerr_endline "Input filename must end with `.atd'";
            Arg.usage args_spec' usage_msg;
            exit 1
          ) else x in

  (* Validate package name *)
  let re = Str.regexp "^[a-zA-Z0-9]+\\(\\.[a-zA-Z0-9]+\\)*$" in
  if not (Str.string_match re env.package 0) then (
    prerr_endline "Invalid package name";
    Arg.usage args_spec' usage_msg;
    exit 1
  );

  (* Parse ATD file *)
  let (atd_head, atd_module), _original_types =
    Atd.Util.load_file
      ~expand:false ~inherit_fields:true ~inherit_variants:true input_file
  in
  let env = {
    env with
    module_items =
      List.map
        (function (Atd.Ast.Type (_, (name, _, _), atd_ty)) -> (name, atd_ty))
        atd_module
  } in

  let close_package = Atds_trans.open_package env in

  (* Output helper classes *)
  Atds_helper.output_atds env;

  (* Generate classes from ATD definition *)
  let _ = Atds_trans.trans_module env atd_module in

  close_package()

let () =
  try main ()
  with Atd.Ast.Atd_error s as e ->
    eprintf "Error:\n%s\n" s;
    raise e
