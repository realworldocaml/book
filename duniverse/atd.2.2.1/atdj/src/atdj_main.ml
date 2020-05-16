(* Main *)

open Atd.Import
open Atdj_env

let args_spec env = Arg.align
    [ "-package",
      Arg.String (fun x -> env := { !env with package = x }),
      " Package name of generated files"
    ]

let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <options> <file>\nOptions are:"

let make_package_dirs package =
  let re   = Re.Str.regexp "\\." in
  let dirs = Re.Str.split re package in
  List.fold_left
    (fun parent dir ->
       let full_dir = parent ^ "/" ^ dir in
       if Sys.file_exists full_dir then
         if not (Sys.is_directory full_dir) then
           failwith (
             sprintf "Cannot make directory %s: file already exists" full_dir
           )
         else ()
       else
         Unix.mkdir full_dir 0o755;
       full_dir
    )
    "." dirs

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
  let re = Re.Str.regexp "^[a-zA-Z0-9]+\\(\\.[a-zA-Z0-9]+\\)*$" in
  if not (Re.Str.string_match re env.package 0) then (
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

  (* Create package directories *)
  let env = { env with package_dir = make_package_dirs env.package } in

  (* Generate classes from ATD definition *)
  let env = Atdj_trans.trans_module env atd_module in

  (* Output helper classes *)
  Atdj_helper.output_util env;
  Atdj_helper.output_atdj env;

  Atdj_helper.output_package_javadoc env atd_head


let () =
  try main ()
  with Atd.Ast.Atd_error s as e ->
    eprintf "Error:\n%s\n" s;
    raise e
