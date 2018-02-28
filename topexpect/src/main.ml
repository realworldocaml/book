module Linked = struct
  include (Topdirs : sig end)
  include (Ephemeron : sig end)
  include (Uchar : sig end)
  include (Condition : sig end)
end

open Parsetree
open Sexplib.Conv
open Ocaml_topexpect

(* Standard outputs should be disable: multi threaded code could print
 * at anytime, so we just disable output by defaul. *)

let stdout_backup = Unix.out_channel_of_descr (Unix.dup Unix.stdout)
let stderr_backup = Unix.out_channel_of_descr (Unix.dup Unix.stderr)

let prerr_endline str =
  output_string stderr_backup str;
  output_char stderr_backup '\n';
  flush stderr_backup

let disable_outputs = lazy (
  let fd_out = Unix.openfile "/dev/null" Unix.[O_WRONLY] 0o600 in
  Unix.dup2 fd_out Unix.stdout;
  Unix.dup2 fd_out Unix.stderr;
  Unix.close fd_out;
)

let verbose = ref false
let () = Hashtbl.add Toploop.directive_table "verbose"
    (Toploop.Directive_bool (fun x -> verbose := x))
let silent = ref false
let () = Hashtbl.add Toploop.directive_table "silent"
    (Toploop.Directive_bool (fun x -> silent := x))
let verbose_findlib = ref false

module Async_autorun = struct
  (* Inspired by Utop auto run rewriter *)
  let (async_typ, async_runner, async_rewrite) =
    let typ = Longident.parse "Async.Deferred.t" in
    let runner = Longident.parse "Async.Thread_safe.block_on_async_exn" in
    let open Ast_helper in
    let rewrite loc e =
      let punit =
        Pat.construct (Location.mkloc (Longident.Lident "()") loc) None in
      with_default_loc loc @@ fun () ->
      Exp.apply
        (Exp.ident (Location.mkloc runner loc))
        [(Asttypes.Nolabel, Exp.fun_ Asttypes.Nolabel None punit e)]
    in
    (typ, runner, rewrite)

  let normalize_type_path env path =
    match Env.find_type path env with
    | { Types.type_manifest = Some ty; _ } -> begin
        match Ctype.expand_head env ty with
        | { Types.desc = Types.Tconstr (path, _, _); _ } -> path
        | _ -> path
      end
    | _ -> path

  let is_persistent_value env longident =
    let rec is_persistent_path = function
      | Path.Pident id -> Ident.persistent id
      | Path.Pdot (p, _, _) -> is_persistent_path p
      | Path.Papply (_, p) -> is_persistent_path p
    in
    try is_persistent_path (fst (Env.lookup_value longident env))
    with Not_found -> false

  let rewrite_item env async_typ pstr_item tstr_item =
    match pstr_item.Parsetree.pstr_desc, tstr_item.Typedtree.str_desc with
    | (Parsetree.Pstr_eval (e, _),
       Typedtree.Tstr_eval ({ Typedtree.exp_type = typ; _ }, _)) ->
      begin match (Ctype.repr typ).Types.desc with
        | Types.Tconstr (path, _, _) when
            Path.same async_typ (normalize_type_path env path) ->
          let loc = pstr_item.Parsetree.pstr_loc in
          { Parsetree.pstr_desc = Parsetree.Pstr_eval (async_rewrite loc e, []);
            Parsetree.pstr_loc = loc }
        | _ -> pstr_item
      end
    | _ -> pstr_item

  let rewrite_phrase =
    let is_eval = function
      | { pstr_desc = Pstr_eval _; _ } -> true
      | _ -> false
    in
    function
    | Ptop_def pstr when List.exists is_eval pstr
                      && is_persistent_value !Toploop.toplevel_env async_runner ->
      Env.reset_cache_toplevel ();
      let snap = Btype.snapshot () in
      let pstr =
        try
          let env = !Toploop.toplevel_env in
          let path = normalize_type_path env (Env.lookup_type async_typ env) in
          let tstr, _tsg, env =
            Typemod.type_structure !Toploop.toplevel_env pstr Location.none in
          List.map2 (rewrite_item env path) pstr tstr.Typedtree.str_items
        with _ ->
          pstr
      in
      Btype.backtrack snap;
      Ptop_def pstr
    | phrase -> phrase
end

let toplevel_exec_phrase ppf p = match Phrase.result p with
  | Error exn -> raise exn
  | Ok phrase ->
    Warnings.reset_fatal ();
    let mapper = Lexbuf.position_mapper (Phrase.start p) in
    let phrase = match phrase with
      | Ptop_def str -> Ptop_def (mapper.Ast_mapper.structure mapper str)
      | Ptop_dir _ as x -> x
    in
    let phrase = match phrase with
      | Ptop_dir _ as x -> x
      | Ptop_def s -> Ptop_def (Pparse.apply_rewriters_str ~tool_name:"expect" s)
    in
    let phrase = Async_autorun.rewrite_phrase phrase in
    if !Clflags.dump_parsetree then Printast. top_phrase ppf phrase;
    if !Clflags.dump_source    then Pprintast.top_phrase ppf phrase;
    Env.reset_cache_toplevel ();
    Toploop.execute_phrase !verbose ppf phrase
;;

type var_and_value = V : 'a ref * 'a -> var_and_value

let protect_vars =
  let set_vars l = List.iter (fun (V (r, v)) -> r := v) l in
  fun vars ~f ->
    let backup = List.map (fun (V (r, _)) -> V (r, !r)) vars in
    set_vars vars;
    Misc.try_finally f (fun () -> set_vars backup)
;;

let capture_compiler_stuff ppf ~f =
  protect_vars
    [ V (Location.formatter_for_warnings , ppf) ]
    ~f
;;

let redirect ~f =
  let lazy () = disable_outputs in
  let stdout_backup = Unix.dup Unix.stdout in
  let stderr_backup = Unix.dup Unix.stdout in
  let filename = Filename.temp_file "expect-test" "stdout" in
  let fd_out = Unix.openfile filename Unix.[O_WRONLY; O_CREAT; O_TRUNC] 0o600 in
  Unix.dup2 fd_out Unix.stdout;
  Unix.dup2 fd_out Unix.stderr;
  let ic = open_in filename in
  let read_up_to = ref 0 in
  let capture buf =
    flush stdout;
    flush stderr;
    let pos = Unix.lseek fd_out 0 Unix.SEEK_CUR in
    let len = pos - !read_up_to in
    read_up_to := pos;
    Buffer.add_channel buf ic len
  in
  Misc.try_finally (fun () -> f ~capture)
    (fun () ->
       close_in_noerr ic;
       Unix.close fd_out;
       Unix.dup2 stdout_backup Unix.stdout;
       Unix.dup2 stderr_backup Unix.stderr;
       Unix.close stdout_backup;
       Unix.close stderr_backup;
       Sys.remove filename)
;;

let cleanup_chunk (kind, str) =
  let len = String.length str in
  if len = 0 then (kind, str) else
    let trim_from = if str.[0] = '\n' then 1 else 0 in
    let trim_to = if str.[len - 1] = '\n' then len - 1 else len in
    (kind, String.sub str trim_from (trim_to - trim_from))

let cleanup_lines lines =
  let lines = List.map cleanup_chunk lines in
  let rec join = function
    | (Chunk.Raw, str1) :: (Chunk.Raw, str2) :: rest ->
      join ((Chunk.Raw, str1 ^ "\n" ^ str2) :: rest)
    | (Chunk.OCaml, str1) :: (Chunk.OCaml, str2) :: rest ->
      join ((Chunk.OCaml, str1 ^ "\n" ^ str2) :: rest)
    | x :: xs -> x :: join xs
    | [] -> []
  in
  join lines

let eval_phrases ~run_nondeterministic ~dry_run lexbuf =
  let open Phrase in
  (* 4.03: Warnings.reset_fatal (); *)
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  let exec_code ~capture phrase =
    let lines = ref [] in
    let capture kind =
      capture buf;
      match Buffer.contents buf with
      | "" -> ()
      | s -> Buffer.clear buf; lines := (kind, s) :: !lines
    in
    let out_phrase' = !Oprint.out_phrase in
    let out_phrase ppf phr = match phr with
      | Outcometree.Ophr_exception _ -> out_phrase' ppf phr
      | _ ->
        capture Chunk.Raw;
        out_phrase' ppf phr;
        capture Chunk.OCaml;
    in
    Oprint.out_phrase := out_phrase;
    let restore () = Oprint.out_phrase := out_phrase' in
    begin match toplevel_exec_phrase ppf phrase with
      | (_ : bool) -> restore ()
      | exception exn ->
        restore ();
        Location.report_exception ppf exn
    end;
    Format.pp_print_flush ppf ();
    capture Chunk.Raw;
    if !silent || (not !verbose_findlib && Phrase.is_findlib_directive phrase)
    then
      Code []
    else
      Code (cleanup_lines (List.rev !lines))
  in
  if dry_run then Phrase.read_all lexbuf
  else (
    redirect ~f:(fun ~capture ->
        capture_compiler_stuff ppf ~f:(fun () ->
            let rec process_phrase chunks phrase =
              match Phrase.kind phrase with
              | Expect x ->
                let e = Expect {x with responses = cleanup_lines x.responses} in
                next_phrase ((phrase, e) :: chunks)
              | Part _ as x ->
                next_phrase ((phrase, x) :: chunks)
              | Code () ->
                match Phrase.read lexbuf with
                | None ->
                  List.rev ((phrase, exec_code ~capture phrase) :: chunks)
                | Some phrase' ->
                  let role = match Phrase.kind phrase' with
                    | Expect { nondeterministic = true; responses; _ }
                      when not run_nondeterministic -> Code responses
                    | _ -> exec_code ~capture phrase
                  in
                  process_phrase ((phrase, role) :: chunks) phrase'
            and next_phrase chunks = match Phrase.read lexbuf with
              | None -> List.rev chunks
              | Some phrase -> process_phrase chunks phrase
            in
            next_phrase []
          )
      )
  )
;;

let process_expect_file ~run_nondeterministic ~fname ~dry_run ~in_place ~sexp_output =
  let lexbuf = Lexbuf.of_file fname in
  let phrases = eval_phrases ~run_nondeterministic ~dry_run lexbuf in
  let success, phrases = Phrase.validate ~run_nondeterministic phrases in
  let oname = if in_place then fname else fname ^ ".corrected" in
  if success && not in_place && Sys.file_exists oname then
    Sys.remove oname;
  let phrases =
    if success then phrases
    else (
      (* Otherwise, generate corrected file and keep toplevel output. *)
      let oc = open_out_bin (oname ^ ".tmp") in
      Phrase.output oc lexbuf phrases;
      flush oc;
      close_out oc;
      Sys.rename (oname ^ ".tmp") oname;
      phrases
    )
  in
  if sexp_output then (
    Phrase.document ~matched:success lexbuf phrases
    |> Document.sexp_of_t
    |> Sexplib.Sexp.output stdout_backup
  );
  success
;;

let override_sys_argv args =
  let len = Array.length args in
  assert (len <= Array.length Sys.argv);
  Array.blit args 0 Sys.argv 0 len;
  Obj.truncate (Obj.repr Sys.argv) len;
  Arg.current := 0;
;;

let in_place    = ref false
let sexp_output = ref false
let dry_run     = ref false
let run_nondeterministic = ref false

let process_file fname =
  let cmd_line =
    Array.sub Sys.argv !Arg.current (Array.length Sys.argv - !Arg.current)
  in
  override_sys_argv cmd_line;
  Toploop.set_paths ();
  Compmisc.init_path true;
  Toploop.toplevel_env := Compmisc.initial_env ();
  Sys.interactive := false;
  let success =
    process_expect_file ~fname
      ~run_nondeterministic:!run_nondeterministic
      ~dry_run:!dry_run
      ~in_place:!in_place ~sexp_output:!sexp_output
  in
  exit (if success then 0 else 1)
;;

let args =
  Arg.align
    [ "-in-place", Arg.Set in_place,    " Overwrite file in place"
    ; "-sexp"    , Arg.Set sexp_output, " Output the result as a s-expression instead of diffing"
    ; "-verbose" , Arg.Set verbose, " Include outcome of phrase evaluation (like ocaml toplevel)"
    ; "-dry-run" , Arg.Set dry_run, " Don't execute code, only return expected outcome"
    ; "-run-nondeterministic" , Arg.Set run_nondeterministic, " Run non-deterministic tests"
    ; "-verbose-findlib", Arg.Set verbose_findlib, " Include outcome of findlib directives (#require, #use, ...)"
    ]

let print_version () =
  Printf.fprintf stdout_backup "topexect, version %s\n" Sys.ocaml_version;
  exit 0;
;;

let print_version_num () =
  Printf.fprintf stdout_backup "%s\n" Sys.ocaml_version;
  exit 0;
;;

module Options = Main_args.Make_bytetop_options (struct
    open Clflags
    open Compenv

    let set r () = r := true
    let clear r () = r := false

    let _absname = set Location.absname
    let _I dir =
      let dir = Misc.expand_directory Config.standard_library dir in
      include_dirs := dir :: !include_dirs
    let _init s = init_file := Some s
    let _noinit = set noinit
    let _labels = clear classic
    let _alias_deps = clear transparent_modules
    let _no_alias_deps = set transparent_modules
    let _app_funct = set applicative_functors
    let _no_app_funct = clear applicative_functors
    let _noassert = set noassert
    let _nolabels = set classic
    let _noprompt = set noprompt
    let _nopromptcont = set nopromptcont
    let _nostdlib = set no_std_include
    let _open s = open_modules := s :: !open_modules
    let _plugin p = Compplugin.load p
    let _ppx s = first_ppx := s :: !first_ppx
    let _principal = set principal
    let _no_principal = clear principal
    let _rectypes = set recursive_types
    let _no_rectypes = clear recursive_types
    let _safe_string = clear unsafe_string
    let _short_paths = clear real_paths
    let _stdin () = raise (Arg.Bad "-stdin not supported")
    let _strict_sequence = set strict_sequence
    let _no_strict_sequence = clear strict_sequence
    let _strict_formats = set strict_formats
    let _no_strict_formats = clear strict_formats
    let _unboxed_types = set unboxed_types
    let _no_unboxed_types = clear unboxed_types
    let _unsafe = set fast
    let _unsafe_string = set unsafe_string
    let _version () = print_version ()
    let _vnum () = print_version_num ()
    let _no_version = set noversion
    let _w s = Warnings.parse_options false s
    let _warn_error s = Warnings.parse_options true s
    let _warn_help = Warnings.help_warnings
    let _dparsetree = set dump_parsetree
    let _dtypedtree = set dump_typedtree
    let _dsource = set dump_source
    let _drawlambda = set dump_rawlambda
    let _dlambda = set dump_lambda
    let _dflambda = set dump_flambda
    let _dtimings () = profile_columns := [ `Time ]
    let _dinstr = set dump_instr

    let anonymous s = process_file s
    let _args _ = failwith "Arg.read_arg not implemented"
    let _args0 _ = failwith "Arg.read_arg0 not implemented"
    (*let _args = Arg.read_arg
    let _args0 = Arg.read_arg0*)
  end);;

(* BLACK MAGIC: patch field of a module at runtime *)
let monkey_patch (type a) (type b) (m: a) (prj: unit -> b) (v : b) =
  let m = Obj.repr m in
  let v = Obj.repr v in
  let v' = Obj.repr (prj ()) in
  if v' == v then () else (
    try
      for i = 0 to Obj.size m - 1 do
        if Obj.field m i == v' then (
          Obj.set_field m i v;
          if Obj.repr (prj ()) == v then raise Exit;
          Obj.set_field m i v';
        )
      done;
      invalid_arg "monkey_patch: field not found"
    with Exit -> ()
  )

let main () =
  let module M = struct
    module type T = module type of Env
    let field () = Env.without_cmis
    let replacement f x = f x
    let () = monkey_patch (module Env : T) field replacement
  end in
  Topfind.don't_load_deeply ["unix"; "findlib.top"; "findlib.internal"; "compiler-libs.toplevel"; "ppx_sexp_conv"];

  let usage =
    Printf.sprintf "Usage: %s [OPTIONS] FILE [ARGS]\n"
      (Filename.basename Sys.argv.(0))
  in
  try
    let args = Arg.align (args @ Options.list) in
    Arg.parse args process_file (usage ^ "\nOptions are:");
    Printf.fprintf stderr_backup "%s\n%!" usage;
    exit 2
  with exn ->
    ignore (Format.flush_str_formatter ());
    Location.report_exception Format.str_formatter exn;
    Printf.fprintf stderr_backup "%s\n%!" (Format.flush_str_formatter ());
    exit 2
;;
let () = main ();;
