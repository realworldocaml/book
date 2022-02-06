(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Mdx
open Result
open Astring
open Mdx.Util.Result.Infix

let src = Logs.Src.create "cram.test"

module Log = (val Logs.src_log src : Logs.LOG)

let ( / ) = Filename.concat

(* From jbuilder's stdlib *)
let ansi_color_strip str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec loop i =
    if i = len then Buffer.contents buf
    else
      match str.[i] with
      | '\027' -> skip (i + 1)
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  and skip i =
    if i = len then Buffer.contents buf
    else match str.[i] with 'm' -> loop (i + 1) | _ -> skip (i + 1)
  in
  loop 0

let output_from_line s =
  `Output (String.drop ~rev:true ~sat:Char.Ascii.is_blank s)

let with_dir root f =
  match root with
  | None -> f ()
  | Some d -> (
      let old_d = Sys.getcwd () in
      try
        Sys.chdir d;
        let r = f () in
        Sys.chdir old_d;
        r
      with e ->
        Sys.chdir old_d;
        raise e)

let get_env unset_variables =
  let env = Array.to_list (Unix.environment ()) in
  let env = List.map (String.cuts ~sep:"=") env in
  let f env var =
    let g l = String.compare (List.nth l 0) var <> 0 in
    List.filter g env
  in
  let env = List.fold_left f env unset_variables in
  Array.of_list (List.map (String.concat ~sep:"=") env)

let run_test ?root unset_variables temp_file t =
  let cmd = Cram.command_line t in
  let env = get_env unset_variables in
  Log.info (fun l -> l "exec: %S" cmd);
  let fd = Unix.openfile temp_file [ O_WRONLY; O_TRUNC ] 0 in
  let pid =
    with_dir root (fun () ->
        Unix.create_process_env "sh" [| "sh"; "-c"; cmd |] env Unix.stdin fd fd)
  in
  Unix.close fd;
  Util.Process.wait ~pid

let root_dir ?root ?block () =
  match (block : Block.t option) with
  | Some { dir = None; _ } -> root
  | Some { dir = Some d; loc = { loc_start = { pos_fname; _ }; _ }; _ } -> (
      match root with
      | Some r -> Some (r / d)
      | None -> Some (Filename.dirname pos_fname / d))
  | None -> root

let resolve_root file dir root =
  match root with None -> dir / file | Some r -> r / dir / file

let run_cram_tests ?syntax t ?root ppf temp_file pad tests =
  Block.pp_header ?syntax ppf t;
  let pad = match syntax with Some Cram -> pad + 2 | _ -> pad in
  List.iter
    (fun test ->
      let root = root_dir ?root ~block:t () in
      let unset_variables = Block.unset_variables t in
      let n = run_test ?root unset_variables temp_file test in
      let lines = Mdx.Util.File.read_lines temp_file in
      let output =
        let output = List.map output_from_line lines in
        if Output.equal output test.output then test.output
        else Output.merge output test.output
      in
      Cram.pp_command ~pad ppf test;
      List.iter
        (function
          | `Ellipsis -> Output.pp ~pad ppf `Ellipsis
          | `Output line ->
              let line = ansi_color_strip line in
              Output.pp ?syntax ~pad ppf (`Output line))
        output;
      Cram.pp_exit_code ~pad ppf n)
    tests;
  Block.pp_footer ?syntax ppf t

let eval_test ?block ?root c cmd =
  Log.debug (fun l -> l "eval_test %a" Fmt.(Dump.list (Fmt.fmt "%S")) cmd);
  let root = root_dir ?root ?block () in
  with_dir root (fun () -> Mdx_top.eval c cmd)

let err_eval ~cmd lines =
  Fmt.epr "Got an error while evaluating:\n---\n%a\n---\n%a\n%!"
    Fmt.(list ~sep:(any "\n") string)
    cmd
    Fmt.(list ~sep:(any "\n") string)
    lines;
  exit 1

let eval_raw ?block ?root c cmd =
  match eval_test ?block ?root c cmd with
  | Ok _ -> ()
  | Error e -> err_eval ~cmd e

let split_lines lines =
  let aux acc s =
    (* XXX(samoht) support windowns *)
    let lines = String.cuts ~sep:"\n" s in
    List.append lines acc
  in
  List.fold_left aux [] (List.rev lines)

let eval_ocaml ~block ?syntax ?root c ppf cmd errors =
  let update ~errors = function
    | { Block.value = OCaml v; _ } as b ->
        { b with value = OCaml { v with errors } }
    (* [eval_ocaml] only called on OCaml blocks *)
    | _ -> assert false
  in
  let contains_warnings = String.is_infix ~affix:"Warning" in
  let lines =
    match eval_test ?root ~block c cmd with
    | Ok lines -> List.filter contains_warnings lines
    | Error lines -> lines
  in
  let errors =
    match lines with
    | [] -> []
    | lines ->
        let lines = split_lines lines in
        let output = List.map output_from_line lines in
        if Output.equal output errors then errors
        else
          List.map
            (function
              | `Ellipsis -> `Ellipsis
              | `Output x -> `Output (ansi_color_strip x))
            (Output.merge output errors)
  in
  Block.pp ?syntax ppf (update ~errors block)

let lines = function Ok x | Error x -> x

let run_toplevel_tests ?syntax ?root c ppf tests t =
  Block.pp_header ?syntax ppf t;
  List.iter
    (fun (test : Toplevel.t) ->
      let lines = lines (eval_test ?root ~block:t c test.command) in
      let lines = split_lines lines in
      let output =
        let output = List.map output_from_line lines in
        if Output.equal output test.output then test.output else output
      in
      let pad = test.hpad in
      Toplevel.pp_command ppf test;
      List.iter
        (function
          | `Ellipsis -> Output.pp ~pad ppf `Ellipsis
          | `Output line ->
              let line = ansi_color_strip line in
              Output.pp ~pad ppf (`Output line))
        output)
    tests;
  Block.pp_footer ?syntax ppf t

type file = { first : Mdx.Part.file; current : Mdx.Part.file }

let files : (string, file) Hashtbl.t = Hashtbl.create 8

let has_changed ~force_output { first; current } =
  let contents = Mdx.Part.contents current in
  if contents = Mdx.Part.contents first && force_output = false then None
  else Some contents

let read_parts file =
  try Hashtbl.find files file
  with Not_found -> (
    match Mdx.Part.read file with
    | exception Sys_error msg -> failwith msg
    | parts ->
        let f = { first = parts; current = parts } in
        Hashtbl.add files file f;
        f)

let read_part file part =
  let parts = read_parts file in
  match Mdx.Part.find parts.current ~part with
  | None ->
      Fmt.failwith "Cannot find part %S in %s"
        (match part with None -> "" | Some p -> p)
        file
  | Some lines ->
      let contents = String.concat ~sep:"\n" lines in
      String.drop contents ~rev:true ~sat:Char.Ascii.is_white
      |> String.drop ~sat:(function '\n' -> true | _ -> false)

let write_parts ~force_output file parts =
  let output_file = file ^ ".corrected" in
  match has_changed ~force_output parts with
  | None -> if Sys.file_exists output_file then Sys.remove output_file
  | Some c ->
      let oc = open_out output_file in
      output_string oc c;
      flush oc;
      close_out oc

let update_block_content ?syntax ppf t content =
  Block.pp_header ?syntax ppf t;
  Output.pp ppf (`Output content);
  Block.pp_footer ?syntax ppf t

let update_file_or_block ?syntax ?root ppf md_file ml_file block part =
  let root = root_dir ?root ~block () in
  let dir = Filename.dirname md_file in
  let ml_file = resolve_root ml_file dir root in
  update_block_content ?syntax ppf block (read_part ml_file part)

exception Test_block_failure of Block.t * string

let with_non_det ~command ~output ~det non_deterministic = function
  (* the command is non-deterministic so skip everything *)
  | Some Label.Nd_command when not non_deterministic -> command ()
  (* its output is non-deterministic; run it but keep the old output. *)
  | Some Label.Nd_output when not non_deterministic -> output ()
  | _ -> det ()

let preludes ~prelude ~prelude_str =
  let aux to_lines p =
    let env, file = Mdx.Prelude.env_and_file p in
    (env, to_lines file)
  in
  match (prelude, prelude_str) with
  | [], [] -> []
  | [], fs -> List.map (aux (fun x -> [ x ])) fs
  | fs, [] -> List.map (aux Mdx.Util.File.read_lines) fs
  | _ -> Fmt.failwith "only one of --prelude or --prelude-str shoud be used"

let run_exn ~non_deterministic ~silent_eval ~record_backtrace ~syntax ~silent
    ~verbose_findlib ~prelude ~prelude_str ~file ~section ~root ~force_output
    ~output ~directives ~packages ~predicates =
  Printexc.record_backtrace record_backtrace;
  let syntax =
    match syntax with Some syntax -> Some syntax | None -> Syntax.infer ~file
  in
  let c =
    Mdx_top.init ~verbose:(not silent_eval) ~silent ~verbose_findlib ~directives
      ~packages ~predicates ()
  in
  let preludes = preludes ~prelude ~prelude_str in

  let test_block ~ppf ~temp_file t =
    let print_block () = Block.pp ?syntax ppf t in
    if Block.is_active ?section t then
      match Block.value t with
      | Raw _ -> print_block ()
      | Include { file_included; file_kind = Fk_ocaml { part_included } } ->
          assert (syntax <> Some Cram);
          update_file_or_block ?syntax ?root ppf file file_included t
            part_included
      | Include { file_included; file_kind = Fk_other _ } ->
          let new_content = read_part file_included None in
          update_block_content ?syntax ppf t new_content
      | OCaml { non_det; env; errors } ->
          let det () =
            assert (syntax <> Some Cram);
            Mdx_top.in_env env (fun () ->
                eval_ocaml ~block:t ?syntax ?root c ppf t.contents errors)
          in
          with_non_det non_deterministic non_det ~command:print_block
            ~output:det ~det
      | Cram { language = _; non_det } ->
          let pad, tests =
            Cram.of_lines
              ~syntax:(Option.value ~default:Normal syntax)
              ~loc:t.loc t.contents
          in
          with_non_det non_deterministic non_det ~command:print_block
            ~output:(fun () ->
              print_block ();
              let unset_variables = Block.unset_variables t in
              List.iter
                (fun t -> ignore (run_test ?root unset_variables temp_file t))
                tests)
            ~det:(fun () ->
              run_cram_tests ?syntax t ?root ppf temp_file pad tests)
      | Toplevel { non_det; env } ->
          let phrases =
            let syntax = Util.Option.value syntax ~default:Normal in
            Toplevel.of_lines ~syntax ~loc:t.loc t.contents
          in
          let phrases = Deprecated.Missing_double_semicolon.fix phrases in
          with_non_det non_deterministic non_det ~command:print_block
            ~output:(fun () ->
              assert (syntax <> Some Cram);
              print_block ();
              List.iter
                (fun (phrase : Toplevel.t) ->
                  match
                    Mdx_top.in_env env (fun () ->
                        eval_test ~block:t ?root c phrase.command)
                  with
                  | Ok _ -> ()
                  | Error e ->
                      let output = List.map (fun l -> `Output l) e in
                      if Output.equal phrase.output output then ()
                      else err_eval ~cmd:phrase.command e)
                phrases)
            ~det:(fun () ->
              assert (syntax <> Some Cram);
              Mdx_top.in_env env (fun () ->
                  run_toplevel_tests ?syntax ?root c ppf phrases t))
    else print_block ()
  in
  let gen_corrected file_contents items =
    let temp_file = Filename.temp_file "ocaml-mdx" ".output" in
    at_exit (fun () -> Sys.remove temp_file);
    let buf = Buffer.create (String.length file_contents + 1024) in
    let ppf = Format.formatter_of_buffer buf in
    let envs = Document.envs items in
    let eval lines () = eval_raw ?root c lines in
    let eval_in_env lines env = Mdx_top.in_env env (eval lines) in
    List.iter
      (function
        | `All, lines -> Ocaml_env.Set.iter (eval_in_env lines) envs
        | `One env, lines -> eval_in_env lines env)
      preludes;
    List.iter
      (function
        | (Mdx.Document.Section _ | Text _) as t -> Mdx.pp_line ?syntax ppf t
        | Block t -> (
            List.iter (fun (k, v) -> Unix.putenv k v) (Block.set_variables t);
            try test_block ~ppf ~temp_file t
            with Failure msg -> raise (Test_block_failure (t, msg))))
      items;
    Deprecated.Missing_double_semicolon.report ~filename:file;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  (match output with
  | Some `Stdout -> Mdx.run_to_stdout ?syntax ~f:gen_corrected file
  | Some (`File outfile) ->
      Mdx.run_to_file ?syntax ~outfile ~f:gen_corrected file
  | None -> Mdx.run ?syntax ~force_output ~f:gen_corrected file)
  >>! fun () ->
  Hashtbl.iter (write_parts ~force_output) files;
  0

module Package = struct
  let unix = "unix"
  let findlib_top = "findlib.top"
  let findlib_internal = "findlib.internal"
  let compilerlibs_toplevel = "compiler-libs.toplevel"
end

module Predicate = struct
  let byte = "byte"
  let toploop = "toploop"
end
