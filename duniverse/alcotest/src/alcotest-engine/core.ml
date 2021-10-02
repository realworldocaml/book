(*
 * Copyright (c) 2013-2016 Thomas Gazagnaire <thomas@gazagnaire.org>
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

include Core_intf
open Model
open Utils

module IntSet = Set.Make (struct
  type t = int

  let compare = (compare : int -> int -> int)
end)

exception Check_error of unit Fmt.t

let () =
  let print_error =
    (* We instantiate the error print buffer lazily, so as to be sensitive to
       [Fmt_tty.setup_std_outputs]. *)
    lazy
      (let buf = Buffer.create 0 in
       let ppf = Format.formatter_of_buffer buf in
       Fmt.set_style_renderer ppf Fmt.(style_renderer stderr);
       fun error ->
         Fmt.pf ppf "Alcotest assertion failure@.%a@." error ();
         let contents = Buffer.contents buf in
         Buffer.clear buf;
         contents)
  in
  Printexc.register_printer (function
    | Check_error err -> Some (Lazy.force print_error err)
    | _ -> None)

module Make (P : Platform.MAKER) (M : Monad.S) : S with type return = unit M.t =
struct
  module P = P (M)

  module Pp = struct
    include Pp
    include Pp.Make (P)
  end

  module M = Monad.Extend (M)
  module Suite = Suite (M)
  include M.Infix

  (** Take a string path and collapse a leading [$HOME] path segment to [~]. *)
  let maybe_collapse_home path =
    match P.home_directory () with
    | Error _ -> path
    | Ok home -> (
        (* Astring doesn't have [cut_prefix]. *)
        match String.is_prefix ~affix:home path with
        | false -> path
        | true ->
            let tail =
              String.Sub.to_string (String.sub ~start:(String.length home) path)
            in
            "~" ^ tail)

  (* Types *)
  type return = unit M.t
  type 'a run = 'a -> unit M.t
  type speed_level = [ `Quick | `Slow ]

  exception Test_error

  type 'a test_case = string * speed_level * 'a run

  let test_case n s f = (n, s, f)

  type 'a test = string * 'a test_case list

  (* global state *)
  type 'a t = {
    (* library values. *)
    suite : 'a Suite.t;
    (* runtime state. *)
    mutable errors : unit Fmt.t list;
    (* runtime options. *)
    max_label : int;
        (** Longest test label in the suite, in UTF-8 characters. *)
    config : Config.t;
    run_id : string;
  }

  let empty ~config ~suite_name =
    let errors = [] in
    let suite =
      match Suite.v ~name:suite_name with
      | Ok s -> s
      | Error `Empty_name ->
          Pp.user_error
            "Suite name cannot cannot be empty. Please pass a non-empty string \
             to `run`."
    in
    let max_label = 0 in
    let run_id = Uuidm.to_string ~upper:true Uuidm.nil in
    { suite; errors; max_label; config; run_id }

  let compare_speed_level s1 s2 =
    match (s1, s2) with
    | `Quick, `Quick | `Slow, `Slow -> 0
    | `Quick, _ -> 1
    | _, `Quick -> -1

  (*
     Reverse a list, taking at most the first n elements of the original
     list.
  *)
  let rev_head n l =
    let rec aux acc n l =
      match l with
      | x :: xs -> if n > 0 then aux (x :: acc) (n - 1) xs else acc
      | [] -> acc
    in
    aux [] n l

  (*
     Show the last lines of a log file.
     The goal is to not clutter up the console output.
  *)
  let read_tail max_lines ic =
    let rev_lines = ref [] in
    try
      while true do
        rev_lines := input_line ic :: !rev_lines
      done;
      assert false
    with End_of_file ->
      let selected_lines =
        match max_lines with
        | `Unlimited -> List.rev !rev_lines
        | `Limit n -> rev_head n !rev_lines
      in
      let omitted_count = List.length !rev_lines - List.length selected_lines in
      let display_lines =
        if omitted_count = 0 then selected_lines
        else
          Fmt.strf "... (omitting %i line%a)" omitted_count Pp.pp_plural
            omitted_count
          :: selected_lines
      in
      String.concat ~sep:"\n" display_lines ^ "\n"

  let log_dir ~via_symlink t =
    let via_symlink =
      (* We don't create symlinks on Windows. *)
      via_symlink && not Sys.win32
    in
    Filename.concat t.config#log_dir
      (if via_symlink then Suite.name t.suite else t.run_id)

  let pp_suite_results t =
    let log_dir = log_dir ~via_symlink:true t |> maybe_collapse_home in
    Pp.suite_results ~log_dir t.config

  let pp_event ~isatty ~prior_error ~tests_so_far t =
    let cfg = t.config in
    let selector_on_failure =
      (not prior_error) && not (cfg#verbose || cfg#show_errors)
    in
    if not cfg#json then
      Pp.event ~isatty ~compact:cfg#compact ~max_label:t.max_label
        ~doc_of_test_name:(Suite.doc_of_test_name t.suite)
        ~selector_on_failure ~tests_so_far
    else Fmt.nop

  let pp_info t =
    Pp.info ~max_label:t.max_label
      ~doc_of_test_name:(Suite.doc_of_test_name t.suite)

  let output_file t tname =
    Filename.concat (log_dir ~via_symlink:true t) (Test_name.file tname)

  let color c ppf fmt = Fmt.(styled c string) ppf fmt
  let red_s fmt = color `Red fmt
  let red ppf fmt = Fmt.kstrf (fun str -> red_s ppf str) fmt

  let pp_error ~doc_of_test_name ~output_file ~max_label cfg ppf e =
    let path, error_fmt =
      match e with `Error (p, f) -> (p, f) | `Exn (p, _, f) -> (p, f)
    in
    let pp_logs ppf () =
      let filename = output_file path in
      if cfg#verbose || not (Sys.file_exists filename) then
        Fmt.pf ppf "%a@," error_fmt ()
      else
        let file = open_in filename in
        let output = read_tail cfg#tail_errors file in
        close_in file;
        Fmt.pf ppf "%s@,Logs saved to %a.@," output
          Fmt.(Pp.quoted (styled `Cyan string))
          (maybe_collapse_home filename)
    in
    Fmt.(
      Pp.with_surrounding_box
        (const
           (Pp.event_line ~margins:3 ~max_label ~doc_of_test_name)
           (`Result (path, e)))
      ++ pp_logs
      ++ Pp.horizontal_rule
      ++ cut)
      ppf ()

  let has_run : Run_result.t -> bool = function
    | `Ok | `Error _ | `Exn _ -> true
    | `Skip | `Todo _ -> false

  let bt () = match Printexc.get_backtrace () with "" -> "" | s -> "\n" ^ s
  let exn path name pp = `Exn (path, name, Fmt.(pp ++ const lines (bt ())))

  let protect_test path (f : 'a run) : 'a -> Run_result.t M.t =
   fun args ->
    M.catch
      (fun () -> f args >|= fun () -> `Ok)
      ((function
         | Check_error err ->
             let err = Fmt.(err ++ const string (bt ())) in
             `Error (path, err)
         | Failure s -> exn path "failure" Fmt.(const string s)
         | Invalid_argument s -> exn path "invalid" Fmt.(const string s)
         | e -> exn path "exception" Fmt.(const exn e))
      >> M.return)

  type running_state = { tests_so_far : int; first_error : int option }
  (** State that is kept during the test executions. *)

  let perform_test t args { tests_so_far; first_error } suite =
    let open Suite in
    let test = suite.fn in
    let print_event =
      pp_event t
        ~prior_error:(Option.is_some first_error)
        ~tests_so_far ~isatty:(P.stdout_isatty ()) Fmt.stdout
    in
    M.return () >>= fun () ->
    print_event (`Start suite.name);
    Fmt.(flush stdout) () (* Show event before any test stderr *);
    test args >|= fun result ->
    (* Store errors *)
    let errored : bool =
      let pp_error =
        pp_error
          ~doc_of_test_name:(Suite.doc_of_test_name t.suite)
          ~output_file:(output_file t) ~max_label:t.max_label t.config
      in
      let error, errored =
        match result with
        | (`Error _ | `Exn (_, _, _)) as e -> ([ Fmt.const pp_error e ], true)
        | _ -> ([], false)
      in
      t.errors <- error @ t.errors;
      errored
    in
    (* Show any remaining test output before the event *)
    Fmt.(flush stdout ());
    Fmt.(flush stderr ());
    print_event (`Result (suite.name, result));
    let error = if errored then Some tests_so_far else None in
    let state =
      {
        tests_so_far = tests_so_far + 1;
        first_error = Option.(first_error || error);
      }
    in
    (state, result)

  let perform_tests t tests args =
    let currently_bailing acc =
      Option.is_some acc.first_error && t.config#bail
    in
    M.List.fold_map_s
      (fun acc test ->
        if currently_bailing acc then
          M.return ({ acc with tests_so_far = succ acc.tests_so_far }, `Skip)
        else perform_test t args acc test)
      { tests_so_far = 0; first_error = None }
      tests
    >|= fun (state, test_results) ->
    let () =
      if currently_bailing state then
        match state.tests_so_far - Option.get_exn state.first_error - 1 with
        | n when n > 0 ->
            Fmt.pr "@\n  %a@\n"
              Fmt.(styled `Faint string)
              (Fmt.str "... with %d subsequent test%a skipped." n Pp.pp_plural n)
        | 0 -> ()
        | _ -> assert false
    in
    test_results

  let skip_fun _ = M.return `Skip
  let skip_label test_case = Suite.{ test_case with fn = skip_fun }

  let filter_test_case (regexp, cases) =
    let regexp_match =
      match regexp with
      | None -> fun _ -> true
      | Some r -> fun n -> Re.execp r n
    in
    let index_match =
      match cases with
      | None -> fun _ -> true
      | Some ints ->
          let set = IntSet.of_list ints in
          fun i -> IntSet.mem i set
    in
    fun test_case ->
      let name, index =
        let tn = test_case.Suite.name in
        Test_name.(name tn, index tn)
      in
      regexp_match name && index_match index

  let filter_test_cases ~subst path test_cases =
    let filter_test_case = filter_test_case path in
    test_cases
    |> List.filter_map (fun tc ->
           if filter_test_case tc then Some tc
           else if subst then Some (skip_label tc)
           else None)

  let redirect_test_output t test_case =
    let output_file = output_file t test_case.Suite.name in
    let fn args =
      P.with_redirect output_file (fun () ->
          test_case.fn args >|= fun result ->
          Pp.rresult_error Fmt.stdout result;
          result)
    in
    { test_case with fn }

  let select_speed speed_level (test_case : 'a Suite.test_case as 'tc) : 'tc =
    if compare_speed_level test_case.speed_level speed_level >= 0 then test_case
    else Suite.{ test_case with fn = skip_fun }

  let result t test args =
    P.prepare ~base:t.config#log_dir
      ~dir:(log_dir ~via_symlink:false t)
      ~name:(Suite.name t.suite);
    let start_time = P.time () in
    let test =
      if t.config#verbose then test else List.map (redirect_test_output t) test
    in
    let speed_level = if t.config#quick_only then `Quick else `Slow in
    let test = List.map (select_speed speed_level) test in
    perform_tests t test args >|= fun results ->
    let time = P.time () -. start_time in
    let success = List.length (List.filter has_run results) in
    let failures = List.length (List.filter Run_result.is_failure results) in
    Pp.{ time; success; failures; errors = List.rev t.errors }

  let list_registered_tests t () =
    Suite.tests t.suite
    |> List.map (fun t -> t.Suite.name)
    |> List.sort Test_name.compare
    |> Fmt.(list ~sep:(const string "\n") (pp_info t) stdout)

  let register (type a) (t : a t) (name, (ts : a test_case list)) : a t =
    let max_label = max t.max_label (String.length_utf8 name) in
    let test_details =
      List.mapi
        (fun index (doc, speed, test) ->
          let path = Test_name.v ~name ~index in
          let doc =
            if doc = "" || doc.[String.length doc - 1] = '.' then doc
            else doc ^ "."
          in
          let test a = protect_test path test a in
          (path, doc, speed, test))
        ts
    in
    let suite =
      List.fold_left
        (fun acc td ->
          match Suite.add acc td with
          | Ok acc -> acc
          | Error (`Duplicate_test_path path) ->
              Fmt.kstr Pp.user_error "Duplicate test path: `%s'" path)
        t.suite test_details
    in
    { t with suite; max_label }

  let register_all t cases = List.fold_left register t cases

  let run_tests t () args =
    let filter = t.config#filter in
    let suite = Suite.tests t.suite in
    let is_empty = filter_test_cases ~subst:false filter suite = [] in
    (if is_empty && filter <> (None, None) then (
     Fmt.(pf stderr)
       "%a\n" red
       "Invalid request (no tests to run, filter skipped everything)!";
     exit 1)
    else
      let tests = filter_test_cases ~subst:true filter suite in
      result t tests args)
    >|= fun result ->
    (pp_suite_results t) Fmt.stdout result;
    result.failures

  let default_log_dir () =
    let fname_concat l = List.fold_left Filename.concat "" l in
    fname_concat [ P.getcwd (); "_build"; "_tests" ]

  type 'a with_options = 'a Config.with_options

  let list_tests (type a) (tl : a test list) =
    (* TODO: refactor [register_all] to not require dummy state *)
    let config =
      Config.apply_defaults ~default_log_dir:"<not-shown-to-user>"
        (Config.User.create ())
    in
    let t = register_all (empty ~config ~suite_name:"<not-shown-to-user>") tl in
    list_registered_tests t ();
    M.return ()

  let run_with_args (config : Config.t) name (type a) (args : a)
      (tl : a test list) =
    let random_state = Random.State.make_self_init () in
    let run_id = Uuidm.v4_gen random_state () |> Uuidm.to_string ~upper:true in
    let t = { (empty ~config ~suite_name:name) with run_id } in
    let t = register_all t tl in
    ( (* Only print inside the concurrency monad *)
      M.return () >>= fun () ->
      let open Fmt in
      pr "Testing %a.@," (Pp.quoted Fmt.(styled `Bold Suite.pp_name)) t.suite;
      pr "@[<v>%a@]"
        (styled `Faint (fun ppf () ->
             pf ppf "This run has ID %a.@,@," (Pp.quoted string) run_id))
        ();
      run_tests t () args )
    >|= fun test_failures ->
    match (test_failures, t.config#and_exit) with
    | 0, true -> exit 0
    | 0, false -> ()
    | _, true -> exit 1
    | _, false -> raise Test_error

  let run config name (tl : unit test list) = run_with_args config name () tl

  let with_defaults f cfg =
    f (Config.apply_defaults ~default_log_dir:(default_log_dir ()) cfg)

  let run_with_args ?and_exit ?verbose ?compact ?tail_errors ?quick_only
      ?show_errors ?json ?filter ?log_dir ?bail =
    Config.User.kcreate
      (with_defaults run_with_args)
      ?and_exit ?verbose ?compact ?tail_errors ?quick_only ?show_errors ?json
      ?filter ?log_dir ?bail

  let run = Config.User.kcreate (with_defaults run)
end
