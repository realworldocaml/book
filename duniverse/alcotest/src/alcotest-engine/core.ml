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
open! Import
open Model

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

module Make (P : Platform.MAKER) (M : Monad.S) = struct
  module P = P (M)

  module Pp = struct
    include Pp
    include Pp.Make (P)
  end

  module M = Monad.Extend (M)
  module Suite = Suite (M)
  module Log_trap = Log_trap.Make (M) (P)
  include M.Syntax

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
    log_trap : Log_trap.t;
  }

  let gen_run_id =
    let random_state = lazy (Random.State.make_self_init ()) in
    let random_hex _ =
      let state = Lazy.force random_state in
      match Random.State.int state 36 with
      | n when n < 10 -> Char.chr (n + Char.code '0')
      | n -> Char.chr (n - 10 + Char.code 'A')
    in
    fun () -> String.v ~len:8 random_hex

  let empty ~config ~trap_logs ~suite_name:unescaped_name =
    let errors = [] in
    let suite =
      match Suite.v ~name:unescaped_name with
      | Ok s -> s
      | Error `Empty_name ->
          Pp.user_error
            "Suite name cannot cannot be empty. Please pass a non-empty string \
             to `run`."
    in
    let max_label = 0 in
    let run_id = gen_run_id () in
    let log_trap =
      match trap_logs with
      | false -> Log_trap.inactive
      | true ->
          Log_trap.active ~root:config#log_dir ~uuid:run_id
            ~suite_name:(Suite.name suite)
    in
    { suite; errors; max_label; config; run_id; log_trap }

  let compare_speed_level s1 s2 =
    match (s1, s2) with
    | `Quick, `Quick | `Slow, `Slow -> 0
    | `Quick, _ -> 1
    | _, `Quick -> -1

  let pp_suite_results t =
    let log_dir = Log_trap.pp_current_run_dir t.log_trap in
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

  let color c ppf fmt = Fmt.(styled c string) ppf fmt
  let red_s fmt = color `Red fmt
  let red ppf fmt = Fmt.kstr (fun str -> red_s ppf str) fmt

  let pp_error t ppf e =
    let path, error_fmt =
      match e with `Error (p, f) -> (p, f) | `Exn (p, _, f) -> (p, f)
    in
    let pp_logs ppf () =
      let pp_logs =
        Log_trap.recover_logs ~tail:t.config#tail_errors t.log_trap path
      in
      match (t.config#verbose, pp_logs) with
      | true, _ | _, None -> Fmt.pf ppf "%a@," error_fmt ()
      | false, Some pp_logs ->
          let pp_log_dir =
            Pp.map_theta
              ~f:(fun s -> Pp.quoted (Fmt.styled `Cyan s))
              (Log_trap.pp_log_location t.log_trap path)
          in
          Fmt.pf ppf "%tLogs saved to %t.@," pp_logs pp_log_dir
    in
    Fmt.(
      Pp.with_surrounding_box
        (const
           (Pp.event_line ~margins:3 ~max_label:t.max_label
              ~doc_of_test_name:(Suite.doc_of_test_name t.suite))
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

  let with_captured_logs t name fn args =
    if t.config#verbose then fn args
    else
      Log_trap.with_captured_logs t.log_trap name
        (fun () ->
          (* When capturing the logs of a test, also add the result of the test
             at the end. *)
          let+ result = fn args in
          Pp.rresult_error Fmt.stdout result;
          result)
        ()

  let perform_test t args { tests_so_far; first_error }
      (test : _ Suite.test_case) =
    let open Suite in
    let print_event =
      pp_event t
        ~prior_error:(Option.is_some first_error)
        ~tests_so_far ~isatty:(P.stdout_isatty ()) Fmt.stdout
    in
    let* () = M.return () in
    print_event (`Start test.name);
    let+ result, errored =
      match test.fn with
      | `Skip -> M.return (`Skip, false)
      | `Run fn ->
          Fmt.(flush stdout) () (* Show event before any test stderr *);
          let+ result = with_captured_logs t test.name fn args in
          (* Store errors *)
          let errored : bool =
            let error, errored =
              match result with
              | (`Error _ | `Exn (_, _, _)) as e ->
                  ([ Fmt.const (pp_error t) e ], true)
              | _ -> ([], false)
            in
            t.errors <- error @ t.errors;
            errored
          in
          (* Show any remaining test output before the event *)
          Fmt.(flush stdout ());
          Fmt.(flush stderr ());
          (result, errored)
    in
    print_event (`Result (test.name, result));
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
    let+ state, test_results =
      M.List.fold_map_s
        (fun acc test ->
          if currently_bailing acc then
            M.return ({ acc with tests_so_far = succ acc.tests_so_far }, `Skip)
          else perform_test t args acc test)
        { tests_so_far = 0; first_error = None }
        tests
    in
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

  let skip_label test_case = Suite.{ test_case with fn = `Skip }

  let filter_test_case p test_case =
    match p with
    | None -> true
    | Some p -> (
        let name, index =
          let tn = test_case.Suite.name in
          Test_name.(Safe_string.to_unescaped_string (name tn), index tn)
        in
        match p ~name ~index with `Run -> true | `Skip -> false)

  let filter_test_cases ~subst path test_cases =
    let filter_test_case = filter_test_case path in
    test_cases
    |> List.filter_map (fun tc ->
           if filter_test_case tc then Some tc
           else if subst then Some (skip_label tc)
           else None)

  let select_speed speed_level (test_case : 'a Suite.test_case as 'tc) : 'tc =
    if compare_speed_level test_case.speed_level speed_level >= 0 then test_case
    else Suite.{ test_case with fn = `Skip }

  let result t test args =
    let initial_backtrace_status = Printexc.backtrace_status () in
    if t.config#record_backtrace then Printexc.record_backtrace true;
    let start_time = P.time () in
    let speed_level = if t.config#quick_only then `Quick else `Slow in
    let test = List.map (select_speed speed_level) test in
    let+ results = perform_tests t test args in
    let time = P.time () -. start_time in
    let success = List.length (List.filter has_run results) in
    let failures = List.length (List.filter Run_result.is_failure results) in
    if t.config#record_backtrace then
      Printexc.record_backtrace initial_backtrace_status;
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
          (path, doc, speed, `Run test))
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
    let+ result =
      if is_empty && Option.is_some filter then (
        flush_all ();
        Fmt.(pf stderr)
          "%a\n" red
          "Invalid request (no tests to run, filter skipped everything)!";
        exit 1)
      else
        let tests = filter_test_cases ~subst:true filter suite in
        result t tests args
    in
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
    let t =
      register_all
        (empty ~config ~trap_logs:false ~suite_name:"<not-shown-to-user>")
        tl
    in
    list_registered_tests t ();
    M.return ()

  let run_with_args' (config : Config.User.t) name (type a) (args : a)
      (tl : a test list) =
    let config =
      Config.apply_defaults ~default_log_dir:(default_log_dir ()) config
    in
    let t = empty ~config ~trap_logs:(not config#verbose) ~suite_name:name in
    let t = register_all t tl in
    let+ test_failures =
      (* Only print inside the concurrency monad *)
      let* () = M.return () in
      let open Fmt in
      pr "Testing %a.@," (Pp.quoted Fmt.(styled `Bold Suite.pp_name)) t.suite;
      pr "@[<v>%a@]"
        (styled `Faint (fun ppf () ->
             pf ppf "This run has ID %a.@,@," (Pp.quoted string) t.run_id))
        ();
      run_tests t () args
    in
    match (test_failures, t.config#and_exit) with
    | 0, true -> exit 0
    | 0, false -> ()
    | _, true -> exit 1
    | _, false -> raise Test_error

  let run' config name (tl : unit test list) = run_with_args' config name () tl

  let run_with_args ?and_exit ?verbose ?compact ?tail_errors ?quick_only
      ?show_errors ?json ?filter ?log_dir ?bail ?record_backtrace =
    Config.User.kcreate run_with_args' ?and_exit ?verbose ?compact ?tail_errors
      ?quick_only ?show_errors ?json ?filter ?log_dir ?bail ?record_backtrace

  let run = Config.User.kcreate run'
end

module V1 = struct
  include V1_types
  module Make = Make
end
