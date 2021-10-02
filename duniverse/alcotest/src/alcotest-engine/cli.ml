(*
 * Copyright (c) 2013-2016 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2019 Craig Ferguson <me@craigfe.io>
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

open Cmdliner
open Astring

module IntSet = Set.Make (struct
  type t = int

  let compare = (compare : int -> int -> int)
end)

module type S = sig
  include Core.S

  val run :
    (?argv:string array -> string -> unit test list -> return) with_options

  val run_with_args :
    (?argv:string array ->
    string ->
    'a Cmdliner.Term.t ->
    'a test list ->
    return)
    with_options
end

module type MAKER = functor (P : Platform.MAKER) (M : Monad.S) ->
  S with type return = unit M.t

module Make (P : Platform.MAKER) (M : Monad.S) : S with type return = unit M.t =
struct
  (**  *)

  (** The priority order for determining options should be as follows:

      + 1. if a CLI flag/option is _explicitly_ set, use that;
      + 2. if the corresponding environment variable is _explicitly_ set, use
        that;
      + 3. if the flag/option is set by [run ?argv]
      + 4. if the flag/option is passed to [run] directly, use that;
      + 5. otherwise, use the default behaviour set by {!Alcotest.Core}. *)

  module C = Core.Make (P) (M)
  include C
  module P = P (M)

  let set_color color_flag =
    let style_renderer =
      match color_flag with
      | Some `Auto -> None
      | Some (`Ansi_tty | `None) as a -> a
      | None -> (
          try
            (* Default to [always] when running inside Dune *)
            let (_ : string) = Sys.getenv "INSIDE_DUNE" in
            Some `Ansi_tty
          with Not_found -> None)
    in
    P.setup_std_outputs ?style_renderer ()

  let set_color =
    let env = Arg.env_var "ALCOTEST_COLOR" in
    let style_renderer =
      let enum = [ ("auto", `Auto); ("always", `Ansi_tty); ("never", `None) ] in
      let color = Arg.enum enum in
      let enum_alts = Arg.doc_alts_enum enum in
      let doc =
        strf
          "Colorize the output. $(docv) must be %s. Defaults to %s when \
           running inside Dune, otherwise defaults to %s."
          enum_alts (Arg.doc_quote "always") (Arg.doc_quote "auto")
      in

      Arg.(
        value & opt (some color) None & info [ "color" ] ~env ~doc ~docv:"WHEN")
    in
    Term.(const set_color $ style_renderer)

  type runtime_options = {
    verbose : bool option;
    compact : bool option;
    tail_errors : [ `Unlimited | `Limit of int ] option;
    show_errors : bool option;
    quick_only : bool option;
    json : bool option;
    log_dir : string option;
    bail : bool option;
  }

  (* Merge two ['a option]s with a left [Some] taking priority *)
  let ( ||* ) a b = match (a, b) with Some a, _ -> Some a | None, b -> b

  let v_runtime_flags ~defaults (`Verbose verbose) (`Compact compact)
      (`Tail_errors tail_errors) (`Show_errors show_errors)
      (`Quick_only quick_only) (`Json json) (`Log_dir log_dir) (`Bail bail) =
    let verbose = verbose ||* defaults.verbose in
    let compact = compact ||* defaults.compact in
    let show_errors = show_errors ||* defaults.show_errors in
    let quick_only = quick_only ||* defaults.quick_only in
    let json = json ||* defaults.json in
    let log_dir = Some log_dir in
    let tail_errors = tail_errors ||* defaults.tail_errors in
    let bail = bail ||* defaults.bail in
    {
      verbose;
      compact;
      tail_errors;
      show_errors;
      quick_only;
      json;
      log_dir;
      bail;
    }

  let run_test ?and_exit
      {
        verbose;
        compact;
        tail_errors;
        show_errors;
        quick_only;
        json;
        log_dir;
        bail;
      } (`Test_filter filter) () tests name args =
    run_with_args ?and_exit ?verbose ?compact ?tail_errors ?quick_only
      ?show_errors ?json ?filter ?log_dir ?bail name tests args

  let fmap f x = Term.(app (const f) x)

  (* If a Cmdliner flag is _not_ set, we interpret it as 'use the program
     default' rather than an explicit 'disable'. This changes the type of
     {!Cmdliner.Arg.flag} to reflect that fact. *)
  let to_tristate = fmap (function true -> Some true | false -> None)

  let json =
    let doc = "Display JSON for the results, to be used by a script." in
    Arg.(value & flag & info [ "json" ] ~docv:"" ~doc)
    |> to_tristate
    |> fmap (fun x -> `Json x)

  let log_dir =
    let fname_concat l = List.fold_left Filename.concat "" l in
    let default_dir = fname_concat [ P.getcwd (); "_build"; "_tests" ] in
    let doc = "Where to store the log files of the tests." in
    Arg.(value & opt dir default_dir & info [ "o" ] ~docv:"DIR" ~doc)
    |> fmap (fun x -> `Log_dir x)

  let verbose =
    let env = Arg.env_var "ALCOTEST_VERBOSE" in
    let doc =
      "Display the test outputs. $(b,WARNING:) when using this option the \
       output logs will not be available for further inspection."
    in
    Arg.(value & flag & info ~env [ "v"; "verbose" ] ~docv:"" ~doc)
    |> to_tristate
    |> fmap (fun x -> `Verbose x)

  let compact =
    let env = Arg.env_var "ALCOTEST_COMPACT" in
    let doc = "Compact the output of the tests." in
    Arg.(value & flag & info ~env [ "c"; "compact" ] ~docv:"" ~doc)
    |> to_tristate
    |> fmap (fun x -> `Compact x)

  let bail =
    let env = Arg.env_var "ALCOTEST_BAIL" in
    let doc = "Stop running tests after the first failure." in
    Arg.(value & flag & info ~env [ "bail" ] ~docv:"" ~doc)
    |> to_tristate
    |> fmap (fun x -> `Bail x)

  let limit_parser s =
    match s with
    | "unlimited" -> Ok `Unlimited
    | s -> (
        try
          let n = int_of_string s in
          if n < 0 then
            Error (`Msg "numeric limit must be nonnegative or 'unlimited'")
          else Ok (`Limit n)
        with Failure _ -> Error (`Msg "invalid numeric limit"))

  let limit_printer ppf limit =
    match limit with
    | `Unlimited -> Fmt.pf ppf "unlimited"
    | `Limit n -> Fmt.pf ppf "%i" n

  (* Parse/print a nonnegative number of lines or "unlimited". *)
  let limit = Cmdliner.Arg.conv (limit_parser, limit_printer)

  let tail_errors =
    let env = Arg.env_var "ALCOTEST_TAIL_ERRORS" in
    let doc =
      "Show only the last $(docv) lines of output in case of an error."
    in
    Arg.(
      value & opt (some limit) None & info ~env [ "tail-errors" ] ~docv:"N" ~doc)
    |> fmap (fun x -> `Tail_errors x)

  let show_errors =
    let env = Arg.env_var "ALCOTEST_SHOW_ERRORS" in
    let doc = "Display the test errors." in
    Arg.(value & flag & info ~env [ "e"; "show-errors" ] ~docv:"" ~doc)
    |> to_tristate
    |> fmap (fun x -> `Show_errors x)

  let quick_only =
    let env = Arg.env_var "ALCOTEST_QUICK_TESTS" in
    let doc = "Run only the quick tests." in
    Arg.(value & flag & info ~env [ "q"; "quick-tests" ] ~docv:"" ~doc)
    |> to_tristate
    |> fmap (fun x -> `Quick_only x)

  let flags_with_defaults defaults =
    Term.(
      pure (v_runtime_flags ~defaults)
      $ verbose
      $ compact
      $ tail_errors
      $ show_errors
      $ quick_only
      $ json
      $ log_dir
      $ bail)

  let regex =
    let parse s =
      try Ok Re.(compile @@ Pcre.re s) with
      | Re.Perl.Parse_error -> Error (`Msg "Perl-compatible regexp parse error")
      | Re.Perl.Not_supported -> Error (`Msg "unsupported regexp feature")
    in
    let print = Re.pp_re in
    Arg.conv (parse, print)

  exception Invalid_format

  let int_range_list : int list Cmdliner.Arg.conv =
    let parse s =
      let rec range lower upper acc =
        if lower > upper then acc else range (succ lower) upper (lower :: acc)
      in
      let process_range acc s =
        String.cuts ~sep:".." s |> List.map String.to_int |> function
        | [ Some i ] -> i :: acc
        | [ Some lower; Some upper ] when lower <= upper ->
            range lower upper acc
        | _ -> raise Invalid_format
      in
      let ranges = String.cuts ~sep:"," s in
      match List.fold_left process_range [] ranges with
      | list -> Ok list
      | exception Invalid_format ->
          Error
            (`Msg "must be a comma-separated list of integers / integer ranges")
    in
    let print ppf set = Fmt.(braces @@ list ~sep:comma int) ppf set in
    Arg.conv (parse, print)

  let test_filter =
    let name_regex =
      let doc = "A regular expression matching the names of tests to run" in
      Arg.(value & pos 0 (some regex) None & info [] ~doc ~docv:"NAME_REGEX")
    in
    let number_filter =
      let doc =
        "A comma-separated list of test case numbers (and ranges of numbers) \
         to run, e.g: '4,6-10,19'"
      in
      Arg.(
        value
        & pos 1 (some int_range_list) None
        & info [] ~doc ~docv:"TESTCASES")
    in
    Term.(
      pure (fun n t -> `Test_filter (Some (n, t))) $ name_regex $ number_filter)

  let default_cmd ?and_exit runtime_flags args library_name tests =
    let exec_name = Filename.basename Sys.argv.(0) in
    let doc = "Run all the tests." in
    let flags = flags_with_defaults runtime_flags in
    ( Term.(
        pure (run_test ?and_exit)
        $ flags
        $ pure (`Test_filter None)
        $ set_color
        $ args
        $ pure library_name
        $ pure tests),
      Term.info exec_name ~doc )

  let test_cmd ?and_exit runtime_flags ~filter args library_name tests =
    let doc = "Run a subset of the tests." in
    let flags = flags_with_defaults runtime_flags in
    let filter =
      Term.(
        pure (fun a -> match a with `Test_filter None -> filter | _ -> a)
        $ test_filter)
    in
    ( Term.(
        pure (run_test ?and_exit)
        $ flags
        $ filter
        $ set_color
        $ args
        $ pure library_name
        $ pure tests),
      Term.info "test" ~doc )

  let list_cmd tests =
    let doc = "List all available tests." in
    ( Term.(pure (fun () -> list_tests) $ set_color $ pure tests),
      Term.info "list" ~doc )

  let run_with_args ?(and_exit = true) ?verbose ?compact ?tail_errors
      ?quick_only ?show_errors ?json ?filter ?log_dir ?bail ?argv name
      (args : 'a Term.t) (tl : 'a test list) =
    let ( >>= ) = M.bind in
    let runtime_flags =
      {
        verbose;
        compact;
        tail_errors;
        show_errors;
        quick_only;
        json;
        log_dir;
        bail;
      }
    in
    let choices =
      [
        list_cmd tl;
        test_cmd ~and_exit runtime_flags ~filter:(`Test_filter filter) args name
          tl;
      ]
    in
    let exit_or_return result =
      if and_exit then exit (Term.exit_status_of_result result) else M.return ()
    in
    let result =
      Term.eval_choice ?argv
        ~catch:and_exit (* Only log exceptions not raised to the user code *)
        (default_cmd ~and_exit runtime_flags args name tl)
        choices
    in
    match result with
    | `Ok unit_m -> unit_m >>= fun () -> exit_or_return (`Ok ())
    | (`Help | `Version | `Error `Exn) as result -> exit_or_return result
    | `Error (`Parse | `Term) as result ->
        exit (Term.exit_status_of_result result)

  let run ?and_exit ?verbose ?compact ?tail_errors ?quick_only ?show_errors
      ?json ?filter ?log_dir ?bail ?argv name tl =
    run_with_args ?and_exit ?verbose ?compact ?tail_errors ?quick_only
      ?show_errors ?json ?filter ?log_dir ?bail ?argv name (Term.pure ()) tl
end
