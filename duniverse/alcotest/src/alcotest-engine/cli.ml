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

include Cli_intf
open! Import
open Cmdliner

module Make (P : Platform.MAKER) (M : Monad.S) :
  V1_types.S with type return = unit M.t = struct
  (**  *)

  (** The priority order for determining options should be as follows:

      + 1. if a CLI flag/option is _explicitly_ set, use that;
      + 2. if the corresponding environment variable is _explicitly_ set, use
        that;
      + 3. if the flag/option is set by [run ?argv]
      + 4. if the flag/option is passed to [run] directly, use that;
      + 5. otherwise, use the default behaviour set by {!Alcotest.Core}. *)

  module C = Core.V1.Make (P) (M)
  include C
  module P = P (M)
  open Cmdliner_syntax

  let set_color =
    let env = Arg.env_var "ALCOTEST_COLOR" in
    let+ color_flag =
      let enum = [ ("auto", `Auto); ("always", `Ansi_tty); ("never", `None) ] in
      let color = Arg.enum enum in
      let enum_alts = Arg.doc_alts_enum enum in
      let doc =
        Fmt.str
          "Colorize the output. $(docv) must be %s. Defaults to %s when \
           running inside Dune, otherwise defaults to %s."
          enum_alts (Arg.doc_quote "always") (Arg.doc_quote "auto")
      in
      Arg.(
        value & opt (some color) None & info [ "color" ] ~env ~doc ~docv:"WHEN")
    in
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

  let default_cmd config args library_name tests =
    let and_exit = Config.User.and_exit config
    and record_backtrace = Config.User.record_backtrace config in
    let exec_name = Filename.basename Sys.argv.(0) in
    let doc = "Run all the tests." in
    let term =
      let+ () = set_color
      and+ cli_config = Config.User.term ~and_exit ~record_backtrace
      and+ args = args in
      let config = Config.User.(cli_config || config) in
      run_with_args' config library_name args tests
    in
    (term, Term.info exec_name ~doc)

  let test_cmd config args library_name tests =
    let doc = "Run a subset of the tests." in
    let term =
      let+ () = set_color
      and+ cli_config = Config.User.term ~and_exit:true ~record_backtrace:true
      and+ args = args in
      let config = Config.User.(cli_config || config) in
      run_with_args' config library_name args tests
    in
    (term, Term.info "test" ~doc)

  let list_cmd tests =
    let doc = "List all available tests." in
    ( (let+ () = set_color in
       list_tests tests),
      Term.info "list" ~doc )

  let run_with_args' (type a) ~argv config name (args : a Term.t)
      (tl : a test list) =
    let ( >>= ) = M.bind in
    let choices = [ list_cmd tl; test_cmd config args name tl ] in
    let and_exit = Config.User.and_exit config in
    let exit_or_return result =
      if and_exit then exit (Term.exit_status_of_result result) else M.return ()
    in
    let result =
      Term.eval_choice ?argv
        ~catch:and_exit (* Only log exceptions not raised to the user code *)
        (default_cmd config args name tl)
        choices
    in
    match result with
    | `Ok unit_m -> unit_m >>= fun () -> exit_or_return (`Ok ())
    | (`Help | `Version | `Error `Exn) as result -> exit_or_return result
    | `Error (`Parse | `Term) as result ->
        exit (Term.exit_status_of_result result)

  let run_with_args ?and_exit ?verbose ?compact ?tail_errors ?quick_only
      ?show_errors ?json ?filter ?log_dir ?bail ?record_backtrace ?argv =
    Config.User.kcreate (run_with_args' ~argv) ?and_exit ?verbose ?compact
      ?tail_errors ?quick_only ?show_errors ?json ?filter ?log_dir ?bail
      ?record_backtrace

  let run =
    Config.User.kcreate (fun config ?argv name tl ->
        run_with_args' config ~argv name (Term.pure ()) tl)
end

module V1 = struct
  include V1_types
  module Make = Make
end
