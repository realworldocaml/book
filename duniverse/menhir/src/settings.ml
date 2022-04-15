(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf

let error format =
  ksprintf (fun s -> prerr_string s; exit 1) format

(* ------------------------------------------------------------------------- *)
(* Prepare for parsing the command line. *)

let backend =
  ref `Unspecified

let set_backend b () =
  backend := b

type token_type_mode =
  | TokenTypeAndCode   (* produce the definition of the [token] type and code for the parser *)
  | TokenTypeOnly      (* produce the type definition only *)
  | CodeOnly of string (* produce the code only; import token type from specified module *)

let token_type_mode =
  ref TokenTypeAndCode

let tokentypeonly () =
  token_type_mode := TokenTypeOnly

let is_uppercase_ascii c =
  c >= 'A' && c <= 'Z'

let is_capitalized_ascii s =
  String.length s > 0 &&
  is_uppercase_ascii s.[0]

let codeonly m =
  if not (is_capitalized_ascii m) then begin
    (* Not using module [Error] to avoid a circular dependency. *)
    fprintf stderr "Error: %s is not a valid OCaml module name.\n" m;
    exit 1
  end;
  token_type_mode := CodeOnly m

let version =
  ref false

type construction_mode =
  | ModeCanonical     (* --canonical: canonical Knuth LR(1) automaton *)
  | ModeInclusionOnly (* --no-pager : states are merged when there is an inclusion
                                      relationship *)
  | ModePager         (* normal mode: states are merged as per Pager's criterion *)
  | ModeLALR          (* --lalr     : states are merged as in an LALR generator,
                                      i.e. as soon as they have the same LR(0) core *)

(* Note that --canonical overrides --no-pager. If both are specified, the result
   is a canonical automaton. *)

let construction_mode =
  ref ModePager

let explain =
  ref false

let base =
  ref ""

let dump =
  ref false

let dump_resolved =
  ref false

let reference_graph =
  ref false

let automaton_graph =
  ref false

let trace =
  ref false

let noprefix =
  ref false

type print_mode =
    | PrintNormal
    | PrintForOCamlyacc
    | PrintUnitActions of bool       (* if true, declare unit tokens *)

type preprocess_mode =
    | PMNormal                       (* preprocess and continue *)
    | PMOnlyPreprocess of print_mode (* preprocess, print grammar, stop *)

let preprocess_mode =
  ref PMNormal

let recovery =
  ref false

let v () =
  dump := true;
  explain := true

let inline =
  ref true

type infer_mode =
    (* Perform no type inference. This is the default mode. *)
  | IMNone
    (* Perform type inference by invoking ocamlc directly. *)
  | IMInfer                (* --infer *)
  | IMDependRaw            (* --raw-depend *)
  | IMDependPostprocess    (* --depend *)
    (* Perform type inference by writing a mock .ml file and
       reading the corresponding inferred .mli file. *)
  | IMWriteQuery of string (* --infer-write-query <filename> *)
  | IMReadReply of string  (* --infer-read-reply <filename> *)

let show_infer_mode = function
  | IMNone ->
      ""
  | IMInfer ->
      "--infer"
  | IMDependRaw ->
      "--raw-depend"
  | IMDependPostprocess ->
      "--depend"
  | IMWriteQuery _ ->
      "--infer-write-query"
  | IMReadReply _ ->
      "--infer-read-reply"

let infer =
  ref IMNone

let set_infer_mode mode2 =
  let mode1 = !infer in
  match mode1, mode2 with
  | IMNone, _ ->
      infer := mode2
  (* It is valid to specify [--infer] in conjunction with [--depend] or
     [--raw-depend]. The latter command then takes precedence. This is
     for compatibility with Menhir prior to 2018/05/23. *)
  | IMInfer, (IMInfer | IMDependRaw | IMDependPostprocess) ->
      infer := mode2
  | (IMDependRaw | IMDependPostprocess), IMInfer ->
      ()
  | _, _ ->
      fprintf stderr "Error: you cannot use both %s and %s.\n"
        (show_infer_mode mode1)
        (show_infer_mode mode2);
      exit 1

let enable_infer () =
  set_infer_mode IMInfer

let enable_depend () =
  set_infer_mode IMDependPostprocess

let enable_raw_depend () =
  set_infer_mode IMDependRaw

let enable_write_query filename =
  set_infer_mode (IMWriteQuery filename)

let enable_read_reply filename =
  set_infer_mode (IMReadReply filename)

let code_inlining =
  ref true

let represent_positions =
  ref false

let represent_states =
  ref false

let represent_values =
  ref false

let represent_everything () =
  represent_positions := true;
  represent_states := true;
  represent_values := true

let comment =
  ref false

let ocamlc =
  ref "ocamlc"

let ocamldep =
  ref "ocamldep"

let logG, logA, logC =
  ref 0, ref 0, ref 0

let timings =
  ref None

let filenames =
  ref StringSet.empty

let no_stdlib =
  ref false

let insert name =
  filenames := StringSet.add name !filenames

let interpret =
  ref false

let interpret_show_cst =
  ref false

let interpret_error =
  ref false

let optimization_level =
  ref 2

let inspection =
  ref false

let coq_no_version_check =
  ref false

let coq_no_complete =
  ref false

let coq_no_actions =
  ref false

let strict =
  ref false

let fixedexc =
  ref false

let exn_carries_state =
  ref false

type suggestion =
  | SuggestNothing
  | SuggestCompFlags
  | SuggestLinkFlags of string (* "cmo" or "cmx" *)
  | SuggestWhereIsMenhirLibSource
  | SuggestUseOcamlfind

let suggestion =
  ref SuggestNothing

let ignored_unused_tokens =
  ref StringSet.empty

let ignore_unused_token t =
  ignored_unused_tokens := StringSet.add t !ignored_unused_tokens

let ignore_all_unused_tokens =
  ref false

let ignore_all_unused_precedence_levels =
  ref false

let list_errors =
  ref false

type list_errors_algorithm = [
  | `Fast
  | `Classic
  | `Validate
]

let list_errors_algorithm =
  ref `Fast

let set_list_errors_algorithm (algorithm: string) : unit =
  let algorithm = match algorithm with
    | "fast"     -> `Fast
    | "classic"  -> `Classic
    | "validate" -> `Validate
    | algorithm ->
        error "Error: --list-errors-algorithm should be followed with \
               fast | classic | validate (got %S).\n" algorithm
  in
  list_errors_algorithm := algorithm

let compile_errors =
  ref None

let set_compile_errors filename =
  compile_errors := Some filename

let compare_errors =
  ref []

let add_compare_errors filename =
  compare_errors := filename :: !compare_errors

let merge_errors =
  ref []

let add_merge_errors filename =
  merge_errors := filename :: !merge_errors

let update_errors =
  ref None

let set_update_errors filename =
  update_errors := Some filename

let echo_errors =
  ref None

let set_echo_errors filename =
  echo_errors := Some filename

let echo_errors_concrete =
  ref None

let set_echo_errors_concrete filename =
  echo_errors_concrete := Some filename

let cmly =
  ref false

let coq_lib_path =
  ref (Some "MenhirLib")

type dollars =
  | DollarsDisallowed
  | DollarsAllowed

let dollars =
  ref DollarsAllowed

let require_aliases =
  ref false

let random_sentence_symbol =
  ref None

let random_sentence_goal =
  ref 0

let random_sentence_style =
  ref `Abstract

let random_sentence_abstract symbol =
  random_sentence_symbol := Some symbol;
  random_sentence_style := `Abstract

let random_sentence_concrete symbol =
  random_sentence_symbol := Some symbol;
  random_sentence_style := `Concrete;
  require_aliases := true

let strategy =
  ref `Unspecified

let set_strategy = function
  | "legacy" ->
      strategy := `Legacy
  | "simplified" ->
      strategy := `Simplified
  | _ ->
      error "Error: --strategy should be followed with legacy | simplified.\n"

let stacklang_dump =
  ref false

let stacklang_graph =
  ref false

let stacklang_test =
  ref false

(* When new command line options are added, please update both the manual
   in [doc/manual.tex] and the man page in [doc/menhir.1]. *)

(* Please note that there is a very short length limit on the explanations
   here, since the output of [menhir -help] must fit in 80 columns. *)

let options = Arg.align [
  "--automaton-graph", Arg.Set automaton_graph, " (undocumented)";
  "--base", Arg.Set_string base, "<basename> Specifies a base name for the output file(s)";
  "--canonical", Arg.Unit (fun () -> construction_mode := ModeCanonical), " Construct a canonical Knuth LR(1) automaton";
  "--cmly", Arg.Set cmly, " Write a .cmly file";
  "--code", Arg.Unit (set_backend `NewCodeBackend), " Use the code back-end (default)";
  "--code-ancient", Arg.Unit (set_backend `OldCodeBackend), " Use the ancient code back-end";
  "--comment", Arg.Set comment, " Include comments in the generated code";
  "--compare-errors", Arg.String add_compare_errors, "<filename> (used twice) Compare two .messages files";
  "--compile-errors", Arg.String set_compile_errors, "<filename> Compile a .messages file to OCaml code";
  "--coq", Arg.Unit (set_backend `CoqBackend), " Generate a formally verified parser, in Coq";
  "--coq-lib-path", Arg.String (fun path -> coq_lib_path := Some path), "<path> How to qualify references to MenhirLib";
  "--coq-lib-no-path", Arg.Unit (fun () -> coq_lib_path := None), " Do *not* qualify references to MenhirLib";
  "--coq-no-version-check", Arg.Set coq_no_version_check, " Do not generate a version check.";
  "--coq-no-actions", Arg.Set coq_no_actions, " Ignore semantic actions in the Coq output";
  "--coq-no-complete", Arg.Set coq_no_complete, " Do not generate a proof of completeness";
  "--depend", Arg.Unit enable_depend, " Invoke ocamldep and display dependencies";
  "--dump", Arg.Set dump, " Write an .automaton file";
  "--dump-resolved", Arg.Set dump_resolved, " Write an .automaton.resolved file";
  "--echo-errors", Arg.String set_echo_errors, "<filename> Echo the sentences in a .messages file";
  "--echo-errors-concrete", Arg.String set_echo_errors_concrete, "<filename> Echo the sentences in a .messages file";
  "--error-recovery", Arg.Set recovery, " (no longer supported)";
  "--exn-carries-state", Arg.Set exn_carries_state, " Declares exception Error of int";
  "--explain", Arg.Set explain, " Explain conflicts in <basename>.conflicts";
  "--external-tokens", Arg.String codeonly, "<module> Import token type definition from <module>";
  "--fixed-exception", Arg.Set fixedexc, " Declares Error = Parsing.Parse_error";
  "--infer", Arg.Unit enable_infer, " Invoke ocamlc to do type inference";
  "--infer-protocol-supported", Arg.Unit (fun () -> exit 0), " Stop with exit code 0";
  "--infer-write-query", Arg.String enable_write_query, "<filename> Write mock .ml file";
  "--infer-read-reply", Arg.String enable_read_reply, "<filename> Read inferred .mli file";
  "--inspection", Arg.Set inspection, " Generate the inspection API";
  "--interpret", Arg.Set interpret, " Interpret the sentences provided on stdin";
  "--interpret-show-cst", Arg.Set interpret_show_cst, " Show a concrete syntax tree upon acceptance";
  "--interpret-error", Arg.Set interpret_error, " Interpret an error sentence";
  "--lalr", Arg.Unit (fun () -> construction_mode := ModeLALR), " Construct an LALR(1) automaton";
  "--list-errors", Arg.Set list_errors, " Produce a list of erroneous inputs";
  "--list-errors-algorithm", Arg.String set_list_errors_algorithm, " (undocumented)";
  "--log-automaton", Arg.Set_int logA, "<level> Log information about the automaton";
  "--log-code", Arg.Set_int logC, "<level> Log information about the generated code";
  "--log-grammar", Arg.Set_int logG, "<level> Log information about the grammar";
  "--merge-errors", Arg.String add_merge_errors, "<filename> (used twice) Merge two .messages files";
  "--no-code-inlining", Arg.Clear code_inlining, " (undocumented)";
  "--no-dollars", Arg.Unit (fun () -> dollars := DollarsDisallowed), " Disallow $i in semantic actions";
  "--no-inline", Arg.Clear inline, " Ignore the %inline keyword";
  "--no-pager", Arg.Unit (fun () -> if !construction_mode = ModePager then construction_mode := ModeInclusionOnly), " (undocumented)";
  "--no-prefix", Arg.Set noprefix, " (undocumented)";
  "--no-stdlib", Arg.Set no_stdlib, " Do not load the standard library";
  "--ocamlc", Arg.Set_string ocamlc, "<command> Specifies how ocamlc should be invoked";
  "--ocamldep", Arg.Set_string ocamldep, "<command> Specifies how ocamldep should be invoked";
  "--only-preprocess", Arg.Unit (fun () -> preprocess_mode := PMOnlyPreprocess PrintNormal),
                       " Print grammar and exit";
  "--only-preprocess-for-ocamlyacc", Arg.Unit (fun () -> preprocess_mode := PMOnlyPreprocess PrintForOCamlyacc),
                       " Print grammar in ocamlyacc format and exit";
  "--only-preprocess-u", Arg.Unit (fun () -> preprocess_mode := PMOnlyPreprocess (PrintUnitActions false)),
                         " Print grammar with unit actions and exit";
  "--only-preprocess-uu", Arg.Unit (fun () -> preprocess_mode := PMOnlyPreprocess (PrintUnitActions true)),
                          " Print grammar with unit actions & tokens";
  "--only-tokens", Arg.Unit tokentypeonly, " Generate token type definition only, no code";
  "--random-seed", Arg.Int Random.init, "<seed> Set the random seed";
  "--random-self-init", Arg.Unit Random.self_init, " Pick a random seed in a system-dependent way";
  "--random-sentence-length", Arg.Set_int random_sentence_goal, "<length> Set the goal length for a random sentence";
  "--random-sentence", Arg.String random_sentence_abstract, "<sym> Generate a random valid sentence";
  "--random-sentence-concrete", Arg.String random_sentence_concrete, "<sym> Generate a random valid sentence";
  "--raw-depend", Arg.Unit enable_raw_depend, " Invoke ocamldep and echo its raw output";
  "--reference-graph", Arg.Set reference_graph, " (undocumented)";
  "--represent-states", Arg.Set represent_states, " (undocumented)";
  "--represent-positions", Arg.Set represent_positions, " (undocumented)";
  "--represent-values", Arg.Set represent_values, " (undocumented)";
  "--represent-everything", Arg.Unit represent_everything, " (undocumented)";
  "--require-aliases", Arg.Set require_aliases, " Check that every token has a token alias";
  "--stacklang-dump", Arg.Set stacklang_dump, " (undocumented)";
  "--stacklang-graph", Arg.Set stacklang_graph, " (undocumented)";
  "--stacklang-test", Arg.Set stacklang_test, " (undocumented)";
  "--stdlib", Arg.String ignore, "<directory> Ignored (deprecated)";
  "--strategy", Arg.String set_strategy, "<strategy> Choose an error-handling strategy";
  "--strict", Arg.Set strict, " Warnings about the grammar are errors";
  "--suggest-comp-flags", Arg.Unit (fun () -> suggestion := SuggestCompFlags),
                          " Suggest compilation flags for ocaml{c,opt}";
  "--suggest-link-flags-byte", Arg.Unit (fun () -> suggestion := SuggestLinkFlags "cma"),
                               " Suggest link flags for ocamlc";
  "--suggest-link-flags-opt", Arg.Unit (fun () -> suggestion := SuggestLinkFlags "cmxa"),
                              " Suggest link flags for ocamlopt";
  "--suggest-menhirLib", Arg.Unit (fun () -> suggestion := SuggestWhereIsMenhirLibSource),
                         " Suggest where is MenhirLib";
  "--suggest-ocamlfind", Arg.Unit (fun () -> suggestion := SuggestUseOcamlfind),
                         " (deprecated)";
  "--table", Arg.Unit (set_backend `TableBackend), " Use the table back-end";
  "--timings", Arg.Unit (fun () -> timings := Some stderr), " Output internal timings to stderr";
  "--timings-to", Arg.String (fun filename -> timings := Some (open_out filename)), "<filename> Output internal timings to <filename>";
  "--trace", Arg.Set trace, " Generate tracing instructions";
  "--unused-precedence-levels", Arg.Set ignore_all_unused_precedence_levels, " Do not warn about unused precedence levels";
  "--unused-token", Arg.String ignore_unused_token, "<token> Do not warn that <token> is unused";
  "--unused-tokens", Arg.Set ignore_all_unused_tokens, " Do not warn about any unused token";
  "--update-errors", Arg.String set_update_errors, "<filename> Update auto-comments in a .messages file";
  "--version", Arg.Set version, " Show version number and exit";
  "-b", Arg.Set_string base, "<basename> Synonymous with --base <basename>";
  "-lg", Arg.Set_int logG, " Synonymous with --log-grammar";
  "-la", Arg.Set_int logA, " Synonymous with --log-automaton";
  "-lc", Arg.Set_int logC, " Synonymous with --log-code";
  "-O", Arg.Set_int optimization_level, " (0|1|2) Set optimization level";
  "-t", Arg.Unit (set_backend `TableBackend), " Synonymous with --table";
  "-v", Arg.Unit v, " Synonymous with --dump --explain";
]

let usage =
  sprintf "Usage: %s <options> <filenames>" Sys.argv.(0)

(* ------------------------------------------------------------------------- *)
(* Parse the command line. *)

let () =
  Arg.parse options insert usage

(* ------------------------------------------------------------------------- *)
(* If required, print a version number and stop. *)

let () =
  if !version then begin
    printf "menhir, version %s\n" Version.version;
    exit 0
  end

(* ------------------------------------------------------------------------- *)

(* Decide which back-end is used. *)

let backend =
  (* If any of the [--interpret] flags is used, then we consider that
     the back-end is [`ReferenceInterpreter]. Indeed, in that case,
     we are not using any of the other back-ends. *)
  if !interpret || !interpret_error || !interpret_show_cst then
    `ReferenceInterpreter
  else
    match !backend with
    | `Unspecified ->
        (* The new code back-end is the default. *)
        `NewCodeBackend
    | `CoqBackend
    | `NewCodeBackend
    | `OldCodeBackend
    | `TableBackend
      as backend ->
        backend

let extension =
  match backend with
  | `CoqBackend ->
      ".vy"
  | `ReferenceInterpreter
  | `NewCodeBackend
  | `OldCodeBackend
  | `TableBackend ->
      ".mly"

let print_backend backend =
  match backend with
  | `ReferenceInterpreter ->
      "reference interpreter"
  | `OldCodeBackend ->
      "ancient code back-end"
  | `NewCodeBackend ->
      "new code back-end"
  | `CoqBackend ->
      "Coq back-end"
  | `TableBackend ->
      "table back-end"

let print_strategy strategy =
  match strategy with
  | `Legacy ->
      "legacy"
  | `Simplified ->
      "simplified"

let strategy =
  let strategy = !strategy in
  match strategy, backend with
  | (`Unspecified | `Simplified), `NewCodeBackend ->
      (* The new code back-end supports only the simplified strategy. *)
      `Simplified
  | (`Unspecified | `Legacy), `OldCodeBackend ->
      (* The old code back-end supports only the legacy strategy. *)
      `Legacy
  | `Simplified, (`ReferenceInterpreter | `TableBackend) ->
      (* The reference interpreter and the table back-end support both
         strategies. *)
      `Simplified
  | (`Unspecified | `Legacy), (`ReferenceInterpreter | `TableBackend) ->
      (* The reference interpreter and the table back-end support both
         strategies; legacy is the default, for backward
         compatibility. *)
      `Legacy
  | _, `CoqBackend ->
      (* The Coq back-end does not care. *)
      `Legacy
  | (`Legacy as strategy), `NewCodeBackend
  | (`Simplified as strategy), `OldCodeBackend ->
      error "Error: the %s does not allow --strategy %s.\n"
        (print_backend backend)
        (print_strategy strategy)

(* ------------------------------------------------------------------------- *)

(* [--exn-carries-state] is supported only by the new code back-end,
   and is incompatible with [--fixed-exception]. *)

let () =
  if !exn_carries_state
  && backend <> `NewCodeBackend then
    error
      "Error: --exn-carries-state is supported only by the code back-end.\n"

let () =
  if !exn_carries_state
  && !fixedexc then
    error
      "Error: --fixed-exception and --exn-carries-state are incompatible.\n"

(* ------------------------------------------------------------------------- *)

(* Menhir is able to suggest compile and link flags to be passed to the
   OCaml compilers. If required, do so and stop. *)

(* If [--table] is not passed, no flags are necessary. If [--table] is
   passed, then [MenhirLib] needs to be visible (at compile time) and
   linked in (at link time). *)

(* The compilation flags are in fact meant to be used both at compile-
   and link-time. *)

let () =
  match !suggestion with
  | SuggestNothing ->
      ()
  | SuggestCompFlags ->
      if backend = `TableBackend then
        printf "-I %s\n%!" (Installation.libdir());
      exit 0
  | SuggestLinkFlags extension ->
      if backend = `TableBackend then
        printf "menhirLib.%s\n%!" extension;
      exit 0
  | SuggestWhereIsMenhirLibSource ->
      printf "%s\n%!" (Installation.libdir());
      exit 0
  | SuggestUseOcamlfind ->
      printf "false\n";
      exit 0

(* ------------------------------------------------------------------------- *)
(* Export the settings. *)

let stdlib_filename =
  "<standard.mly>"

let filenames =
  StringSet.elements !filenames

let base =
  if !base = "" then
    match filenames with
    | [] ->
        fprintf stderr "%s\n" usage;
        exit 1
    | [ filename ] ->
        Filename.chop_suffix filename extension
    | _ ->
        fprintf stderr "Error: you must specify --base when providing multiple input files.\n";
        exit 1
  else
    !base

let token_type_mode =
  !token_type_mode

let construction_mode =
  !construction_mode

let explain =
  !explain

let dump =
  !dump

let dump_resolved =
  !dump_resolved

let reference_graph =
  !reference_graph

let automaton_graph =
  !automaton_graph

let trace =
  !trace

let () =
  if !recovery then begin
    fprintf stderr "Error: --error-recovery mode is no longer supported.\n";
    exit 1
  end

let noprefix =
  !noprefix

let code_inlining =
  !code_inlining

let inline =
  !inline

let comment =
  !comment

let preprocess_mode =
  !preprocess_mode

let ocamlc =
  !ocamlc

let ocamldep =
  !ocamldep

let logG, logA, logC =
  !logG, !logA, !logC

let timings =
  !timings

let interpret =
  !interpret

let interpret_show_cst =
  !interpret_show_cst

let interpret_error =
  !interpret_error

let optimization_level =
  !optimization_level

let inspection =
  !inspection

let () =
  if inspection && backend <> `TableBackend then begin
    fprintf stderr "Error: --inspection requires --table.\n";
    exit 1
  end

let no_stdlib =
  !no_stdlib

let coq_no_version_check =
  !coq_no_version_check

let coq_no_complete =
  !coq_no_complete

let coq_no_actions =
  !coq_no_actions

let strict =
  !strict

let fixedexc =
  !fixedexc

let exn_carries_state =
  !exn_carries_state

let ignored_unused_tokens =
  !ignored_unused_tokens

let ignore_all_unused_tokens =
  !ignore_all_unused_tokens

let ignore_all_unused_precedence_levels =
  !ignore_all_unused_precedence_levels

let list_errors =
  !list_errors

let list_errors_algorithm : list_errors_algorithm =
  !list_errors_algorithm

let compile_errors =
  !compile_errors

let compare_errors =
  match !compare_errors with
  | [] ->
      None
  | [ filename2; filename1 ] -> (* LIFO *)
      Some (filename1, filename2)
  | _ ->
      error
        "To compare two .messages files, please use:\n\
         --compare-errors <filename1> --compare-errors <filename2>.\n"

let merge_errors =
  match !merge_errors with
  | [] ->
      None
  | [ filename2; filename1 ] -> (* LIFO *)
      Some (filename1, filename2)
  | _ ->
      error
        "To merge two .messages files, please use:\n\
         --merge-errors <filename1> --merge-errors <filename2>.\n"

let update_errors =
  !update_errors

let echo_errors =
  !echo_errors

let echo_errors_concrete =
  !echo_errors_concrete

let cmly =
  !cmly

let coq_lib_path =
  !coq_lib_path

let dollars =
  !dollars

let require_aliases =
  !require_aliases

let random_sentence =
  match !random_sentence_symbol with
  | None ->
      None
  | Some nt ->
      let goal = !random_sentence_goal
      and style = !random_sentence_style in
      Some (nt, goal, style)

let infer =
  !infer

(* If some flags imply that we will NOT produce an OCaml parser, then there is
   no need to perform type inference, so [--infer] is ignored. This saves time
   and dependency nightmares. *)

let skipping_parser_generation =
  backend = `ReferenceInterpreter ||
  backend = `CoqBackend ||
  compile_errors <> None ||
  list_errors ||
  compare_errors <> None ||
  merge_errors <> None ||
  update_errors <> None ||
  echo_errors <> None ||
  false
    (* maybe also: [preprocess_mode <> PMNormal] *)

let infer =
  match infer with
  | IMInfer when skipping_parser_generation ->
      IMNone
  | _ ->
      infer

(* [--table] or [--coq] implies [--represent-everything]. Indeed, only the
   code back-ends need clever computations of which states, values, and
   positions are represented as part of stack cells. *)

let () =
  match backend with
  | `ReferenceInterpreter
  | `TableBackend
  | `CoqBackend ->
      represent_everything()
  | `OldCodeBackend
  | `NewCodeBackend ->
      ()

let represent_positions =
  !represent_positions

let represent_states =
  !represent_states

let represent_values =
  !represent_values

let stacklang_dump =
  !stacklang_dump

let stacklang_graph =
  !stacklang_graph

let stacklang_test =
  !stacklang_test
