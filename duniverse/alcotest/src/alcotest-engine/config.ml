include Config_intf
include Config_intf.Types
open! Import
open Cmdliner_syntax

(* Keys are configuration properties, which have defaults and may be
   customisable via a CLI. *)
module Key = struct
  module type S = sig
    type t

    val default : t
  end

  module type Cli = sig
    include S

    val term : t option Cmdliner.Term.t
  end

  module Flag (X : sig
    val term : bool Cmdliner.Term.t
  end) : Cli with type t = bool = struct
    type t = bool

    let default = false

    (* If a Cmdliner flag is _not_ set, we interpret it as 'use the program
       default' rather than an explicit 'disable'. This changes the type of
       {!Cmdliner.Arg.flag} to reflect that fact. *)
    let term = X.term >>| function true -> Some true | false -> None
  end

  (** {1 Definitions of supported keys} *)

  module Arg = Cmdliner.Arg

  module And_exit = struct
    type t = bool

    let default = true
  end

  module Record_backtrace = struct
    type t = bool

    let default = true
  end

  module Verbose = Flag (struct
    let term =
      let env = Arg.env_var "ALCOTEST_VERBOSE" in
      let doc =
        "Display the test outputs. $(b,WARNING:) when using this option the \
         output logs will not be available for further inspection."
      in
      Arg.(value & flag & info ~env [ "v"; "verbose" ] ~docv:"" ~doc)
  end)

  module Compact = Flag (struct
    let term =
      let env = Arg.env_var "ALCOTEST_COMPACT" in
      let doc = "Compact the output of the tests." in
      Arg.(value & flag & info ~env [ "c"; "compact" ] ~docv:"" ~doc)
  end)

  module Bail = Flag (struct
    let term =
      let env = Arg.env_var "ALCOTEST_BAIL" in
      let doc = "Stop running tests after the first failure." in
      Arg.(value & flag & info ~env [ "bail" ] ~docv:"" ~doc)
  end)

  module Json = Flag (struct
    let term =
      let doc = "Display JSON for the results, to be used by a script." in
      Arg.(value & flag & info [ "json" ] ~docv:"" ~doc)
  end)

  module Show_errors = Flag (struct
    let term =
      let env = Arg.env_var "ALCOTEST_SHOW_ERRORS" in
      let doc = "Display the test errors." in
      Arg.(value & flag & info ~env [ "e"; "show-errors" ] ~docv:"" ~doc)
  end)

  module Quick_only = Flag (struct
    let term =
      let env = Arg.env_var "ALCOTEST_QUICK_TESTS" in
      let doc = "Run only the quick tests." in
      Arg.(value & flag & info ~env [ "q"; "quick-tests" ] ~docv:"" ~doc)
  end)

  module Tail_errors = struct
    type t = bound

    let default = `Unlimited

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
    let limit = Arg.conv (limit_parser, limit_printer)

    let term =
      let env = Arg.env_var "ALCOTEST_TAIL_ERRORS" in
      let doc =
        "Show only the last $(docv) lines of output in case of an error."
      in
      Arg.(
        value
        & opt (some limit) None
        & info ~env [ "tail-errors" ] ~docv:"N" ~doc)
  end

  module Log_dir = struct
    type t = string option

    let term =
      let doc = "Where to store the log files of the tests." in
      Arg.(value & opt (some dir) None & info [ "o" ] ~docv:"DIR" ~doc)
  end

  module Filter = struct
    type t = filter

    let regex : Re.re Arg.conv =
      let parse s =
        try Ok Re.(compile @@ Pcre.re s) with
        | Re.Perl.Parse_error ->
            Error (`Msg "Perl-compatible regexp parse error")
        | Re.Perl.Not_supported -> Error (`Msg "unsupported regexp feature")
      in
      let print = Re.pp_re in
      Arg.conv (parse, print)

    let int_range_list : int list Arg.conv =
      let exception Invalid_format in
      let parse s =
        let rec range lower upper acc =
          if lower > upper then acc else range (succ lower) upper (lower :: acc)
        in
        let process_range acc s =
          String.cuts ~sep:".." s
          |> List.concat_map (String.cuts ~sep:"-")
          |> List.map String.to_int
          |> function
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
              (`Msg
                "must be a comma-separated list of integers / integer ranges")
      in
      let print ppf set = Fmt.(braces @@ list ~sep:comma int) ppf set in
      Arg.conv (parse, print)

    let term =
      let+ name_regex =
        let doc = "A regular expression matching the names of tests to run" in
        Arg.(value & pos 0 (some regex) None & info [] ~doc ~docv:"NAME_REGEX")
      and+ index_cases =
        let doc =
          "A comma-separated list of test case numbers (and ranges of numbers) \
           to run, e.g: '4,6-10,19'. When specifying ranges, both '-' and '..' \
           are accepted as valid separators."
        in
        Arg.(
          value
          & pos 1 (some int_range_list) None
          & info [] ~doc ~docv:"TESTCASES")
      in
      match (name_regex, index_cases) with
      | None, None -> None
      | _, _ ->
          let name_filter =
            match name_regex with
            | None -> fun _ -> true
            | Some r -> fun n -> Re.execp r n
          in
          let index_filter =
            match index_cases with
            | None -> fun _ -> true
            | Some ints ->
                let set = Int.Set.of_list ints in
                fun i -> Int.Set.mem i set
          in
          Some
            (fun ~name ~index ->
              if name_filter name && index_filter index then `Run else `Skip)
  end
end

(* User configs before defaults have been applied. *)
module User = struct
  open Key

  type t = {
    and_exit : And_exit.t option;
    verbose : Verbose.t option;
    compact : Compact.t option;
    tail_errors : Tail_errors.t option;
    quick_only : Quick_only.t option;
    show_errors : Show_errors.t option;
    json : Json.t option;
    filter : Filter.t option;
    (* TODO: set Log_dir default internally *)
    log_dir : Log_dir.t;
    bail : Bail.t option;
    record_backtrace : Record_backtrace.t option;
  }

  let ( || ) a b =
    let merge_on f = Option.(f a || f b) in
    {
      and_exit = merge_on (fun t -> t.and_exit);
      verbose = merge_on (fun t -> t.verbose);
      compact = merge_on (fun t -> t.compact);
      tail_errors = merge_on (fun t -> t.tail_errors);
      quick_only = merge_on (fun t -> t.quick_only);
      show_errors = merge_on (fun t -> t.show_errors);
      json = merge_on (fun t -> t.json);
      filter = merge_on (fun t -> t.filter);
      log_dir = merge_on (fun t -> t.log_dir);
      bail = merge_on (fun t -> t.bail);
      record_backtrace = merge_on (fun t -> t.record_backtrace);
    }

  let term ~and_exit ~record_backtrace =
    let+ verbose = Verbose.term
    and+ compact = Compact.term
    and+ tail_errors = Tail_errors.term
    and+ show_errors = Show_errors.term
    and+ quick_only = Quick_only.term
    and+ json = Json.term
    and+ filter = Filter.term
    and+ log_dir = Log_dir.term
    and+ bail = Bail.term in
    {
      and_exit = Some and_exit;
      verbose;
      compact;
      tail_errors;
      show_errors;
      quick_only;
      json;
      filter;
      log_dir;
      bail;
      record_backtrace = Some record_backtrace;
    }

  (* Lift a config-sensitive function to one that consumes optional arguments that
     override config defaults. *)
  let kcreate : 'a. (t -> 'a) -> 'a with_options =
   fun f ?and_exit ?verbose ?compact ?tail_errors ?quick_only ?show_errors ?json
       ?filter ?log_dir ?bail ?record_backtrace ->
    f
      {
        and_exit;
        verbose;
        compact;
        tail_errors;
        quick_only;
        show_errors;
        json;
        filter;
        log_dir;
        bail;
        record_backtrace;
      }

  let create : (unit -> t) with_options = kcreate (fun t () -> t)
  let and_exit t = Option.value ~default:And_exit.default t.and_exit

  let record_backtrace t =
    Option.value ~default:Record_backtrace.default t.record_backtrace
end

let apply_defaults ~default_log_dir : User.t -> t =
 fun {
       and_exit;
       verbose;
       compact;
       tail_errors;
       quick_only;
       show_errors;
       json;
       filter;
       log_dir;
       bail;
       record_backtrace;
     } ->
  let open Key in
  object
    method and_exit = Option.value ~default:And_exit.default and_exit
    method verbose = Option.value ~default:Verbose.default verbose
    method compact = Option.value ~default:Compact.default compact
    method tail_errors = Option.value ~default:Tail_errors.default tail_errors
    method quick_only = Option.value ~default:Quick_only.default quick_only
    method show_errors = Option.value ~default:Show_errors.default show_errors
    method json = Option.value ~default:Json.default json
    method filter = filter
    method log_dir = Option.value ~default:default_log_dir log_dir
    method bail = Option.value ~default:Bail.default bail

    method record_backtrace =
      Option.value ~default:Record_backtrace.default record_backtrace
  end
