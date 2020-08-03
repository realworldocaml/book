open Core
open Async

include Log

module Console = struct
  module Ansi = Console.Ansi

  let with_style ~debug ~info ~error msg =
    let style, prefix =
      match Log.Message.level msg with
      | None -> info, ""
      | Some `Debug -> debug, "[DEBUG]"
      | Some `Info  -> info,  " [INFO]"
      | Some `Error -> error, "[ERROR]"
    in
    String.concat ~sep:" "
      [ prefix
      ; Log.Message.message msg ]
    |> Ansi.string_with_attr style

  let output
        ?(debug=([`Yellow] :> Ansi.attr list))
        ?(info=([] :> Ansi.attr list))
        ?(error=([`Red] :> Ansi.attr list))
        writer =
    Log.Output.create ~flush:(fun () -> return ()) (fun msgs ->
      Queue.iter msgs ~f:(fun msg ->
        with_style ~debug ~info ~error msg
        |> (fun styled_msg ->
          Writer.write writer styled_msg;
          Writer.newline writer));
      Deferred.any_unit [ Writer.flushed writer; Writer.consumer_left writer ])

  module Blocking = struct
    let output
          ?(debug=([`Yellow] :> Ansi.attr list))
          ?(info=([] :> Ansi.attr list))
          ?(error=([`Red] :> Ansi.attr list))
          outc =
      Log.Blocking.Output.create (fun msg ->
        (with_style ~debug ~info ~error msg)
        |> fun line -> Out_channel.output_lines outc [line])
  end
end

module Syslog = struct

  let to_syslog msg =
    let prefix =
      match Log.Message.level msg with
      | None   -> ""
      | Some l -> Log.Level.to_string l ^ " "
    in
    prefix ^ Log.Message.message msg
  ;;

  let to_level msg =
    match Log.Message.level msg with
    (* syslog is generally not configured to show `LOG_DEBUG *)
    | None        -> Syslog.Level.INFO
    | Some `Debug -> Syslog.Level.INFO
    | Some `Info  -> Syslog.Level.INFO
    | Some `Error -> Syslog.Level.ERR
  ;;

  let default_options = [ Syslog.Open_option.PID; Syslog.Open_option.CONS ]

  let openlog ?id ?(options = default_options) ?facility () =
    Syslog.openlog ?id ~options ?facility ()
  ;;

  let output ?id ?options ?facility () =
    let ready =
      let d = Ivar.create () in
      (* openlog () shouldn't block by default, but In_thread.run's a
         cheap cure for paranoia *)
      upon (In_thread.run (openlog ?id ?options ?facility)) (fun () -> Ivar.fill d ());
      Ivar.read d
    in
    Log.Output.create
      ~flush:(fun () -> return ())
      (fun msgs ->
         ready >>= fun () ->
         In_thread.run (fun () ->
           Queue.iter msgs ~f:(fun msg ->
             let syslog_level = to_level msg in
             let msg          = to_syslog msg in
             Syslog.syslog ~level:syslog_level (msg ^ "\n"))))
  ;;

  module Blocking = struct
    let output () =
      openlog ();
      Log.Blocking.Output.create (fun msg ->
        let syslog_level = to_level msg in
        let msg          = to_syslog msg in
        Syslog.syslog ~level:syslog_level (msg ^ "\n"))
    ;;
  end
end

module Command = struct
  open Core
  open Async

  type console_style = Plain | Color [@@deriving sexp]
  type console_output =
    | No
    | Stdout of console_style
    | Stderr of console_style
  [@@deriving sexp]

  module Parameters = struct
    type t =
      { log_level:Level.t
      ; log_to_console:console_output
      ; log_to_syslog:bool
      ; log_to_file:string option
      }
    [@@deriving fields, sexp]

    module Flag_name = struct
      (* This module exists to make it easier to inspect flag names. *)
      let log_to_file = "log-to-file"
      let log_to_console = "log-to-console"
      let log_to_stdout = "log-to-stdout"
      let log_with_color = "log-with-color"
      let log_to_syslog = "log-to-syslog"
      let log_level = "log-level"
    end

    let log_to_file_flag t =
      let default = Option.value t.log_to_file ~default:"<NONE>" in
      let doc = sprintf "FILENAME Log to a file (default: %s)" default in
      Command.Param.(flag Flag_name.log_to_file (optional Filename.arg_type) ~doc)
    ;;

    let log_to_console_flag t =
      let default =
        match t.log_to_console with
        | No -> false
        | Stderr _ | Stdout _ -> true
      in
      let doc = sprintf !"BOOL Log to console (default: %{Bool})" default in
      Command.Param.(
        flag
          Flag_name.log_to_console
          (optional_with_default default bool)
          ~doc)
    ;;

    let log_to_syslog_flag t =
      let doc = sprintf !"BOOL Log to syslog (default: %{Bool})" t.log_to_syslog in
      Command.Param.(
        flag
          Flag_name.log_to_syslog
          (optional_with_default t.log_to_syslog bool)
          ~doc)
    ;;

    let log_to_stdout_flag t =
      let default =
        match t.log_to_console with
        | No -> false
        | Stdout _ -> true
        | Stderr _ -> false
      in
      let doc =
        sprintf !"BOOL Log to stdout when logging to console (default: %{Bool})" default
      in
      Command.Param.(
        flag
          Flag_name.log_to_stdout
          (optional_with_default default bool)
          ~doc)
    ;;

    let log_with_color_flag t =
      let default =
        match t.log_to_console with
        | No | Stdout Plain | Stderr Plain -> false
        | Stdout Color | Stderr Color -> true
      in
      let doc =
        sprintf !"BOOL Log with color when logging to console (default: %{Bool})" default
      in
      Command.Param.(
        flag
          Flag_name.log_with_color
          (optional_with_default default bool)
          ~doc)
    ;;

    let log_level_flag t =
      let doc =
        sprintf
          !"LEVEL Set log level to one of [debug | error | info] (default: %{Log.Level})"
          t.log_level
      in
      Command.Param.(
        flag
          Flag_name.log_level
          (optional_with_default t.log_level Level.arg)
          ~doc)
    ;;

    let create_log_to_console ~log_to_console ~log_with_color ~log_to_stdout =
      match log_to_console, log_with_color, log_to_stdout with
      | false, true, _
      | false, _, true ->
        failwithf
          "-%s and -%s require -%s"
          Flag_name.log_with_color
          Flag_name.log_to_stdout
          Flag_name.log_to_console
          ()
      | true, false, false -> Stderr Plain
      | true, true, false -> Stderr Color
      | true, false, true -> Stdout Plain
      | true, true, true -> Stdout Color
      | false, false, false -> No
    ;;

    (* This tests the only(?) path to turning input into a safe type. *)
    let%expect_test "console_output" =
      let print ~log_to_console ~log_with_color ~log_to_stdout =
        let result =
          match
            create_log_to_console ~log_to_console ~log_with_color ~log_to_stdout
            |> sexp_of_console_output
            |> Sexp.to_string
          with
          | exception e -> Or_error.of_exn ~backtrace:(`This "") e
          | s -> Or_error.return s
        in
        Async.printf !"%{sexp:string Or_error.t}\n" result
      in
      print ~log_to_console:false ~log_with_color:false ~log_to_stdout:false;
      let%bind () = [%expect {| (Ok No) |}] in
      print ~log_to_console:false ~log_with_color:true ~log_to_stdout:true;
      let%bind () = [%expect {|
        (Error
         ((Failure "-log-with-color and -log-to-stdout require -log-to-console") "")) |}]
      in
      print ~log_to_console:true ~log_with_color:true ~log_to_stdout:false;
      let%bind () = [%expect {| (Ok "(Stderr Color)") |}] in
      print ~log_to_console:true ~log_with_color:false ~log_to_stdout:false;
      let%bind () = [%expect {| (Ok "(Stderr Plain)") |}] in
      print ~log_to_console:true ~log_with_color:true ~log_to_stdout:true;
      let%bind () = [%expect {| (Ok "(Stdout Color)") |}] in
      print ~log_to_console:true ~log_with_color:false ~log_to_stdout:true;
      let%bind () = [%expect {| (Ok "(Stdout Plain)") |}] in
      Deferred.unit
    ;;

    let params t =
      let%map_open.Command log_to_file = log_to_file_flag t
      and log_to_console = log_to_console_flag t
      and log_to_stdout = log_to_stdout_flag t
      and log_with_color = log_with_color_flag t
      and log_to_syslog = log_to_syslog_flag t
      and log_level = log_level_flag t
      in
      { log_to_file
      ; log_to_console =
          create_log_to_console ~log_to_console ~log_with_color ~log_to_stdout
      ; log_to_syslog
      ; log_level
      }
    ;;

    let console_output t =
      match t.log_to_console with
      | No -> None
      | Stderr Plain -> Some (Log.Output.stderr ())
      | Stderr Color -> Some (Console.output (Lazy.force Writer.stderr))
      | Stdout Plain -> Some (Log.Output.stdout ())
      | Stdout Color -> Some (Console.output (Lazy.force Writer.stdout))
    ;;

    let syslog_output t = if t.log_to_syslog then Some (Syslog.output ()) else None

    let file_output t =
      match t.log_to_file with
      | None -> None
      | Some filename -> Some (Output.file `Text ~filename)
    ;;

    let outputs t =
      List.filter_map [ console_output t; syslog_output t; file_output t ] ~f:Fn.id
    ;;
  end

  let setup_via_params
        ?(default_output_level = `Info)
        ~log_to_console_by_default
        ~log_to_syslog_by_default
        ?log_to_file_by_default
        ()
    =
    let default =
      Parameters.Fields.create
        ~log_level:default_output_level
        ~log_to_console:log_to_console_by_default
        ~log_to_syslog:log_to_syslog_by_default
        ~log_to_file:log_to_file_by_default
    in
    let%map.Command params = Parameters.params default in
    Log.Global.set_output (Parameters.outputs params);
    Log.Global.set_level (Parameters.log_level params)
  ;;
end
