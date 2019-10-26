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
      Writer.flushed writer)

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
