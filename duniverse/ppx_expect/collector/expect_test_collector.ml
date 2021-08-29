open Expect_test_common
module List = ListLabels

module Test_outcome = struct
  type t =
    { file_digest : File.Digest.t
    ; location : File.Location.t
    ; expectations : Expectation.Raw.t list
    ; uncaught_exn_expectation : Expectation.Raw.t option
    ; saved_output : (File.Location.t * string) list
    ; trailing_output : string
    ; upon_unreleasable_issue : Expect_test_config_types.Upon_unreleasable_issue.t
    ; uncaught_exn : (exn * Printexc.raw_backtrace) option
    }
end

let tests_run : Test_outcome.t list ref = ref []

let protect ~finally ~f =
  match f () with
  | x ->
    finally ();
    x
  | exception e ->
    finally ();
    raise e
;;

module Current_file = struct
  let current = ref None

  let set ~absolute_filename =
    match !current with
    | None -> current := Some absolute_filename
    | Some _ -> failwith "Expect_test_collector.set: already set"
  ;;

  let unset () =
    match !current with
    | Some _ -> current := None
    | None -> failwith "Expect_test_collector.unset: not set"
  ;;

  let get () =
    match !current with
    | Some fn -> fn
    | None -> failwith "Expect_test_collector.get: not set"
  ;;
end

module Make (C : Expect_test_config_types.S) = struct
  let ( >>= ) t f = C.IO_flush.bind t ~f
  let return = C.IO_flush.return

  module C = struct
    include C

    let flush () =
      Format.pp_print_flush Format.std_formatter ();
      Format.pp_print_flush Format.err_formatter ();
      Caml.flush Caml.stdout;
      Caml.flush Caml.stderr;
      C.flush ()
    ;;
  end

  module Instance : sig
    val save_output : File.Location.t -> unit C.IO_flush.t
    val save_and_return_output : File.Location.t -> string C.IO_flush.t

    val exec
      :  file_digest:File.Digest.t
      -> location:File.Location.t
      -> expectations:Expectation.Raw.t list
      -> uncaught_exn_expectation:Expectation.Raw.t option
      -> f:(unit -> unit C.IO_run.t)
      -> unit
  end = struct
    type t =
      { mutable saved : (File.Location.t * int) list
      ; chan : out_channel
      ; filename : File.Name.t
      }

    external before_test
      :  output:out_channel
      -> stdout:out_channel
      -> stderr:out_channel
      -> unit
      = "expect_test_collector_before_test"

    external after_test
      :  stdout:out_channel
      -> stderr:out_channel
      -> unit
      = "expect_test_collector_after_test"

    external pos_out : out_channel -> int = "caml_out_channel_pos_fd"

    let get_position () = pos_out stdout

    let create () =
      let filename = Filename.temp_file "expect-test" "output" in
      let chan = open_out filename in
      before_test ~output:chan ~stdout ~stderr;
      { chan; filename = File.Name.of_string filename; saved = [] }
    ;;

    let extract_output ic len =
      let s = really_input_string ic len in
      if not (Check_backtraces.contains_backtraces s)
      then s
      else
        Expect_test_config_types.Upon_unreleasable_issue
        .message_when_expectation_contains_backtrace
          C.upon_unreleasable_issue
        ^ s
    ;;

    let relative_filename t = File.Name.relative_to ~dir:(File.initial_dir ()) t.filename

    let with_ic fname ~f =
      let ic = open_in fname in
      protect ~finally:(fun () -> close_in ic) ~f:(fun () -> f ic)
    ;;

    let get_outputs_and_cleanup t =
      let last_ofs = get_position () in
      after_test ~stdout ~stderr;
      close_out t.chan;
      let fname = relative_filename t in
      protect
        ~finally:(fun () -> Sys.remove fname)
        ~f:(fun () ->
          with_ic fname ~f:(fun ic ->
            let ofs, outputs =
              List.fold_left
                (List.rev t.saved)
                ~init:(0, [])
                ~f:(fun (ofs, acc) (loc, next_ofs) ->
                  let s = extract_output ic (next_ofs - ofs) in
                  next_ofs, (loc, s) :: acc)
            in
            let trailing_output = extract_output ic (last_ofs - ofs) in
            List.rev outputs, trailing_output))
    ;;

    let current_test : (File.Location.t * t) option ref = ref None

    let get_current () =
      match !current_test with
      | Some (_, t) -> t
      | None ->
        failwith "Expect_test_collector.Instance.get_current called outside a test."
    ;;

    let save_output location =
      let t = get_current () in
      C.flush ()
      >>= fun () ->
      let pos = get_position () in
      t.saved <- (location, pos) :: t.saved;
      return ()
    ;;

    let save_and_return_output location =
      let t = get_current () in
      C.flush ()
      >>= fun () ->
      let pos = get_position () in
      let prev_pos =
        match t.saved with
        | [] -> 0
        | (_, prev_pos) :: _ -> prev_pos
      in
      t.saved <- (location, pos) :: t.saved;
      flush t.chan;
      let len = pos - prev_pos in
      return
        (with_ic (relative_filename t) ~f:(fun ic ->
           seek_in ic prev_pos;
           really_input_string ic len))
    ;;

    let () =
      Caml.at_exit (fun () ->
        match !current_test with
        | None -> ()
        | Some (loc, t) ->
          let blocks, trailing = get_outputs_and_cleanup t in
          Printf.eprintf
            "File %S, line %d, characters %d-%d:\n\
             Error: program exited while expect test was running!\n\
             Output captured so far:\n\
             %!"
            (File.Name.to_string loc.filename)
            loc.line_number
            (loc.start_pos - loc.line_start)
            (loc.end_pos - loc.line_start);
          List.iter blocks ~f:(fun (_, s) -> Printf.eprintf "%s%!" s);
          Printf.eprintf "%s%!" trailing)
    ;;

    let rec final_flush ?(count = 0) k =
      let max_attempts = 10 in
      C.flush ()
      >>= fun () ->
      if C.flushed ()
      then k ~append:""
      else if count = max_attempts
      then
        k
          ~append:
            (Printf.sprintf
               "\n\
                STOPPED COLLECTING OUTPUT AFTER %d FLUSHING ATTEMPS\n\
                THERE MUST BE A BACKGROUND JOB PRINTING TO STDOUT\n"
               max_attempts)
      else final_flush ~count:(count + 1) k
    ;;

    let exec ~file_digest ~location ~expectations ~uncaught_exn_expectation ~f =
      let t = create () in
      current_test := Some (location, t);
      let finally uncaught_exn =
        C.run (fun () ->
          C.IO_flush.to_run
            (final_flush (fun ~append ->
               current_test := None;
               let saved_output, trailing_output = get_outputs_and_cleanup t in
               tests_run
               := { file_digest
                  ; location
                  ; expectations
                  ; uncaught_exn_expectation
                  ; saved_output
                  ; trailing_output = trailing_output ^ append
                  ; upon_unreleasable_issue = C.upon_unreleasable_issue
                  ; uncaught_exn
                  }
                  :: !tests_run;
               return ())))
      in
      match C.run f with
      | () -> finally None
      | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        finally (Some (exn, bt))
    ;;
  end

  let save_output = Instance.save_output
  let save_and_return_output = Instance.save_and_return_output

  let run
        ~file_digest
        ~(location : File.Location.t)
        ~absolute_filename:defined_in
        ~description
        ~tags
        ~expectations
        ~uncaught_exn_expectation
        ~inline_test_config
        f
    =
    Ppx_inline_test_lib.Runtime.test
      ~config:inline_test_config
      ~descr:
        (match description with
         | None -> ""
         | Some s -> ": " ^ s)
      ~tags
      ~filename:(File.Name.to_string location.filename)
      ~line_number:location.line_number
      ~start_pos:(location.start_pos - location.line_start)
      ~end_pos:(location.end_pos - location.line_start)
      (fun () ->
         let registering_tests_for = Current_file.get () in
         if defined_in <> registering_tests_for
         then
           Printf.ksprintf
             failwith
             "Trying to run an expect test from the wrong file.\n\
              - test declared at %s:%d\n\
              - trying to run it from %s\n"
             defined_in
             location.line_number
             registering_tests_for
         else (
           (* To avoid capturing not-yet flushed data of the stdout buffer *)
           C.run (fun () -> C.IO_flush.to_run (C.flush ()));
           Instance.exec ~file_digest ~location ~expectations ~uncaught_exn_expectation ~f;
           true))
  ;;
end
[@@inline never]

let tests_run () =
  (* We prepend tests when we encounter them, so reverse the list to reinstate order *)
  List.rev !tests_run
;;
