open Core
open Import
open File_descr_watcher_intf
open Read_write.Export
module Table = Bounded_int_table

type t =
  { descr_tables : (File_descr.t, unit) Table.t Read_write.t
  ; handle_fd_read_ready : File_descr.t -> unit
  ; handle_fd_read_bad : File_descr.t -> unit
  ; handle_fd_write_ready : File_descr.t -> unit
  ; handle_fd_write_bad : File_descr.t -> unit
  }
[@@deriving sexp_of]

let backend = Config.File_descr_watcher.Select

let invariant t : unit =
  try Read_write.iter t.descr_tables ~f:(Table.invariant ignore ignore) with
  | exn ->
    raise_s
      [%message
        "Select_file_descr_watcher.invariant failed"
          (exn : exn)
          ~select_file_descr_watcher:(t : t)]
;;

type 'a additional_create_args =
  handle_fd_read_bad:(File_descr.t -> unit)
  -> handle_fd_write_bad:(File_descr.t -> unit)
  -> 'a

let create
      ~handle_fd_read_bad
      ~handle_fd_write_bad
      ~num_file_descrs
      ~handle_fd_read_ready
      ~handle_fd_write_ready
  =
  { descr_tables =
      Read_write.create_fn (fun () ->
        Table.create
          ~num_keys:num_file_descrs
          ~key_to_int:File_descr.to_int
          ~sexp_of_key:File_descr.sexp_of_t
          ())
  ; handle_fd_read_ready
  ; handle_fd_read_bad
  ; handle_fd_write_ready
  ; handle_fd_write_bad
  }
;;

let reset_in_forked_process _ = ()

let iter t ~f =
  Read_write.iteri t.descr_tables ~f:(fun read_or_write table ->
    Table.iteri table ~f:(fun ~key ~data:_ -> f key read_or_write))
;;

module Pre = struct
  type t = File_descr.t list Read_write.t [@@deriving sexp_of]
end

let set t file_descr desired =
  Read_write.iteri t.descr_tables ~f:(fun read_or_write table ->
    if Read_write.get desired read_or_write
    then Table.set table ~key:file_descr ~data:()
    else Table.remove table file_descr)
;;

let pre_check t = Read_write.map t.descr_tables ~f:Table.keys

module Check_result = struct
  type t =
    { pre : Pre.t
    ; select_result : (Unix.Select_fds.t, exn) Result.t
    }
  [@@deriving sexp_of]
end

let thread_safe_check (type a) (_ : t) (pre : Pre.t) (timeout : a Timeout.t) (span : a) =
  let timeout =
    match timeout with
    | Never -> `Never
    | Immediately -> `Immediately
    (* Wait no longer than one second, which avoids any weirdness due to feeding large
       timeouts to select. *)
    | After -> `After (Time_ns.Span.min span Time_ns.Span.second)
  in
  { Check_result.pre
  ; select_result =
      Result.try_with (fun () ->
        Unix.select ~read:pre.read ~write:pre.write ~except:[] ~timeout ())
  }
;;

let post_check t ({ Check_result.pre; select_result } as check_result) =
  try
    match select_result with
    (* We think 514 should be treated like EINTR. *)
    | Error (Unix.Unix_error ((EINTR | EUNKNOWNERR 514), _, _)) -> ()
    | Ok { read; write; except } ->
      assert (List.is_empty except);
      List.iter write ~f:t.handle_fd_write_ready;
      List.iter read ~f:t.handle_fd_read_ready
    | Error (Unix.Unix_error (EBADF, _, _)) ->
      let bad read_or_write =
        let fds =
          match read_or_write with
          | `Read -> pre.read
          | `Write -> pre.write
        in
        List.fold fds ~init:[] ~f:(fun ac file_descr ->
          match
            Syscall.syscall (fun () -> ignore (Unix.fstat file_descr : Unix.stats))
          with
          | Ok () -> ac
          | Error (Unix.Unix_error (EBADF, _, _)) -> file_descr :: ac
          | Error exn ->
            raise_s
              [%message
                "fstat raised unexpected exn" (file_descr : File_descr.t) (exn : exn)])
      in
      List.iter (bad `Write) ~f:t.handle_fd_write_bad;
      List.iter (bad `Read) ~f:t.handle_fd_read_bad
    | Error exn -> raise_s [%message "select raised unexpected exn" ~_:(exn : exn)]
  with
  | exn ->
    raise_s
      [%message
        "File_descr_watcher.post_check bug"
          (exn : exn)
          (check_result : Check_result.t)
          ~select_file_descr_watcher:(t : t)]
;;
