open! Core
include Async_kernel.Async_kernel_config

let file_descr_watcher =
  match file_descr_watcher with
  | (Epoll | Select) as x -> x
  | Epoll_if_timerfd ->
    (* Without timerfd, epoll_wait(2) timeouts would have only millisecond precision. *)
    if Result.is_ok Linux_ext.Timerfd.create then Epoll else Select
;;

let max_num_open_file_descrs =
  if not
       (Max_num_open_file_descrs.equal
          max_num_open_file_descrs
          Max_num_open_file_descrs.default)
  then max_num_open_file_descrs
  else (
    match file_descr_watcher with
    | Select ->
      (* The maximum numeric value for a file descriptor watchable by [select] is limited
         by [FD_SETSIZE], which happens to be 1024 on Linux. *)
      Max_num_open_file_descrs.create_exn 1024
    | Epoll | Epoll_if_timerfd ->
      Int.min
        Max_num_open_file_descrs.(default |> raw)
        (match Unix.RLimit.(get num_file_descriptors).max with
         | Infinity -> Int.max_value
         | Limit int64 -> int64 |> Int64.to_int_exn)
      |> Max_num_open_file_descrs.create_exn)
;;
