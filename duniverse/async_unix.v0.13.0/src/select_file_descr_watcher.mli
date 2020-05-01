open! Core
open! Import

type 'a additional_create_args =
  handle_fd_read_bad:(File_descr.t -> unit)
  -> handle_fd_write_bad:(File_descr.t -> unit)
  -> 'a

include
  File_descr_watcher_intf.S
  with type 'a additional_create_args := 'a additional_create_args
