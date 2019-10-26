open! Core
open! Import

type 'a additional_create_args = timerfd:Linux_ext.Timerfd.t -> 'a

include
  File_descr_watcher_intf.S
  with type 'a additional_create_args := 'a additional_create_args
