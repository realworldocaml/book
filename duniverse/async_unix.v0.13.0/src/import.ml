open Core
include Int.Replace_polymorphic_compare
include Async_kernel
module Epoll_max_ready_events = Config.Epoll_max_ready_events
module Max_inter_cycle_timeout = Config.Max_inter_cycle_timeout
module Max_num_open_file_descrs = Config.Max_num_open_file_descrs
module Max_num_threads = Config.Max_num_threads
module Min_inter_cycle_timeout = Config.Min_inter_cycle_timeout
module Debug = Async_kernel_private.Debug
module Job = Async_kernel_private.Job
module Kernel_scheduler = Async_kernel_scheduler.Private
module File_descr = Unix.File_descr

let print_s sexp = Core.printf "%s\n%!" (sexp |> Sexp.to_string_hum)
let am_test_runner = Base.Exported_for_specific_uses.am_testing

let () =
  if Async_kernel_config.Print_debug_messages_for.thread_pool
  then Thread_pool.debug := true
;;
