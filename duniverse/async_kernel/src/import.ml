module Debug_in_this_directory = Debug
module Time_ns_in_this_directory = Time_ns
open! Core_kernel
include Int.Replace_polymorphic_compare
module Debug = Debug_in_this_directory
module Time_ns = Time_ns_in_this_directory
module Epoll_max_ready_events = Async_kernel_config.Epoll_max_ready_events
module Max_inter_cycle_timeout = Async_kernel_config.Max_inter_cycle_timeout
module Max_num_open_file_descrs = Async_kernel_config.Max_num_open_file_descrs
module Max_num_threads = Async_kernel_config.Max_num_threads
module Pool = Tuple_pool

module Max_num_jobs_per_priority_per_cycle =
  Async_kernel_config.Max_num_jobs_per_priority_per_cycle

let concat = String.concat
let eprint = Core_kernel.Debug.eprint
let eprint_s = Core_kernel.Debug.eprint_s
let eprints = Core_kernel.Debug.eprints
let print_s sexp = printf "%s\n%!" (sexp |> Sexp.to_string_hum)
let sec = Time_ns.Span.of_sec

(* We don't want to use these modules in Async_kernel, to avoid difficulties with
   using it on js_of_ocaml. *)
module Thread = struct end
module Unix = struct end
