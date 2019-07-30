open! Core_kernel
open! Deferred.Let_syntax
open! Import
include Core_kernel.Invariant

module Async = struct
  include Async_invariant_intf.Async

  let invariant here t sexp_of_t f =
    match%map Monitor.try_with f ~extract_exn:true with
    | Ok () -> ()
    | Error exn ->
      raise_s
        [%message
          "invariant failed" ~_:(here : Source_code_position.t) (exn : exn) ~_:(t : t)]
  ;;

  let check_field t f wait_for_previous field =
    let%bind () = wait_for_previous in
    match%map Monitor.try_with ~extract_exn:true (fun () -> f (Field.get field t)) with
    | Ok () -> ()
    | Error exn ->
      raise_s
        [%message "problem with field" ~field:(Field.name field : string) (exn : exn)]
  ;;
end
