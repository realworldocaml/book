open! Core_kernel
open! Import

type 'a t = 'a ref

(* Being a pointer, no one outside this module can construct a value that is
   [phys_same] as this one.

   this code is duplicated in Option_array.Cheap_option, and if we find yet another
   place where we want it we should reconsider making it shared. *)
let none = Obj.obj (Obj.new_block Obj.abstract_tag 1)
let create () = ref none
let is_none x = phys_equal !x none
let is_some x = not (is_none x)
let get_some_exn x = if is_none x then raise_s [%message "Moption.get_some_exn"] else !x
let get t = if is_none t then None else Some !t
let unsafe_get t = !t
let set_some t v = t := v
let set_none t = t := none

let set t v =
  match v with
  | None -> set_none t
  | Some v -> set_some t v
;;

let sexp_of_t sexp_of_a t = [%sexp (get t : a option)]

let invariant invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    Option.iter (get t) ~f:invariant_a)
;;

module Optional_syntax = struct
  module Optional_syntax = struct
    let is_none = is_none
    let unsafe_value = unsafe_get
  end
end
