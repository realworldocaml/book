open Core

type +'a t

(* This [Obj.magic] is OK because we never allow user code access to [none] (except via
   [unsafe_value]).  We disallow [_ Uopt.t Uopt.t], so there is no chance of confusing
   [none] with [some none].  And [float Uopt.t array] is similarly disallowed. *)
let none : 'a t = Obj.magic "Uopt.none"
let some (x : 'a) : 'a t = Obj.magic x
let unsafe_value (x : 'a t) : 'a = Obj.magic x
let is_none t = phys_equal t none
let is_some t = not (is_none t)
let invariant invariant_a t = if is_some t then invariant_a (unsafe_value t)

let sexp_of_t sexp_of_a t =
  if is_none t then [%sexp None] else [%sexp Some (unsafe_value t : a)]
;;

let value_exn t = if is_none t then failwith "Uopt.value_exn" else unsafe_value t
let to_option t = if is_none t then None else Some (unsafe_value t)

let of_option = function
  | None -> none
  | Some a -> some a
;;

module Optional_syntax = struct
  module Optional_syntax = struct
    let is_none = is_none
    let unsafe_value = unsafe_value
  end
end
