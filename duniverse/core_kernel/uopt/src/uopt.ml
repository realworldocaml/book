open Core_kernel

type 'a t = 'a

(* This [Obj.magic] is OK because we never allow user code access to [none] (except via
   [unsafe_value].  We disallow [_ Uopt.t Uopt.t], so there is no chance of confusing
   [none] with [some none].  And [float Uopt.t array] is similarly disallowed. *)
let none = "Uopt.none" |> (Obj.magic : string -> _ t)
let is_none t = phys_equal t none
let is_some t = not (is_none t)
let invariant invariant_a t = if is_some t then invariant_a t
let sexp_of_t sexp_of_a t = if is_none t then [%sexp None] else [%sexp Some (t : a)]
let some a = a
let value_exn t = if is_none t then failwith "Uopt.value_exn" else t
let unsafe_value t = t
let to_option t = if is_none t then None else Some t

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
