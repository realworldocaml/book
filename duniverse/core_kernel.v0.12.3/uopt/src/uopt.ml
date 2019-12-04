open Core_kernel

type 'a t = 'a

(* This [Obj.magic] is OK because we never allow user code access to [none] (except via
   [unsafe_value].  [Uopt] is used only internally in [Incremental_lib].  And we never
   have an [_ Uopt.t Uopt.t], so there is no chance of confusing [none] with [some
   none].  And we never build [float Uopt.t array]s. *)
let none = "Uopt.none" |> (Obj.magic : string -> _ t)
let is_none t = phys_equal t none
let is_some t = not (is_none t)
let invariant invariant_a t = if is_some t then invariant_a t
let sexp_of_t sexp_of_a t = if is_none t then [%sexp None] else [%sexp Some (t : a)]
let some a = a
let value_exn t = if is_none t then failwith "Uopt.value_exn" else t
let unsafe_value t = t

let%test _ = is_none none
let%test _ = not (is_some none)
let%test _ = not (is_none (some 13))
let%test _ = is_some (some 13)
let%test _ = Exn.does_raise (fun () -> value_exn none)
let%test _ = value_exn (some 13) = 13
let%test _ = unsafe_value (some 13) = 13
