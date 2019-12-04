open Core

module type S = sig
  type t
  val zero : t
  val succ : t -> t
end

module type Cnt = sig
  type t
  val _incr : unit -> t
end

module Cnt(V:S) : Cnt with type t = V.t = struct
  type t = V.t
  let p = ref V.zero

  let _incr () =
    p := V.succ !p;
    !p

  let%test _ = (V.succ V.zero > V.zero);;
end

module C1 = Cnt(Int)
let%test_module _ = (module Cnt(Int))
let%test_module "description" = (module Cnt(Int))

let%test_module _ = (module struct
  open List

  let%test _ = (group [] ~break:(fun _ -> assert false)) = []

  let mis = ['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i']
  let equal_letters =
    [['M'];['i'];['s';'s'];['i'];['s';'s'];['i'];['p';'p'];['i']]
  let single_letters =
    [['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i']]
  let every_three =
    [['M'; 'i'; 's']; ['s'; 'i'; 's']; ['s'; 'i'; 'p']; ['p'; 'i' ]]

  let%test _ = (group ~break:(<>) mis) = equal_letters
  let%test _ = (group ~break:(fun _ _ -> false) mis) = single_letters
  let%test _ = (groupi ~break:(fun i _ _ -> i mod 3 = 0) mis) = every_three

end)
