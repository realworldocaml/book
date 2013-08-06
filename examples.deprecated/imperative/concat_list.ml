  module Concat_list : sig
    type 'a t
    val singleton : 'a -> 'a t
    val concat  : 'a t -> 'a t -> 'a t  (* constant time *)
    val to_list : 'a t -> 'a list       (* linear time   *)
  end = struct

    type 'a t = Singleton of 'a | Concat of 'a t * 'a t

    let singleton x = Singleton x

    let concat x y = Concat (x,y)

    let rec to_list_with_tail t tail =
      match t with
      | Singleton x -> x :: tail
      | Concat (x,y) -> to_list_with_tail x (to_list_with_tail y tail)

    let to_list t =
      to_list_with_tail t []

  end;;
