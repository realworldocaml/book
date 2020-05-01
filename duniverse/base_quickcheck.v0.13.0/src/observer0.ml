open! Base

type 'a t = 'a -> size:int -> hash:Hash.state -> Hash.state

let create f : _ t = f

let observe (t : _ t) x ~size ~hash =
  if size < 0
  then raise_s [%message "Base_quickcheck.Observer.observe: size < 0" (size : int)]
  else t x ~size ~hash
;;

let opaque _ ~size:_ ~hash = hash
