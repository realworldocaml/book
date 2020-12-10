(** Re-export for compatibility with 4.02 *)
type ('a, 'e) result = ('a, 'e) Result.result =
  | Ok of 'a
  | Error of 'e

type msg = [ `Msg of string ]

val (>>=) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
