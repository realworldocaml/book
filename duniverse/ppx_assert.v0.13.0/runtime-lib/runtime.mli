open Base

(** Types used in the generated code *)

type 'a test_pred
  = ?here:Lexing.position list
  -> ?message:string
  -> ('a -> bool)
  -> 'a
  -> unit

type 'a test_eq
  = ?here:Lexing.position list
  -> ?message:string
  -> ?equal:('a -> 'a -> bool)
  -> 'a
  -> 'a
  -> unit

type 'a test_result
   = ?here:Lexing.position list
  -> ?message:string
  -> ?equal:('a -> 'a -> bool)
  -> expect:'a
  -> 'a
  -> unit

(** Functions called by the generated code *)

val test_pred :
  pos:string ->
  sexpifier:('a -> Sexp.t) ->
  here:Lexing.position list ->
  ?message:string ->
  ('a -> bool) ->
  'a ->
  unit

val test_eq :
  pos:string ->
  sexpifier:('a -> Sexp.t) ->
  comparator:('a -> 'a -> int) ->
  here:Lexing.position list ->
  ?message:string ->
  ?equal:('a -> 'a -> bool) ->
  'a ->
  'a ->
  unit

val test_result :
  pos:string ->
  sexpifier:('a -> Sexp.t) ->
  comparator:('a -> 'a -> int) ->
  here:Lexing.position list ->
  ?message:string ->
  ?equal:('a -> 'a -> bool) ->
  expect:'a ->
  got:'a ->
  unit

(** Called to set/unset the [diff] function, used by [test_result] *)
val set_diff_function : (from_:string -> to_:string -> unit) option -> unit




