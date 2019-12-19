(** A module internal to [Core_bench]. Please look at {!Bench}. *)

open! Core

module Id : Unique_id.Id

module Basic_test : sig
  type packed_f = T : ([`init] -> (unit -> 'a)) -> packed_f

  type t = {
    test_id     : Id.t;
    name        : string;
    test_name   : string;
    file_name   : string;
    module_name : string;
    key         : int;
    arg         : int option;
    group_key   : int option;
    f           : packed_f;
  }

  val test_id     : t -> Id.t
  val name        : t -> string
  val test_name   : t -> string
  val file_name   : t -> string
  val module_name : t -> string
  val key         : t -> int
  val arg         : t -> int option
  val group_key   : t -> int option
  val f           : t -> packed_f

  val create_with_initialization :
    name:string ->
    ?test_name:string ->
    ?file_name:string ->
    ?module_name:string ->
    ?group_key:int option ->
    ?arg:int option -> key:int -> ([`init] -> unit -> unit) -> t

  val make_filename : t -> string
end

type t

val name  : t -> string
val test_name : t -> string
val file_name : t -> string
val module_name : t -> string
val tests : t -> Basic_test.t list

val expand : t list -> Basic_test.t list

val create :
  name:string
  -> ?test_name:string
  -> ?file_name:string
  -> ?module_name:string
  -> ?key:int
  -> (unit -> 'a)
  -> t

val create_with_initialization :
  name:string
  -> ?test_name:string
  -> ?file_name:string
  -> ?module_name:string
  -> ?key:int
  -> ([`init] -> unit -> 'a)
  -> t

val create_indexed
  :  name:string
  -> ?test_name:string
  -> ?file_name:string
  -> ?module_name:string
  -> args:int list
  -> ?key:int
  -> (int -> (unit -> 'a) Staged.t)
  -> t

val create_group
  :  name:string
  -> ?test_name:string
  -> ?file_name:string
  -> ?module_name:string
  -> t list
  -> t
