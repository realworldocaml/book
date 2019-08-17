(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

(* CPS-friendly streams with elimination form

   next : 'a t -> (exn -> unit) -> (unit -> unit) -> ('a -> unit) -> unit

   where next stream throw e k calls e () if the stream is empty, f v if the
   next value in the stream is v, and throw exn if retrieving the next value
   raises exception exn.

   The *_expected and *_n functions can pass Failure and Invalid_argument,
   respectively, to their exception continuations. Occurence of these exceptions
   indicates programming errors, since the functions are not part of the
   interface of Markup.ml, and the internal code should be calling them only
   when it is statically provable that the functions will succeed. *)

open Common

type 'a t

val make : (exn cont -> unit cont -> 'a cont -> unit) -> 'a t
val construct : 'a t cps -> 'a t
val empty : unit -> 'a t

val next : 'a t -> exn cont -> unit cont -> 'a cont -> unit
val next_option : 'a t -> 'a option cps
val next_expected : 'a t -> 'a cps
val next_n : int -> 'a t -> 'a list cps

val push : 'a t -> 'a -> unit
val push_option : 'a t -> 'a option -> unit
val push_list : 'a t -> 'a list -> unit

val peek : 'a t -> exn cont -> unit cont -> 'a cont -> unit
val peek_option : 'a t -> 'a option cps
val peek_expected : 'a t -> 'a cps
val peek_n : int -> 'a t -> 'a list cps

val tap : ('a -> unit) -> 'a t -> (unit -> unit)
val checkpoint : 'a t -> 'a t * (unit -> unit)

val transform : ('a -> 'b -> ('c list * 'a option) cps) -> 'a -> 'b t -> 'c t
val map : ('a -> 'b cps) -> 'a t -> 'b t
val fold : ('a -> 'b -> 'a cps) -> 'a -> 'b t -> 'a cps
val iter : ('a -> unit cps) -> 'a t -> unit cps
val filter_map : ('a -> 'b option cps) -> 'a t -> 'b t
val filter : ('a -> bool cps) -> 'a t -> 'a t
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list cps
val enumerate : 'a t -> (int * 'a) t
