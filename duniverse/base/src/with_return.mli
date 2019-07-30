
(** [with_return f] allows for something like the return statement in C within [f].

    There are three ways [f] can terminate:

    + If [f] calls [r.return x], then [x] is returned by [with_return].
    + If [f] evaluates to a value [x], then [x] is returned by [with_return].
    + If [f] raises an exception, it escapes [with_return].

    Here is a typical example:

    {[
      let find l ~f =
        with_return (fun r ->
          List.iter l ~f:(fun x -> if f x then r.return (Some x));
          None
        )
    ]}

    It is only because of a deficiency of ML types that [with_return] doesn't have type:

    {[ val with_return : 'a. (('a -> ('b. 'b)) -> 'a) -> 'a ]}

    but we can slightly increase the scope of ['b] without changing the meaning of the
    type, and then we get:

    {[
      type 'a return = { return : 'b . 'a -> 'b }
      val with_return : ('a return -> 'a) -> 'a
    ]}

    But the actual reason we chose to use a record type with polymorphic field is that
    otherwise we would have to clobber the namespace of functions with [return] and that
    is undesirable because [return] would get hidden as soon as we open any monad. We
    considered names different than [return] but everything seemed worse than just having
    [return] as a record field. We are clobbering the namespace of record fields but that
    is much more acceptable. *)

open! Import

type -'a return = private { return : 'b. 'a -> 'b } [@@unboxed]

val with_return        : ('a return -> 'a  ) -> 'a

(** Note that [with_return_option] allocates ~5 words more than the equivalent
    [with_return] call. *)
val with_return_option : ('a return -> unit) -> 'a option

(** [prepend a ~f] returns a value [x] such that each call to [x.return] first applies [f]
    before applying [a.return].  The call to [f] is "prepended" to the call to the
    original [a.return].  A possible use case is to hand [x] over to another function
    which returns ['b], a subtype of ['a], or to capture a common transformation [f]
    applied to returned values at several call sites. *)
val prepend : 'a return -> f:('b -> 'a) -> 'b return
