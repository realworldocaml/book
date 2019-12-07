(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Result value combinators.

    [Rresult] is a module for handling computation results and errors
    in an explicit and declarative manner without resorting to
    exceptions. It defines a {!result} type equal to OCaml 4.03's
    [result] type and {{!R}combinators} to operate on these values.

    Open the module to use it, this defines the {{!result}result type},
    the {!R.Infix} operators {!R} in your scope.

    Consult {{!usage}usage guidelines} for the type.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Results} *)

(** The type for results. *)
type ('a, 'b) result = ('a, 'b) Result.result = Ok of 'a | Error of 'b

open Result

val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
(** [(>>=)] is {!R.( >>= )}. *)

val ( >>| ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
(** [(>>|)] is {!R.( >>| )}. *)

(** Result value combinators. *)
module R : sig

  (** {1 Results} *)

  type ('a, 'b) t = ('a, 'b) result
  (** The type for results. *)

  val ok : 'a -> ('a, 'b) result
  (** [ok v] is [Ok v]. *)

  val error : 'b -> ('a, 'b) result
  (** [error e] is [Error e]. *)

  val reword_error : ('b -> 'c) -> ('a, 'b) result -> ('a, 'c) result
  (** [reword_error reword r] is:
      {ul
      {- [r] if [r = Ok v]}
      {- [Error (reword e)] if [r = Error e]}} *)

  val get_ok : ('a, 'b) result -> 'a
  (** [get r] is [v] if [r = Ok v] and @raise Invalid_argument otherwise. *)

  val get_error : ('a, 'b) result -> 'b
  (** [get_error r] is [e] if [r = Error e] and @raise Invalid_argument
      otherwise. *)

  (**/**)
  val return : 'a -> ('a, 'b) result
  val fail : 'b -> ('a, 'b) result
  (**/**)

  (** {1 Composing results} *)

  val bind : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  (** [bind r f] is [f v] if [r = Ok v] and [r] if [r = Error _]. *)

  val map : ('a -> 'c) -> ('a, 'b) result -> ('c, 'b) result
  (** [map f r] is [bind (fun v -> ret (f v))] r. *)

  val join : (('a, 'b) result, 'b) result -> ('a, 'b) result
  (** [join r] is [v] if [r = Ok v] and [r] otherwise. *)

  val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  (** [r >>= f] is {!bind}[ r f]. *)

  val ( >>| ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
  (** [r >>| f] is {!map}[ r f]. *)

  (** Infix operators.

      Gathers {!R}'s infix operators. *)
  module Infix : sig

   (** {1 Infix operators} *)

    val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
    (** [(>>=)] is {!R.( >>= )}. *)

    val ( >>| ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
    (** [(>>|)] is {!R.( >>| )}. *)
  end

  (** {1:msgs Error messages} *)

  type msg = [ `Msg of string ]
  (** The type for (error) messages. *)

  val msg : string -> [> msg]
  (** [msg s] is [`Msg s]. *)

  val msgf : ('a, Format.formatter, unit, [> msg]) format4 -> 'a
  (** [msgf fmt ...] formats a message according to [fmt]. *)

  val pp_msg : Format.formatter -> msg -> unit
  (** [pp_msg ppf m] prints [m] on [ppf]. *)

  val error_msg : string -> ('a, [> msg]) result
  (** [error_msg s] is [error (`Msg s)]. *)

  val error_msgf : ('a, Format.formatter, unit, ('b, [> msg]) result)
      format4 -> 'a
  (** [error_msgf fmt ...] is an error message formatted according to [fmt]. *)

  val reword_error_msg : ?replace:bool -> (string -> msg)  ->
    ('a, msg) result -> ('a, [> msg]) result
  (** [reword_error_msg ~replace reword r] is like {!reword_error} except
      if [replace] is [false] (default), the result of [reword old_msg] is
      concatened, on a new line to the old message. *)

  val error_to_msg : pp_error:(Format.formatter -> 'b -> unit) ->
    ('a, 'b) result -> ('a, [> msg]) result
  (** [error_to_msg pp_error r] converts errors in [r] with [pp_error] to
      an error message. *)

  val error_msg_to_invalid_arg : ('a, msg) result -> 'a
  (** [err_msg_to_invalid_arg r] is [v] if [r = Ok v] and

      @raise Invalid_argument with the error message otherwise. *)

  val open_error_msg : ('a, msg) result -> ('a, [> msg]) result
  (** [open_error_msg r] allows to combine a closed error message
      variant with other variants. *)

  val failwith_error_msg : ('a, msg) result -> 'a
  (** [failwith_error_msg r] raises [Failure m] if [r] is
      [Error (`Msg m)]. *)

  (** {1:exn Trapping unexpected exceptions}

      {e Getting rid of [null] was not enough}. *)

  type exn_trap = [ `Exn_trap of exn * Printexc.raw_backtrace ]
  (** The type for exception traps. *)

  val pp_exn_trap : Format.formatter -> exn_trap -> unit
  (** [pp_exn_trap ppf bt] prints [bt] on [ppf]. *)

  val trap_exn : ('a -> 'b) -> 'a -> ('b, [> exn_trap]) result
  (** [trap_exn f v] is [f v] and traps any exception that may occur as
      an exception trap error. *)

  val error_exn_trap_to_msg : ('a, exn_trap) result -> ('a, [> msg]) result
  (** [error_exn_trap_to_msg r] converts exception trap errors in
      [r] to an error message. *)

  val open_error_exn_trap : ('a, exn_trap) result -> ('a, [> exn_trap]) result
  (** [open_error_exn_trap r] allows to combine a closed exception trap error
      variant with other variants. *)

  (** {1:print Pretty printing} *)

  val pp :
    ok:(Format.formatter -> 'a -> unit) ->
    error:(Format.formatter -> 'b -> unit) -> Format.formatter ->
    ('a, 'b) result -> unit
  (** [pp ok error ppf r] prints [r] on [ppf] using [ok] and [error] according
      to [r]. *)

  val dump :
    ok:(Format.formatter -> 'a -> unit) ->
    error:(Format.formatter -> 'b -> unit) -> Format.formatter ->
    ('a, 'b) result -> unit
  (** [dump ~ok ~error] formats an OCaml result value using [ok] or [error]
      according to case, no parentheses are added. *)

  (** {1:pred Predicates and comparison} *)

  val is_ok : ('a, 'b) result -> bool
  (** [is_ok r] is [true] iff [r = Ok _]. *)

  val is_error : ('a, 'b) result -> bool
  (** [is_error r] is [true] iff [r = Error _]. *)

  val equal : ok:('a -> 'a -> bool) -> error:('b -> 'b -> bool) ->
    ('a, 'b) result -> ('a, 'b) result -> bool
  (** [equal ~ok ~error r r'] tests [r] and [r'] for equality using [ok]
      and [error]. *)

  val compare : ok:('a -> 'a -> int) -> error:('b -> 'b -> int) ->
    ('a, 'b) result -> ('a, 'b) result -> int
  (** [compare ~ok ~error r r'] totally orders [r] and [r'] using [ok]
      and [error]. *)

  (** {1:convert Converting} *)

  val to_option : ('a, 'b) result -> 'a option
  (** [to_option r] is [Some v] if [r = Ok v] and [None] otherwise. *)

  val of_option : none:(unit -> ('a, 'b) result) -> 'a option -> ('a, 'b) result
  (** [of_option ~none r] is [Ok v] if [r = Some v] and [none ()] otherwise. *)

  val to_presult : ('a, 'b) result -> [> `Ok of 'a | `Error of 'b ]
  (** [to_presult r] is [r] as a polymorphic variant result value. *)

  val of_presult : [< `Ok of 'a | `Error of 'b ] -> ('a, 'b) result
  (** [of_presult pr] is [pr] as a result value. *)

  (** {1:ignore Ignoring errors}

      {b Warning.} Using these functions is, most of the time, a bad idea. *)

  val ignore_error : use:('b -> 'a) -> ('a, 'b) result -> 'a
  (** [ignore_error ~use r] is [v] if [r = Ok v] and [use e] if
      [r = Error e]. *)

  val kignore_error :
    use:('b -> ('a, 'c) result) -> ('a, 'b) result -> ('a, 'c) result
    (** [kignore_error ~use r] is [r] if [r = Ok v] and [use e] if
        [r = Error e]. *)
end

(** {1:usage Usage design guidelines}

    These are rough design guidelines, don't forget to think.

    {2 Error messages}

    Use {{!R.msgs}error messages} if:
    {ol
    {- Your error messages don't need to be localized, e.g. scripts,
       command line programs.}
    {- The errors don't need to be processed. They are just meant to
       be logged at certain point in your program.}}

    If the above doesn't hold and your errors need to be processed for
    localization or error recovery then use a custom error type in your
    result values.

    {2 Custom error types}

    If your module has specific errors then define an error type, and
    a result type that tags this error type with the library name (or
    any other tag that may make sense, see for example {!R.exn}) along
    with the following functions:

{[
module Mod : sig
  type error = ...
  type 'a result = ('a, [`Mod of error]) Rresult.result
  val pp_error : Format.formatter -> [`Mod of error] -> unit
  val open_error : 'a result -> ('a, [> `Mod of error]) Rresult.result
  val error_to_msg : 'a result -> ('a, Rresult.R.msg) Rresult.result

  val f : ... -> 'a result
end
]}

If your library has generic errors that may be useful in other context
or shared among modules and to be composed together, then define your
error type itself as being a variant and return these values
without tagging them.
{[
module Mod : sig
  type error = [`Generic of ... | ... ]
  type 'a result = ('a, error) Rresult.result
  val pp_error : Format.formatter -> error -> unit
  val open_error : 'a result -> ('a, [> error]) Rresult.result
  val error_to_msg : 'a result -> ('a, Rresult.R.msg) Rresult.result

  val f : ... -> 'a result
end
]}
In the latter case it may still be useful to provide a function to
tag these errors whenever they reach a certain point of the program.
For this the following function could be added to [Mod]:
{[
val pack_error : 'a result ->  ('a, [> `Mod of error]) Rresult.result
]}
You should then provide the following functions aswell, so that
the packed error composes well in the system:
{[
val pp_pack_error : Format.formatter -> [ `Mod of error] -> unit
val open_pack_error :  ('a, [ `Mod of error]) Rresult.result ->
  ('a, [> `Mod of error]) Rresult.result

val error_pack_to_msg : ('a, [ `Mod of error]) Rresult.result ->
  ('a, Rresult.R.msg) Rresult.result
]}
*)

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
