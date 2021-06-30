open! Import
module A = Parser_automaton

module type State = sig
  (** State of the parser *)
  type t

  (** Create a new parser state. [pos] is the initial position, it defaults to
      [{line=1;col=0;offset=0}]. *)
  val create : ?pos:Positions.pos -> unit -> t

  (** Reset the given parsing state. The following always succeed:

      {[
        reset t ?pos;
        assert (t = create ?pos ())
      ]}
  *)
  val reset : ?pos:Positions.pos -> t -> unit

  (** Number of characters fed to the parser *)
  val offset : t -> int

  (** Position in the text *)

  val line : t -> int
  val column : t -> int
  val position : t -> Positions.pos

  (** Prevent the state from receiving any more characters. Trying to feed more characters
      will result in an exception, unless the state is reset. *)
  val stop : t -> unit
end

module type Stack = Kind.Stack

module type S = sig
  (** Values produced by the parser *)
  type parsed_value

  module State : State
  module Stack : Stack

  (** Feed one character to the parser. In case of error, it raises [Parse_error] *)
  val feed : State.t -> char -> Stack.t -> Stack.t

  (** Instruct the parser that the end of input was reached. In case of error, it raises
      [Parse_error] *)
  val feed_eoi : State.t -> Stack.t -> parsed_value

  (** {3 Convenience functions} *)

  val feed_string : State.t -> string -> Stack.t -> Stack.t
  val feed_substring : State.t -> string -> pos:int -> len:int -> Stack.t -> Stack.t
  val feed_bytes : State.t -> bytes -> Stack.t -> Stack.t
  val feed_subbytes : State.t -> bytes -> pos:int -> len:int -> Stack.t -> Stack.t

  (** {3 High-level functions} *)

  val parse_string : string -> (parsed_value, Parse_error.t) result
  val parse_string_exn : string -> parsed_value
end

module type S_eager = sig
  (** Same as [Parser] but gives back a s-expression as soon as they are found in the
      input.

      For instance you can use this function to parse a stream and stop at the first
      s-expression:

      {[
        exception Got_sexp of Sexp.t

        let fetch_sexp stream =
          let module P = Parsexp.Sexp_parsing.Eager in
          let rec hot_loop state stream stack =
            match Stream.peek stream with
            | None -> P.feed_eoi state stack
            | Some char ->
              let stack = P.feed state char stack in
              Stream.junk stream;
              hot_loop state stream stack
          in
          let got_sexp state sexp =
            raise_notrace (Got_sexp sexp)
          in
          let count = Stream.count stream in
          let state = P.State.create ~f:got_sexp ~no_sexp_is_error:true in
          match hot_loop state stream P.Stack.empty with
          | () -> assert false
          | exception (Got_sexp sexp) ->
            (* This test is true if the s-expression includes the last character passed to
               the parser *)
            if P.State.offset state > Stream.count stream - count then Stream.junk stream;
            sexp
      ]}
  *)

  (** Values produces by the parser *)
  type parsed_value

  module State : sig
    include State

    module Read_only : sig
      (** Read-only handle to a parser state *)
      type t

      val offset : t -> int
      val line : t -> int
      val column : t -> int
      val position : t -> Positions.pos
    end

    (** [create ~f] create a new eager parser state. [f] will be called on each
        s-expression found. If [f] raises, then the parser is made unusable ([stop t] is
        invoked).

        [no_sexp_is_error] controls the behavior of the parse when the end of input is
        reached and no s-expression has been found. When [no_sexp_is_error] is [false]
        (the default) [feed_eoi] just returns [()], when it is [false] [feed_eoi]
        raises. In any case, if the end of input is reached while parsing an incomplete
        s-expression such as [(abc], error is raised.

        [f] must not save the read-only parser state it receives to access it after
        returning. It is unspecified what values it will read if it does so. *)
    val create
      :  ?pos:Positions.pos
      -> ?no_sexp_is_error:bool (** default: false *)
      -> (Read_only.t -> parsed_value -> unit)
      -> t

    (**/**)

    val old_parser_cont_state : t -> Old_parser_cont_state.t

    (**/**)
  end

  module Stack : Stack

  val feed : State.t -> char -> Stack.t -> Stack.t
  val feed_eoi : State.t -> Stack.t -> unit
  val feed_string : State.t -> string -> Stack.t -> Stack.t
  val feed_substring : State.t -> string -> pos:int -> len:int -> Stack.t -> Stack.t
  val feed_bytes : State.t -> bytes -> Stack.t -> Stack.t
  val feed_subbytes : State.t -> bytes -> pos:int -> len:int -> Stack.t -> Stack.t

  module Lexbuf_consumer : sig
    type t

    val create : unit -> t

    (** Consume exactly one s-expression from the given lexing buffer *)
    val parse : t -> Lexing.lexbuf -> parsed_value

    (** Consume exactly one s-expression from the given lexing buffer. Returns [None] if
        the end of input is reached before seeing any s-expression. *)
    val parse_opt : t -> Lexing.lexbuf -> parsed_value option
  end
end

module Mode (Kind : Kind.S) = struct
  module type S = sig
    type parsed_value

    val mode : (Kind.state, Kind.Stack.t) A.mode
    val make_value : (Kind.state, Kind.Stack.t) A.state -> Kind.Stack.t -> parsed_value
  end
end

module Mode_eager (Kind : Kind.S) = struct
  module type S = sig
    type parsed_value

    val make_value : (Kind.state, Kind.Stack.t) A.state -> Kind.Stack.t -> parsed_value
  end
end

module type Parser = sig
  module Mode = Mode
  module Mode_eager = Mode_eager

  module type S = S
  module type S_eager = S_eager
  module type Stack = Stack
  module type State = State

  module Make (Kind : Kind.S) (Mode : Mode(Kind).S) :
    S with type parsed_value = Mode.parsed_value

  module Make_eager (Kind : Kind.S) (Mode : Mode_eager(Kind).S) :
    S_eager with type parsed_value = Mode.parsed_value
end
