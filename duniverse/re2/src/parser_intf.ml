open Core_kernel

module type S = sig

  (** A value of type ['a t] is a regex that parses ['a]s.
      The matching is implemented using Re2.

      UTF-8 is supported by Re2 but not by this module. This is because we want to use
      [char] as a character type, but that's just wrong in a multibyte encoding. *)
  type 'a t [@@deriving sexp_of]

  (** [case_sensitive] defaults to [true]. *)
  val compile : ?case_sensitive:bool -> 'a t -> (string -> 'a option) Staged.t
  val run     : ?case_sensitive:bool -> 'a t ->  string -> 'a option
  val matches : ?case_sensitive:bool -> 'a t ->  string -> bool

  (** [to_regex_string] and [to_re2] both forget what a ['a t] knows
      about turning the matching strings into ['a]s *)
  val to_regex_string : _ t -> string
  val to_re2 : ?case_sensitive:bool -> _ t -> Regex.t

  (** The applicative interface provides sequencing, e.g. [both a b] is a regex that
      parses an [a] followed by a [b] and returns both results in a pair. *)
  include Applicative.S with type 'a t := 'a t

  (** [of_re2 r] forgets the options that [r] was compiled with, instead using
      [`Encoding_latin1 true], [`Dot_nl true], and the case-sensitivity setting of the
      overall pattern. You can still try and use '(?flags:re)' Re2 syntax to set options
      for the scope of this regex.

      The returned values are precisely the captures of the underlying regex, in order:
      note that unlike (say) [Re2.Match.get_all], the whole match is *not* included (if
      you want that, just use [capture]). Named captures are not accessible by name. *)
  val of_re2 : Regex.t -> string option array t

  (** [ignore t] is a regex which matches the same strings that [t] matches, but doesn't
      call functions on the captured submatches. Particularly, something like [ignore (map
      (string "x") ~f:Int.of_string)] won't raise an exception, because the int conversion
      is never attempted.  *)
  val ignore : _ t -> unit t

  (** [capture t] returns the string matched by [t] *)
  val capture : unit t -> string t

  (** [and_capture t] returns the string matched by [t] in addition to whatever it was
      already going to return. *)
  val and_capture : 'a t -> ('a * string) t

  (** Regex that matches nothing *)
  val fail : 'a t

  val or_ : 'a t list -> 'a t

  (** [greedy] defaults to true. If false, the regexp will prefer not matching. *)
  val optional : ?greedy:bool -> 'a t -> 'a option t


  (** [repeat ~min ~max t] constructs the regex [t{min,max}]. [min] defaults to [0] and
      [max] defaults to [None] (unbounded), so that just plain [repeat t] is equivalent
      to [t*].

      It would be better for [repeat] to be ['a t -> 'a list t], but the re2 library
      doesn't give you access to repeated submatches like that. Hence, [repeat] ignores
      all submatches of its argument and does not call any callbacks that may have been
      attached to them, as if it had [ignore] called on its result. *)
  val repeat
    :  ?greedy:bool
    -> ?min:int
    -> ?max:int option
    -> unit t
    -> unit t

  (** [times r n] essentially constructs the regex [r{n}]. It is equivalent to
      [repeat ~min:n ~max:(Some n) r].

      Compare with, say, [all (List.init n ~f:(fun _ -> r))], which constructs the regex
      rrr...r (with n copies of r) and has the type ['a t -> 'a list t]. *)
  val times : unit t -> int -> unit t

  val string : string -> unit t

  val any_string : string t

  (** Matches empty string at the beginning of the text *)
  val start_of_input : unit t

  (** Matches empty string at the end of the text *)
  val end_of_input : unit t

  module Char : sig
    (** [any], unlike "." by default, matches newline.

        (However, note that [of_re2 (Re2.create_exn ".")] will match newline.  See the
        comment on [of_re2] for more information.)
    *)
    val any : char t

    (** Duplicates in the lists given to [one_of] and [not_one_of] are ignored. *)
    val one_of     : char list -> char t
    val not_one_of : char list -> char t

    (** The following 6 values match the Re2 character classes with the same name. *)

    (** A character matching [Char.is_uppercase] *)
    val upper : char t

    (** A character matching [Char.is_lowercase] *)
    val lower : char t

    (** A character matching [Char.is_alpha] *)
    val alpha : char t

    (** A character matching [Char.is_digit] *)
    val digit : char t

    (** A character matching [Char.is_alphanum] *)
    val alnum : char t

    (** A character matching [Char.is_whitespace] *)
    val space : char t
  end

  module Decimal : sig

    val digit : int t

    (** optional sign symbol:
        "+" or "" mean 1
        "-" means -1
    *)
    val sign : int t

    val unsigned : int t
    val int : int t
  end

end

module type Parser = sig
  type 'a t
  module Open_on_rhs_intf : sig module type S = S with type 'a t = 'a t end
  include Applicative.Let_syntax
    with type 'a t := 'a t
    with module Open_on_rhs_intf := Open_on_rhs_intf
  include Open_on_rhs_intf.S with type 'a t := 'a t
end
