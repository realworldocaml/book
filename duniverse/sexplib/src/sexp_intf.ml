(** Sexp_intf: interface specification for handling S-expressions (I/O, etc.) *)

open Format
open Bigarray

module type S = sig
  module Raw_grammar = Sexplib0.Sexp.Private.Raw_grammar

  (** Type of S-expressions *)
  type t = Type.t =
    | Atom of string
    | List of t list

  (** Type of bigstrings *)
  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  val compare : t -> t -> int
  val equal : t -> t -> bool

  (** {6 Defaults} *)

  (** [default_indent] reference to default indentation level for
      human-readable conversions.  Initialisation value: 2. *)
  val default_indent : int ref

  (** {6 S-expression size} *)

  (** [size sexp] @return [(n_atoms, n_chars)], where [n_atoms] is
      the number of atoms in S-expression [sexp], and [n_chars] is the
      number of characters in the atoms of the S-expression. *)
  val size : t -> int * int

  (** {6 Scan functions} *)

  (** [scan_sexp ?buf lexbuf] scans an S-expression from lex buffer
      [lexbuf] using the optional string buffer [buf] for storing
      intermediate strings. *)
  val scan_sexp : ?buf:Buffer.t -> Lexing.lexbuf -> t

  (** [scan_sexps ?buf lexbuf] reads a list of whitespace separated
      S-expressions from lex buffer [lexbuf] using the optional string
      buffer [buf] for storing intermediate strings. *)
  val scan_sexps : ?buf:Buffer.t -> Lexing.lexbuf -> t list

  (** [scan_rev_sexps ?buf lexbuf] same as {!scan_sexps}, but returns the
      reversed list and is slightly more efficient. *)
  val scan_rev_sexps : ?buf:Buffer.t -> Lexing.lexbuf -> t list

  (** [scan_sexp_opt ?buf lexbuf] is equivalent to [scan_sexp ?buf lexbuf]
      except that it returns [None] when the eof is reached. *)
  val scan_sexp_opt : ?buf:Buffer.t -> Lexing.lexbuf -> t option

  (** [scan_iter_sexps ?buf ~f lexbuf] iterates over all whitespace
      separated S-expressions scanned from lex buffer [lexbuf] using
      function [f], and the optional string buffer [buf] for storing
      intermediate strings. *)
  val scan_iter_sexps : ?buf:Buffer.t -> f:(t -> unit) -> Lexing.lexbuf -> unit

  (** [scan_fold_sexps ?buf ~f ~init lexbuf] folds over all whitespace
      separated S-expressions scanned from lex buffer [lexbuf] using
      function [f], initial state [init], and the optional string buffer
      [buf] for storing intermediate strings. *)
  val scan_fold_sexps
    :  ?buf:Buffer.t
    -> f:('a -> t -> 'a)
    -> init:'a
    -> Lexing.lexbuf
    -> 'a

  (** [scan_sexps_conv ?buf ~f lexbuf] maps all whitespace separated
      S-expressions scanned from lex buffer [lexbuf] to some list using
      function [f], and the optional string buffer [buf] for storing
      intermediate strings. *)
  val scan_sexps_conv : ?buf:Buffer.t -> f:(t -> 'a) -> Lexing.lexbuf -> 'a list

  (** {6 Type and exception definitions for (partial) parsing} *)

  module Parse_pos : sig
    (** Position information after complete parse *)
    type t = Pre_sexp.Parse_pos.t = private
      { mutable text_line : int (** Line position in parsed text *)
      ; mutable text_char : int (** Character position in parsed text *)
      ; mutable global_offset : int (** Global/logical offset *)
      ; mutable buf_pos : int (** Read position in string buffer *)
      }

    (** [create ?text_line ?text_char ?buf_pos ?global_offset ()] @return
        a parse position with the given parameters.

        @param text_line default = [1]
        @param text_char default = [0]
        @param global_offset default = [0]
        @param buf_pos default = [0]
    *)
    val create
      :  ?text_line:int
      -> ?text_char:int
      -> ?buf_pos:int
      -> ?global_offset:int
      -> unit
      -> t

    (** [with_buf_pos t pos] @return a copy of the parse position [t] where
        [buf_pos] is set to [pos]. *)
    val with_buf_pos : t -> int -> t
  end

  module Cont_state : sig
    (** State of parser continuations *)
    type t = Pre_sexp.Cont_state.t =
      | Parsing_toplevel_whitespace
      | Parsing_nested_whitespace
      | Parsing_atom
      | Parsing_list
      | Parsing_sexp_comment
      | Parsing_block_comment

    (** [to_string cont_state] converts state of parser continuation
        [cont_state] to a string. *)
    val to_string : t -> string
  end

  (** Type of result from calling {!Sexp.parse}. *)
  type ('a, 't) parse_result = ('a, 't) Pre_sexp.parse_result =
    | Done of 't * Parse_pos.t
    (** [Done (t, parse_pos)] finished parsing
        an S-expression.  Current parse position
        is [parse_pos]. *)
    | Cont of Cont_state.t * ('a, 't) parse_fun
    (** [Cont (cont_state, parse_fun)] met the end of input before completely
        parsing an S-expression.  The user has to call [parse_fun] to
        continue parsing the S-expression in another buffer.  [cont_state]
        is the current parsing state of the continuation.
        NOTE: the continuation may only be called once and will raise
        [Failure] otherwise! *)

  (** Type of parsing functions with given offsets and lengths. *)
  and ('a, 't) parse_fun = pos:int -> len:int -> 'a -> ('a, 't) parse_result

  (** Module for parsing S-expressions annotated with location information *)
  module Annotated : sig
    (** Position information for annotated S-expressions *)
    type pos = Pre_sexp.Annotated.pos =
      { line : int
      ; col : int
      ; offset : int
      }

    (** Range information for annotated S-expressions *)
    type range = Pre_sexp.Annotated.range =
      { start_pos : pos
      ; end_pos : pos
      }

    (** S-expression annotated with location information *)
    type t = Pre_sexp.Annotated.t =
      | Atom of range * Type.t
      | List of range * t list * Type.t

    (** Type of conversion results of annotated S-expressions. *)
    type 'a conv =
      [ `Result of 'a
      | `Error of exn * t
      ]

    val sexp_of_conv : ('a -> Type.t) -> 'a conv -> Type.t

    (** Exception associated with conversion errors.  First argument describes
        the location, the second the reason. *)
    exception Conv_exn of string * exn

    (** Stack used by annotation parsers *)
    type stack = Pre_sexp.Annotated.stack =
      { mutable positions : pos list
      ; mutable stack : t list list
      }

    (** [get_sexp annot_sexp] @return S-expression associated with annotated
        S-expression [annot_sexp]. *)
    val get_sexp : t -> Type.t

    (** [get_range annot_sexp] @return the range associated with annotated
        S-expression [annot_sexp]. *)
    val get_range : t -> range

    (** [find_sexp annot_sexp sexp] @return [Some res] where [res] is the
        annotated S-expression that is physically equivalent to [sexp] in
        [annot_sexp], or [None] if there is no such S-expression. *)
    val find_sexp : t -> Type.t -> t option

    (** {6 Annotated (partial) parsing} *)

    (** [parse ?parse_pos ?len str] same as {!parse}, but returns an
        S-expression annotated with location information. *)
    val parse : ?parse_pos:Parse_pos.t -> ?len:int -> string -> (string, t) parse_result

    (** [parse_bigstring ?parse_pos ?len str] same as {!parse_bigstring},
        but returns an S-expression annotated with location information. *)
    val parse_bigstring
      :  ?parse_pos:Parse_pos.t
      -> ?len:int
      -> bigstring
      -> (bigstring, t) parse_result

    (** [input_sexp ?parse_pos ic] like {!input_sexp}, but returns an
        annotated S-expression instead. *)
    val input_sexp : ?parse_pos:Parse_pos.t -> in_channel -> t

    (** [input_sexps ?parse_pos ?buf ic] like {!input_sexps}, but returns
        a list of annotated S-expressions. *)
    val input_sexps : ?parse_pos:Parse_pos.t -> ?buf:bytes -> in_channel -> t list

    (** [input_sexps ?parse_pos ?buf ic] like {!input_rev_sexps}, but
        returns a list of annotated S-expressions. *)
    val input_rev_sexps : ?parse_pos:Parse_pos.t -> ?buf:bytes -> in_channel -> t list

    (** {6 Loading of annotated S-expressions} *)

    (** NOTE: these functions should only be used if an annotated S-expression
        is required. *)

    (** [load_sexp ?strict ?buf file] like {!load_sexp}, but returns an
        annotated S-expression. *)
    val load_sexp : ?strict:bool -> ?buf:bytes -> string -> t

    (** [load_sexps ?buf file] like {!load_sexps}, but returns a list of
        annotated S-expressions. *)
    val load_sexps : ?buf:bytes -> string -> t list

    (** [load_rev_sexps ?buf file] like {!load_rev_sexps}, but returns a
        list of annotated S-expressions. *)
    val load_rev_sexps : ?buf:bytes -> string -> t list

    (** {6 String and bigstring conversions} *)

    (** [of_string str] same as {!of_string}, but returns an annotated
        S-expression. *)
    val of_string : string -> t

    (** [of_bigstring bstr] same as {!of_string}, but operates on bigstrings. *)
    val of_bigstring : bigstring -> t

    (** Converters using annotations for determining error locations *)

    (** [conv f annot_sexp] converts the S-expression associated with
        annotated S-expression [annot_sexp] using [f].  @return [`Result
        res] on success, or [`Error (exn, sub_annot_sexp)] otherwise, where
        [exn] is the exception associated with the conversion error, and
        [sub_annot_sexp] is the annotated S-expression on which conversion
        failed. *)
    val conv : (Type.t -> 'a) -> t -> 'a conv

    (** [get_conv_exn ~file ~exc annot_sexp] @return the exception that
        would be raised for a given [file] and exception [exc]
        if conversion had failed on annotated S-expression [annot_sexp].
        The format of the exception message is "file:line:col" *)
    val get_conv_exn : file:string -> exc:exn -> t -> exn
  end

  (** Type of state maintained during parsing *)
  type 't parse_state = 't Pre_sexp.parse_state = private
    { parse_pos : Parse_pos.t (** Current parse position *) }

  (** Type of parse errors *)
  type parse_error = Pre_sexp.parse_error =
    { err_msg : string (** Reason why parsing failed *)
    ; parse_state :
        [ `Sexp of t list list parse_state | `Annot of Annotated.stack parse_state ]
        (** State of parser *)
    }

  (** Exception raised during partial parsing *)
  exception Parse_error of parse_error

  (** {6 Unannotated (partial) parsing} *)

  (** [parse ?parse_pos ?len str] (partially) parses an S-expression in string buffer
      [str] starting out with position information provided in [parse_pos] and reading at
      most [len] characters.  To parse a single atom that is not delimited by whitespace
      it is necessary to call this function a second time with the returned continuation,
      and a dummy buffer that contains whitespace.

      [parse] starts parsing [str] at position [parse_pos.buf_pos].  Each subsequent
      [parse_fun] from a [Cont] uses the [buf] and [pos] that is supplied to it.  The
      final [parse_fun] that returns [Done] mutates the [buf_pos] in the originally
      supplied [parse_pos], and then returns it.

      @param parse_pos default = [Parse_pos.create ()]
      @param len default = [String.length str - parse_pos.Parse_pos.buf_pos]
  *)
  val parse : ?parse_pos:Parse_pos.t -> ?len:int -> string -> (string, t) parse_result

  (** [parse_bigstring ?parse_pos ?len str] same as {!parse}, but operates on
      bigstrings. *)
  val parse_bigstring
    :  ?parse_pos:Parse_pos.t
    -> ?len:int
    -> bigstring
    -> (bigstring, t) parse_result

  (** [input_sexp ?parse_pos ic] parses an S-expression from input channel
      [ic] using initial position information in [parse_pos].  NOTE: this
      function is not as fast on files as {!Sexp.load_sexp}, and is also
      slightly slower than the scan-functions.  But it is guaranteed that
      [input_sexp] is only going to read data parseable as an S-expression.
      Thus, subsequent input functions will see the data immediately
      following it.

      @param parse_pos default = [Parse_pos.create ()]
  *)
  val input_sexp : ?parse_pos:Parse_pos.t -> in_channel -> t

  (** [input_sexps ?parse_pos ?buf ic] parses whitespace separated
      S-expressions from input channel [ic] until EOF is reached.  Faster than
      the scan-functions.

      @param parse_pos default = [Parse_pos.create ()]
  *)
  val input_sexps : ?parse_pos:Parse_pos.t -> ?buf:bytes -> in_channel -> t list

  (** [input_rev_sexps ?parse_pos ?buf ic] same as {!Sexp.input_sexps},
      but returns a reversed list of S-expressions, which is slightly more
      efficient. *)
  val input_rev_sexps : ?parse_pos:Parse_pos.t -> ?buf:bytes -> in_channel -> t list

  (** {6 Loading of (converted) S-expressions} *)

  (** [load_sexp ?strict ?buf file] reads one S-expression from [file] using
      buffer [buf] for storing intermediate data.  Faster than the
      scan-functions.

      @raise Parse_error if the S-expression is unparseable.

      @raise Failure if parsing reached the end of file before one S-expression
      could be read.

      @raise Failure if [strict] is true and there is more than one
      S-expression in the file.

      @param strict default = [true]
  *)
  val load_sexp : ?strict:bool -> ?buf:bytes -> string -> t

  (** [load_sexps ?buf file] reads a list of whitespace separated S-expressions
      from [file] using buffer [buf] for storing intermediate data.
      Faster than the scan-functions.

      @raise Parse_error if there is unparseable data in the file.

      @raise Failure if parsing reached the end of file before the last
      S-expression could be fully read.
  *)
  val load_sexps : ?buf:bytes -> string -> t list

  (** [load_rev_sexps ?buf file] same as {!Sexp.load_sexps}, but returns a
      reversed list of S-expressions, which is slightly more efficient. *)
  val load_rev_sexps : ?buf:bytes -> string -> t list

  (** [load_sexp_conv ?strict ?buf file f] like {!Sexp.load_sexp}, but
      performs a conversion on the fly using [f].  Performance is equivalent
      to executing {!Sexp.load_sexp} and performing conversion when there
      are no errors.  In contrast to the plain S-expression loader, this
      function not only performs the conversion, it will give exact error
      ranges for conversion errors.

      @raise Parse_error if there is unparseable data in the file.

      @raise Failure if parsing reached the end of file before the last
      S-expression could be fully read.
  *)
  val load_sexp_conv
    :  ?strict:bool
    -> ?buf:bytes
    -> string
    -> (t -> 'a)
    -> 'a Annotated.conv

  (** [load_sexp_conv_exn ?strict ?buf file f] like {!load_sexp_conv},
      but returns the converted value or raises [Of_sexp_error] with exact
      location information in the case of a conversion error. *)
  val load_sexp_conv_exn : ?strict:bool -> ?buf:bytes -> string -> (t -> 'a) -> 'a

  (** [load_sexps_conv ?buf file f] like {!Sexp.load_sexps}, but performs
      a conversion on the fly using [f].  Performance is equivalent to
      executing {!Sexp.load_sexps} and performing conversion when there
      are no errors.  In contrast to the plain S-expression loader, this
      function not only performs the conversion, it will give exact error
      ranges for conversion errors.

      @raise Parse_error if there is unparseable data in the file.

      @raise Failure if parsing reached the end of file before the last
      S-expression could be fully read.
  *)
  val load_sexps_conv : ?buf:bytes -> string -> (t -> 'a) -> 'a Annotated.conv list

  (** [load_sexps_conv_exn ?buf file f] like {!load_sexps_conv}, but returns
      the converted value or raises [Of_sexp_error] with exact location
      information in the case of a conversion error. *)
  val load_sexps_conv_exn : ?buf:bytes -> string -> (t -> 'a) -> 'a list

  (** {6 Output of S-expressions to I/O-channels} *)

  (** NOTE: for performance reasons these output functions may need to
      allocate large strings to write out huge S-expressions.  This may
      cause problems on 32-bit platforms.  If you think that you may need to
      write huge S-expressions on such platforms, you might want to use the
      pretty-printers that write to formatters instead (see further below). *)

  (** [output_hum oc sexp] outputs S-expression [sexp] to output channel
      [oc] in human readable form. *)
  val output_hum : out_channel -> t -> unit

  (** [output_hum_indent indent oc sexp] outputs S-expression [sexp]
      to output channel [oc] in human readable form using indentation level
      [indent].
  *)
  val output_hum_indent : int -> out_channel -> t -> unit

  (** [output_mach oc sexp] outputs S-expression [sexp] to output channel
      [oc] in machine readable (i.e. most compact) form. *)
  val output_mach : out_channel -> t -> unit

  (** [output oc sexp] same as [output_mach]. *)
  val output : out_channel -> t -> unit

  (** {6 Output of S-expressions to file} *)

  (** All save-functions write to a temporary file before moving it into
      place to avoid intermittent garbling of existing files, which may
      cause problems for other processes that try to read. *)

  (** [save_hum ?perm file sexp] outputs S-expression [sexp] to [file] in human
      readable form.

      @param perm default = umask
  *)
  val save_hum : ?perm:int -> string -> t -> unit

  (** [save_mach ?perm file sexp] outputs S-expression [sexp] to [file]
      in machine readable (i.e. most compact) form.

      @param perm default = umask
  *)
  val save_mach : ?perm:int -> string -> t -> unit

  (** [save ?perm file sexp] same as {!save_mach}. *)
  val save : ?perm:int -> string -> t -> unit

  (** [save_sexps_hum ?perm file sexps] outputs S-expression list [sexps] to
      [file] in human readable form, each sexp being followed by a newline.

      @param perm default = umask
  *)
  val save_sexps_hum : ?perm:int -> string -> t list -> unit

  (** [save_sexps_mach ?perm file sexps] outputs S-expression list [sexps] to
      [file] in machine readable form, each sexp being followed by a
      newline.

      @param perm default = umask
  *)
  val save_sexps_mach : ?perm:int -> string -> t list -> unit

  (** [save_sexps ?perm file sexp] same as {!save_sexps_mach}. *)
  val save_sexps : ?perm:int -> string -> t list -> unit

  (** {6 Output of S-expressions to formatters} *)

  (** [pp_hum ppf sexp] outputs S-expression [sexp] to formatter [ppf]
      in human readable form. *)
  val pp_hum : formatter -> t -> unit

  (** [pp_hum_indent n ppf sexp] outputs S-expression [sexp] to formatter
      [ppf] in human readable form and indentation level [n]. *)
  val pp_hum_indent : int -> formatter -> t -> unit

  (** [pp_mach ppf sexp] outputs S-expression [sexp] to formatter [ppf]
      in machine readable (i.e. most compact) form. *)
  val pp_mach : formatter -> t -> unit

  (** [pp ppf sexp] same as [pp_mach]. *)
  val pp : formatter -> t -> unit

  (** {6 String and bigstring conversions} *)

  (** Module encapsulating the exception raised by string converters when
      type conversions fail. *)
  module Of_string_conv_exn : sig
    type t =
      { exc : exn
      ; sexp : Type.t
      ; sub_sexp : Type.t
      }

    exception E of t
  end

  (** [of_string str] converts string [str] to an S-expression.  NOTE:
      trailing whitespace is considered an error, which may be overly
      strict for some applications.  Either strip the string of trailing
      whitespace first, or, even cheaper, use {!parse} instead. *)
  val of_string : string -> t

  (** [of_string_conv str conv] like {!of_string}, but performs type conversion
      with [conv].  @return conversion result. *)
  val of_string_conv : string -> (t -> 'a) -> 'a Annotated.conv

  (** [of_string_conv_exn str conv] like {!of_string_conv}, but raises
      {!Of_string_conv_exn.E} if type conversion fails.  @return converted
      value. *)
  val of_string_conv_exn : string -> (t -> 'a) -> 'a

  (** [of_bigstring bstr] same as {!of_string}, but operates on bigstrings. *)
  val of_bigstring : bigstring -> t

  (** [of_bigstring_conv bstr conv] like {!of_bigstring}, but performs
      type conversion with [conv].  @return conversion result. *)
  val of_bigstring_conv : bigstring -> (t -> 'a) -> 'a Annotated.conv

  (** [of_bigstring_conv_exn bstr conv] like {!of_bigstring_conv}, but raises
      {!Of_string_conv_exn.E} if type conversion fails.  @return converted
      value. *)
  val of_bigstring_conv_exn : bigstring -> (t -> 'a) -> 'a

  (** [to_string_hum ?indent sexp] converts S-expression [sexp] to a
      string in human readable form with indentation level [indent].

      @param indent default = [!default_indent]
  *)
  val to_string_hum : ?indent:int -> t -> string

  (** [to_string_mach sexp] converts S-expression [sexp] to a string in
      machine readable (i.e. most compact) form. *)
  val to_string_mach : t -> string

  (** [to_string sexp] same as [to_string_mach]. *)
  val to_string : t -> string

  (** {6 Buffer conversions} *)

  (** [to_buffer_hum ~buf ?indent sexp] outputs the S-expression [sexp]
      converted to a string in human readable form to buffer [buf].

      @param indent default = [!default_indent]
  *)
  val to_buffer_hum : buf:Buffer.t -> ?indent:int -> t -> unit

  (** [to_buffer_mach ~buf sexp] outputs the S-expression [sexp] converted
      to a string in machine readable (i.e. most compact) form to buffer [buf].
  *)
  val to_buffer_mach : buf:Buffer.t -> t -> unit

  (** [to_buffer ~buf sexp] same as {!to_buffer_mach}. *)
  val to_buffer : buf:Buffer.t -> t -> unit

  (** [to_buffer_gen ~buf ~add_char ~add_string sexp] outputs the S-expression
      [sexp] converted to a string to buffer [buf] using the output functions
      [add_char] and [add_string]. *)
  val to_buffer_gen
    :  buf:'buffer
    -> add_char:('buffer -> char -> unit)
    -> add_string:('buffer -> string -> unit)
    -> t
    -> unit

  (** {6 Utilities for automated type conversions} *)

  (** [unit] the unit-value as expressed by an S-expression. *)
  val unit : t

  val is_unit : t -> bool

  (** [sexp_of_t sexp] maps S-expressions which are part of a type with
      automated S-expression conversion to themselves. *)
  val sexp_of_t : t -> t

  (** [t_of_sexp sexp] maps S-expressions which are part of a type with
      automated S-expression conversion to themselves. *)
  val t_of_sexp : t -> t

  val t_sexp_grammar : Sexplib0.Private.Raw_grammar.t

  (** {6 Utilities for conversion error handling} *)

  (** Type of successful search results.  [`Found] means that an
      S-expression was found at the immediate position, and [`Pos (pos,
      found)] indicates that it was found at position [pos] within a
      structure (= S-expression list) where [found] describes recursively
      where it was found in that structure. *)
  type found =
    [ `Found
    | `Pos of int * found
    ]

  (** Type of search results.  [`Not_found] means that an
      S-expression was not found within another S-expression. *)
  type search_result =
    [ `Not_found
    | found
    ]

  (** [search_physical sexp ~contained] @return the search result
      indicating whether, and if, where the S-expression [contained]
      was found within S-expression [sexp]. *)
  val search_physical : t -> contained:t -> search_result

  (** [subst_found sexp ~subst found] @return the S-expression that
      results from substituting [subst] within S-expression [sexp]
      at the location described by [found]. *)
  val subst_found : t -> subst:t -> found -> t

  (** S-expressions annotated with relative source positions and comments *)
  module With_layout : sig

    (* relative source positions *)
    type pos = Src_pos.Relative.t =
      { row : int
      ; col : int
      }

    val sexp_of_pos : pos -> Type.t

    (** S-expressions annotated with relative source positions and comments. All the
        positions are relative to the opening paren of the enclosing list, or the first
        character of the file. *)
    type t = Type_with_layout.t =
      | Atom of pos * string * string option (* second is quoted representation *)
      | List of pos * t_or_comment list * pos

    (* positions of left and right parens *)
    and t_or_comment = Type_with_layout.t_or_comment =
      | Sexp of t
      | Comment of comment

    and comment = Type_with_layout.comment =
      | Plain_comment of pos * string (* line or block comment *)
      | Sexp_comment of pos * comment list * t

    (* position of #; *)

    val sexp_of_t : t -> Type.t
    val sexp_of_comment : comment -> Type.t
    val sexp_of_t_or_comment : t_or_comment -> Type.t

    module Forget : sig
      val t : t -> Type.t
      val t_or_comment : t_or_comment -> Type.t option
      val t_or_comments : t_or_comment list -> Type.t list
    end


    module Render : sig
      type asexp
      type 'a t (* monad for position-respecting asexp rendering *)

      val return : 'a -> 'a t
      val bind : 'a t -> f:('a -> 'b t) -> 'b t
      val sexp : asexp -> unit t (* assumes that positions in [asexp] are relative *)

      val run : (char -> unit) -> unit t -> unit
    end
    with type asexp := t_or_comment

    module Parser : sig
      type token

      val sexp : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> t_or_comment
      val sexp_opt : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> t_or_comment option
      val sexps : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> t_or_comment list
      val rev_sexps : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> t_or_comment list

      (* for debugging only, cannot be used otherwise anyway *)
      val sexps_abs
        :  (Lexing.lexbuf -> token)
        -> Lexing.lexbuf
        -> Type_with_layout.Parsed.t_or_comment list
    end

    module Lexer : sig
      val main : ?buf:Buffer.t -> Lexing.lexbuf -> Parser.token
    end
  end
end
