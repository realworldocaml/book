(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(** Embed typed ASN.1 grammars in OCaml

    Skip the notation part of Abstract Syntax Notation, and embed the abstract
    syntax directly in OCaml.

    {b References}
    {ul
    {- ITU-T. {{:http://handle.itu.int/11.1002/1000/12479}Abstract Syntax Notation One (ASN.1): Specification of basic notation}. ITU-T X.680 | ISO/IEC 8824-1, 2015}
    {- ITU-T. {{:http://handle.itu.int/11.1002/1000/12483 }ASN.1 encoding rules: Specification of Basic Encoding Rules (BER), Canonical Encoding Rules (CER) and Distinguished Encoding Rules (DER)}. ITU-T X.690 | ISO/IEC 8825-1, 2015}}

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

open Result

(** {1 Object identifiers} *)

type oid
(** ASN.1 [OBJECT IDENTIFIER]. *)

(** Object identifiers.

    Magic numbers in a suit and tie. Their consulting fee is astronomical. *)
module OID : sig

  (** {1 Object identifiers} *)

  type t = oid
  (** OIDs are conceptually a sequence of non-negative integers, called
      {e nodes}.

      Every OID has at least two nodes. *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val seeded_hash : int -> t -> int

  (** {1 Construction} *)

  val base : int -> int -> t
  (** [base n1 n2] is the OID [n1.n2].

      Either [n1] is [[0..1]] and [n2] is [[0..39]] (inclusive), or [n1] is [2]
      and [n2] is non-negative.

      @raise Invalid_argument if the components are out of range. *)

  val (<|) : t -> int -> t
  (** [oid <| n] is the OID [oid.n].

      @raise Invalid_argument if [n] is negative. *)

  val (<||) : t -> int list -> t
  (** [oid <|| ns] is the old [oid.n1.n2. ...] if [ns] is [[n1; n2; ...]].

      @raise Invalid_argument if any of [ns] is negative. *)

  (** {1 Conversion} *)

  val to_nodes : t -> int * int * int list
  (** [to_nodes oid] are the nodes this [oid] consists of. Every OID has at
      least two nodes; the rest are collected in the list. *)

  val of_nodes : int -> int -> int list -> t option
  (** [of_nodes n1 n2 ns] is the oid [n1.n2.ns...], or [None], if any of the
      components are out of range. See {{!base}[base]} and {{!(<|)}[<|]}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf oid] pretty-prints [oid] on [ppf] as dotted-decimal. *)

  val of_string : string -> t option
  (** [of_string s] is the OID represented by [s], or [None], if [s] is not
      dotted-decimal or the components are out of range. *)
end


(** {1 ASN.1 Abstract Syntax} *)

type 'a t
(** Abstract syntax of values of type ['a]. *)


(** ASN.1 Abstract Syntax.

    This module is the OCaml term-level analogue of ASN.1's surface notation.

    It provides a ground type {{!S.t}['a t]} representing typed abstract syntax,
    a suite of primitives that correspond to ASN.1 primitives, and a suite of
    combinators that correspond to the combining structures of ASN.1.

    ASN.1 naming and modules are not supported; these are provided by the host
    language instead. *)
module S : sig

  (** {1 ASN.1 Abstract Syntax} *)

  (** ['a t] denotes a particular structure of data, irrespective of any
      encoding, that is represented by ['a] in OCaml. *)
  type nonrec 'a t = 'a t

  (** {1 Basic combinators} *)

  val fix : ('a t -> 'a t) -> 'a t
  (** [fix fasn] is the fixpoint, allowing [fasn] to construct a syntax that
      refers to itself. *)

  val map : ?random:(unit -> 'b) -> ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t
  (** [map ?random f g asn] creates a derived syntax that encodes and decodes
      like [asn], but uses [f] to project and [g] to inject.

      [~random] is a function that generates random samples of ['b]. Defaults to
      [f a] where [a] is a random ['a]. *)

  (** {1 Tags} *)

  type cls = [ `Universal | `Application | `Private ]
  (** ASN.1 tag CLASS. *)

  val implicit : ?cls:cls -> int -> 'a t -> 'a t
  (** [implicit ?cls n asn] is the ASN.1 [IMPLICIT] construct, changing the tag
      of [asn] to [(cls, n)].

      [n] is the tag value.

      [~cls] is the class. Defaults to [CONTEXT SPECIFIC].

      {b Note} [implicit] implicitly becomes [explicit] when applied to nodes
      that cannot be made [IMPLICIT], like [CHOICE]. This is consistent with
      X.608 (see [31.2.7]) in case of a bare tag, and with the common practice
      in case of a tag marked as [IMPLICIT]. *)

  val explicit : ?cls:cls -> int -> 'a t -> 'a t
  (** [explicit ?cls n asn] is the ASN.1 [EXPLICIT] construct, changing the tag
      of [asn] to [(cls, n)].

      [n] is the tag value.

      [~cls] is the class. Defaults to [CONTEXT SPECIFIC]. *)

  (** {1 Combining constructs}

      These look like
{[sequence @@
    (required ~label:"l1" asn1)
  @ (optional ~label:"l2" asn2)
  @ (required ~label:"l3" asn3)
 -@ (optional ~label:"l4" asn4)]}
      or
{[choice3 asn1 asn2 asn3]} *)

  type 'a element
  (** An [element] is a single slot in a {{!sequence}[sequence]}. *)

  val required : ?label:string -> 'a t -> 'a element
  (** [required ?label asn] is a regular sequence element.

      [~label] is the name of the element. *)

  val optional : ?label:string -> 'a t -> 'a option element
  (** [optional ?label asn] is a sequence element marked with the
      ASN.1 [OPTIONAL] keyword.

      [~label] is the name of the element. *)

  type 'a sequence
  (** A [sequence] is the body of a multi-field ASN.1 construct, like
     [SEQUENCE] and [SET]. *)

  val single : 'a element -> 'a sequence
  (** [single e] is the singleton sequence containing just [e]. *)

  val ( @ ) : 'a element -> 'b sequence -> ('a * 'b) sequence
  (** [e @ seq] extends [seq] by prepending [e]. *)

  val ( -@ ) : 'a element -> 'b element  -> ('a * 'b) sequence
  (** [e -@ e1] is [e @ single e1] *)

  val sequence : 'a sequence -> 'a t
  (** [sequence seq] is the ASN.1 [SEQUENCE] construct, with the body [seq]. *)

  val sequence_of : 'a t -> 'a list t
  (** [sequence_of] is the ASN.1 [SEQUENCE OF] construct. *)

  val sequence2 : 'a element -> 'b element -> ('a * 'b) t
  (** [sequence2 e1 e2] is [sequence (e1 -@ e2)]. Other [sequenceN] functions
      are analogous. *)

  val sequence3 :
    'a element ->
    'b element -> 'c element -> ('a * 'b * 'c) t

  val sequence4 :
    'a element ->
    'b element ->
    'c element -> 'd element -> ('a * 'b * 'c * 'd) t

  val sequence5 :
    'a element ->
    'b element ->
    'c element ->
    'd element -> 'e element -> ('a * 'b * 'c * 'd * 'e) t

  val sequence6 :
    'a element ->
    'b element ->
    'c element ->
    'd element ->
    'e element ->
    'f element -> ('a * 'b * 'c * 'd * 'e * 'f) t

  val set : 'a sequence -> 'a t
  (** [seq seq] is the ASN.1 [SET] construct, with the body [seq]. *)

  val set_of : 'a t -> 'a list t
  (** [set_of asn] is the ASN.1 [SET OF] construct. *)

  val set2 : 'a element -> 'b element -> ('a * 'b) t
  (** [set2 e1 e2] is [set (e1 -@ e2)]. Other [setN] functions are analogous. *)

  val set3 :
    'a element ->
    'b element -> 'c element -> ('a * 'b * 'c) t

  val set4 :
    'a element ->
    'b element ->
    'c element -> 'd element -> ('a * 'b * 'c * 'd) t

  val set5 :
    'a element ->
    'b element ->
    'c element ->
    'd element -> 'e element -> ('a * 'b * 'c * 'd * 'e) t

  val set6 :
    'a element ->
    'b element ->
    'c element ->
    'd element ->
    'e element ->
    'f element -> ('a * 'b * 'c * 'd * 'e * 'f) t

  val choice2 :
    'a t -> 'b t -> [ `C1 of 'a | `C2 of 'b ] t
  (** [choice2 asn1 asn2] is the ASN.1 [CHOICE] construct, choosing between
      [asn1] and [asn2]. Other [choiceN] functions are analogous.

      Larger [CHOICE] can be obtained by nesting [choice] variants.

      {b Note} [CHOICE] containing elements with the same tag yields an illegal
      syntax. This will be detected by {!codec}. *)

  val choice3 :
    'a t -> 'b t -> 'c t
    -> [ `C1 of 'a | `C2 of 'b | `C3 of 'c ] t

  val choice4 :
    'a t -> 'b t -> 'c t -> 'd t
    -> [ `C1 of 'a | `C2 of 'b | `C3 of 'c | `C4 of 'd ] t

  val choice5 :
    'a t -> 'b t -> 'c t -> 'd t -> 'e t
    -> [ `C1 of 'a | `C2 of 'b | `C3 of 'c | `C4 of 'd | `C5 of 'e ] t

  val choice6 :
    'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
    -> [ `C1 of 'a | `C2 of 'b | `C3 of 'c | `C4 of 'd | `C5 of 'e | `C6 of 'f ] t

  (** {1 Primitives} *)

  val bool : bool t
  (** [bool] is ASN.1 [BOOLEAN]. *)

  val integer : Z.t t
  (** [integer] is ASN.1 [INTEGER]. *)

  val bit_string : bool array t
  (** [bit_string] is ASN.1 [BIT STRING]. *)

  val bit_string_cs : Cstruct.t t
  (** [bit_string_cs] is ASN.1 [BIT STRING], represented as {!Cstruct.t}, and
      padded with 0-bits up to the next full octet. *)

  val octet_string : Cstruct.t  t
  (** [octet_string] is ASN.1 [OCTET STRING]. *)

  val null : unit t
  (** [null] is ASN.1 [NULL]. *)

  val oid : oid t
  (** [oid] is ASN.1 [OBJECT IDENTIFIER]. *)

  val enumerated : (int -> 'a) -> ('a -> int) -> 'a t
  (** [enumerated f g] is ASN.1 [ENUMERATED], with [f] projecting from, and [g]
      injecting into an [int].

      The full [INTEGER] range is {i not} supported. *)

  val generalized_time : Ptime.t t
  (** [generalized_time] is ASN.1 [GeneralizedTime]. *)

  val utc_time : Ptime.t t
  (** [utc_time] is ASN.1 [UTCTime].

      Representable years are 1951–2050. *)

  (** {2 String primitives}

      Various ASN.1 stringy types.

      {b Note} Presently, no conversion or validation is performed on strings.
      They differ only in tags. *)

  val utf8_string      : string t
  val numeric_string   : string t
  val printable_string : string t
  val teletex_string   : string t
  val videotex_string  : string t
  val ia5_string       : string t
  val graphic_string   : string t
  val visible_string   : string t
  val general_string   : string t
  val universal_string : string t
  val bmp_string       : string t

  (** {2 Additional primitives} *)

  val int : int t
  (** [int] is ASN.1 [INTEGER], projected into an OCaml [int]. *)

  val bit_string_flags : (int * 'a) list -> 'a list t
  (** [bit_string_flags xs] is ASN.1 [BIT STRING], represented as a collection
      of values.

      [xs] is a list of [(bit, x)], where bit [bit] denotes the presence of [x]. *)

  (** {1 Errors} *)

  (* XXX repeats *)
  val error : [ `Parse of string ] -> 'a
  (** [error err] aborts parsing with the {{!error}[error]} [err].

      Aborting the parse is useful, for example, in the [f] argument to
      {{!map}[map]}. *)

  val parse_error : ('a, Format.formatter, unit, 'b) format4 -> 'a
  (** [parse_error fmt ...] aborts parsing with the message produced by using
      [fmt] to format arguments [...]. *)
end

(** {1 Encoding formats} *)

type encoding

val ber : encoding
(** [ber] is ASN.1 Basic Encoding Rules (BER). *)

val der : encoding
(** [der] is ASN.1 Distinguished Encoding Rules (DER). *)


(** {1 Encoding and decoding} *)

type 'a codec

exception Ambiguous_syntax

val codec : encoding -> 'a t -> 'a codec
(** [codec enc asn] represents the syntax [asn] encoded under the rules [enc].

    This function performs work up-front, and is generally expected to be called
    in the static context on statically known syntaxes.

    @raise Ambiguous_syntax if [asn] contains [CHOICE] constructs over
    sub-syntaxes with the same tags. *)

val encode : 'a codec -> 'a -> Cstruct.t
(** [encode codec x] is the encoding of [x], using [codec]. *)

val encode_into : 'a codec -> 'a -> (int * (Cstruct.t -> unit))
(** [encode_into codec x] is the pair [(n, f)], where [n] is the length of [x]
    encoded with [codec], and [f] is a function that will write the encoded [x]
    to the first [n] bytes of the provided {!Cstruct.t}. *)

type error = [ `Parse of string ]
(** Parse errors. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf err] pretty-prints [err] on [ppf]. *)

val decode : 'a codec -> Cstruct.t -> ('a * Cstruct.t, error) result
(** [decode codec cs] is the pair [(x, cs')], where [x] is the result of
    decoding the prefix of [cs] with [codec] and [cs'] are the trailing bytes,
    or an {!error}. *)

(** {1 Misc} *)

val random : 'a t -> 'a
(** [random asn] is a random inhabitant of ['a]. *)
