(* (c) 2017 Hannes Mehnert, all rights reserved *)

type 'a t
(** The type of a domain name, a sequence of labels separated by dots.  Each
   label may contain any bytes. The length of each label may not exceed 63
   characters.  The total length of a domain name is limited to 253 (its byte
   representation is 255), but other protocols (such as SMTP) may apply even
   smaller limits.  A domain name label is case preserving, comparison is done
   in a case insensitive manner.  Every [t] is a fully qualified domain name,
   its last label is the [root] label. The specification of domain names
   originates from {{:https://tools.ietf.org/html/rfc1035}RFC 1035}.

    The invariants on the length of domain names are preserved throughout the
   module - no [t] will exist which violates these.

    Phantom types are used for further name restrictions, {!host} checks for
   host names ([`host t]): only letters, digits, and hyphen allowed, hyphen not
   first character of a label, the last label must contain at least on letter.
   {!service} checks for a service name ([`service t]): its first label is a
   service name: 1-15 characters, no double-hyphen, hyphen not first or last
   charactes, only letters, digits and hyphen allowed, and the second label is a
   protocol ([_tcp] or [_udp] or [_sctp]).

    When a [t] is constructed (either from a string, etc.), it is a [`raw t].
   Subsequent modifications, such as adding or removing labels, appending, of
   any kind of name also result in a [`raw t], which needs to be checked for
   [`host t] (using {!host}) or [`service t] (using {!service}) if desired.

    Constructing a [t] (via {!of_string}, {!of_string_exn}, {!of_strings} etc.)
   does not require a trailing dot.


    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {2 Constructor} *)

val root : [ `raw ] t
(** [root] is the root domain ("."), the empty label. *)

(** {2 String representation} *)

val of_string : string -> ([ `raw ] t, [> `Msg of string ]) result
(** [of_string name] is either [t], the domain name, or an error if the provided
    [name] is not a valid domain name. A trailing dot is not requred. *)

val of_string_exn : string -> [ `raw ] t
(** [of_string_exn name] is [t], the domain name. A trailing dot is not
    required.

    @raise Invalid_argument if [name] is not a valid domain name. *)

val to_string : ?trailing:bool -> 'a t -> string
(** [to_string ~trailing t] is [String.concat ~sep:"." (to_strings t)], a
    human-readable representation of [t].  If [trailing] is provided and
    [true] (defaults to [false]), the resulting string will contain a trailing
    dot. *)

(** {2 Predicates and basic operations} *)

val canonical : 'a t -> 'a t
(** [canonical t] is [t'], the canonical domain name, as specified in RFC 4034
    (and 2535): all characters are lowercase. *)

val host : 'a t -> ([ `host ] t, [> `Msg of string ]) result
(** [host t] is a [`host t] if [t] is a hostname: the contents of the domain
    name is limited: each label may start with a digit or letter, followed by
    digits, letters, or hyphens. *)

val host_exn : 'a t -> [ `host ] t
(** [host_exn t] is a [`host t] if [t] is a hostname: the contents of the domain
    name is limited: each label may start with a digit or letter, followed by
    digits, letters, or hyphens.

    @raise Invalid_argument if [t] is not a hostname. *)

val service : 'a t -> ([ `service ] t, [> `Msg of string ]) result
(** [service t] is [`service t] if [t] contains a service name, the following
    conditions have to be met:
    The first label is a service name (or port number); an underscore preceding
    1-15 characters from the set [- a-z A-Z 0-9].
    The service name may not contain a hyphen ([-]) following another hyphen;
    no hyphen at the beginning or end.

    The second label is the protocol, one of [_tcp], [_udp], or [_sctp].
    The remaining labels must form a valid hostname.

    This function can be used to validate RR's of the types SRV (RFC 2782)
    and TLSA (RFC 7671). *)

val service_exn : 'a t -> [ `service ] t
(** [service_exn t] is [`service t] if [t] is a service name (see {!service}).

    @raise Invalid_argument if [t] is not a service names. *)

val raw : 'a t -> [ `raw ] t
(** [raw t] is the [`raw t]. *)

val count_labels : 'a t -> int
(** [count_labels name] returns the amount of labels in [name]. *)

val is_subdomain : subdomain:'a t -> domain:'b t -> bool
(** [is_subdomain ~subdomain ~domain] is [true] if [subdomain] contains any
    labels prepended to [domain]: [foo.bar.com] is a subdomain of [bar.com] and
    of [com], [sub ~subdomain:x ~domain:root] is true for all [x]. *)

val get_label : ?rev:bool -> 'a t -> int -> (string, [> `Msg of string ]) result
(** [get_label ~rev name idx] retrieves the label at index [idx] from [name]. If
    [idx] is out of bounds, an Error is returned. If [rev] is provided and [true]
    (defaults to [false]), [idx] is from the end instead of the beginning. *)

val get_label_exn : ?rev:bool -> 'a t -> int -> string
(** [get_label_exn ~rev name idx] is the label at index [idx] in [name].

    @raise Invalid_argument if [idx] is out of bounds in [name]. *)

val find_label : ?rev:bool -> 'a t -> (string -> bool) -> int option
(** [find_label ~rev name p] returns the first position where [p lbl] is [true]
    in [name], if it exists, otherwise [None]. If [rev] is provided and [true]
    (defaults to [false]), the [name] is traversed from the end instead of the
    beginning. *)

val find_label_exn : ?rev:bool -> 'a t -> (string -> bool) -> int
(** [find_label_exn ~rev name p], see {!find_label}.

    @raise Invalid_argument if [p] does not return [true] in [name]. *)

(** {2 Label addition and removal} *)
val prepend_label : 'a t -> string -> ([ `raw ] t, [> `Msg of string ]) result
(** [prepend_label name pre] is either [t], the new domain name, or an error. *)

val prepend_label_exn : 'a t -> string -> [ `raw ] t
(** [prepend_label_exn name pre] is [t], the new domain name.

    @raise Invalid_argument if [pre] is not a valid domain name. *)

val drop_label : ?rev:bool -> ?amount:int -> 'a t ->
  ([ `raw ] t, [> `Msg of string ]) result
(** [drop_label ~rev ~amount t] is either [t], a domain name with [amount]
    (defaults to [1]) labels dropped from the beginning - if [rev] is provided
    and [true] (defaults to [false]) labels are dropped from the end.
    [drop_label (of_string_exn "foo.com") = Ok (of_string_exn "com")],
    [drop_label ~rev:true (of_string_exn "foo.com") = Ok (of_string_exn "foo")].
*)

val drop_label_exn : ?rev:bool -> ?amount:int -> 'a t -> [ `raw ] t
(** [drop_label_exn ~rev ~amount t], see {!drop_label}. Instead of a [result],
    the value is returned directly.

    @raise Invalid_argument if there are not sufficient labels. *)

val append : 'a t -> 'b t -> ([ `raw ] t, [> `Msg of string ]) result
(** [append pre post] is [pre ^ "." ^ post]. *)

val append_exn : 'a t -> 'b t -> [ `raw ] t
(** [append_exn pre post] is [pre ^ "." ^ post].

    @raise Invalid_argument if the result would violate length restrictions. *)

(** {2 Comparison} *)

val equal : ?case_sensitive:bool -> 'a t -> 'b t -> bool
(** [equal ~case_sensitive t t'] is [true] if all labels of [t] and [t'] are
    equal. If [case_sensitive] is provided and [true], the cases of the labels
    are respected (defaults to [false]). *)

val compare : 'a t -> 'b t -> int
(** [compare t t'] compares the domain names [t] and [t'] using a case
    insensitive string comparison. *)

val equal_label : ?case_sensitive:bool -> string -> string -> bool
(** [equal_label ~case_sensitive a b] is [true] if [a] and [b] are equal
    ignoring casing. If [case_sensitive] is provided and [true] (defaults to
    [false]), the casing is respected. *)

val compare_label : string -> string -> int
(** [compare_label t t'] compares the labels [t] and [t'] using a case
    insensitive string comparison. *)

(** {2 Collections} *)

module Host_map : sig
  include Map.S with type key = [ `host ] t

  (** [find key t] is [Some a] where a is the binding of [key] in [t]. [None] if
      the [key] is not present. *)
  val find : key -> 'a t -> 'a option
end
(** The module of a host name map *)

module Host_set : Set.S with type elt = [ `host ] t
(** The module of a host name set *)

module Service_map : sig
  include Map.S with type key = [ `service ] t

  (** [find key t] is [Some a] where a is the binding of [key] in [t]. [None] if
      the [key] is not present. *)
  val find : key -> 'a t -> 'a option
end
(** The module of a service name map *)

module Service_set : Set.S with type elt = [ `service ] t
(** The module of a service name set *)

module Map : sig
  include Map.S with type key = [ `raw ] t

  (** [find key t] is [Some a] where a is the binding of [key] in [t]. [None] if
      the [key] is not present. *)
  val find : key -> 'a t -> 'a option
end
(** The module of a domain name map *)

module Set : Set.S with type elt = [ `raw ] t
(** The module of a domain name set *)

(** {2 String list representation} *)

val of_strings : string list -> ([ `raw ] t, [> `Msg of string ]) result
(** [of_strings labels] is either [t], a domain name, or an error if
    the provided [labels] violate domain name constraints. A trailing empty
    label is not required. *)

val of_strings_exn : string list -> [ `raw ] t
(** [of_strings_exn labels] is [t], a domain name.  A trailing empty
    label is not required.

    @raise Invalid_argument if [labels] are not a valid domain name. *)

val to_strings : ?trailing:bool -> 'a t -> string list
(** [to_strings ~trailing t] is the list of labels of [t].  If [trailing] is
    provided and [true] (defaults to [false]), the resulting list will contain
    a trailing empty label. *)

(** {2 Pretty printer} *)

val pp : 'a t Fmt.t
(** [pp ppf t] pretty prints the domain name [t] on [ppf]. *)

(**/**)
(* exposing internal structure, used by udns (but could as well use Obj.magic *)

val of_array : string array -> [ `raw ] t
(** [of_array a] is [t], a domain name from [a], an array containing a reversed
    domain name. *)

val to_array : 'a t -> string array
(** [to_array t] is [a], an array containing the reversed domain name of [t]. *)
