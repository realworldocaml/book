module Types = struct
  type bound = [ `Unlimited | `Limit of int ]
  type filter = name:string -> index:int -> [ `Run | `Skip ]

  type t =
    < and_exit : bool
    ; verbose : bool
    ; compact : bool
    ; tail_errors : bound
    ; quick_only : bool
    ; show_errors : bool
    ; json : bool
    ; filter : filter option
    ; log_dir : string
    ; bail : bool
    ; record_backtrace : bool >

  type 'a with_options =
    ?and_exit:bool ->
    ?verbose:bool ->
    ?compact:bool ->
    ?tail_errors:bound ->
    ?quick_only:bool ->
    ?show_errors:bool ->
    ?json:bool ->
    ?filter:filter ->
    ?log_dir:string ->
    ?bail:bool ->
    ?record_backtrace:bool ->
    'a
end

module type Config = sig
  include module type of Types

  module User : sig
    type t
    (** The type of configurations supplied by the user, with defaults not yet
        supplied. *)

    val create : (unit -> t) with_options
    (** Build a config object with the supplied options. *)

    val kcreate : (t -> 'a) -> 'a with_options
    (** Like [create], but passes the constructed config to a continuation
        rather than returning directly. *)

    val term : and_exit:bool -> record_backtrace:bool -> t Cmdliner.Term.t
    (** [term] provides a command-line interface for building configs. *)

    val ( || ) : t -> t -> t
    (** Merge two configs, with fields from the left taking priority over those
        in the right. *)

    (** {2 Accessors} *)

    val and_exit : t -> bool
    val record_backtrace : t -> bool
  end

  val apply_defaults : default_log_dir:string -> User.t -> t
end
