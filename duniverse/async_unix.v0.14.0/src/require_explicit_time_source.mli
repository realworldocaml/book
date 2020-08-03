(** Deprecates functions that use wall-clock time, so that code must be explicit about
    what time source is used.

    Idiomatic usage is:

    {[ open! Require_explicit_time_source ]}

    or, in an import.ml:

    {[include Require_explicit_time_source]} *)

include Require_explicit_time_source_intf.Require_explicit_time_source (** @open *)
