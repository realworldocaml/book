(** Extensible character buffers.

    This module implements character buffers that automatically expand as necessary.  It
    provides cumulative concatenation of strings in quasi-linear time (instead of
    quadratic time when strings are concatenated pairwise).
*)

include Buffer_intf.Buffer (** @inline *)
