(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(**[Make()] creates a new abstract type [t] that is isomorphic to [string].
   The functions [import] and [export] witness this isomorphism. A value of
   type [t] is internally represented as a unique integer code; a mutable
   table keeps track of all strings that have ever been encountered. *)
module Make () : sig

  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  val import : string -> t
  val export : t -> string

  module Set : sig

    include Set.S with type elt = t

    val import: StringSet.t -> t

    (**[print] prints a set of strings as a comma-separated list,
       without opening and closing delimiters. *)
    val print : t -> string

  end

  module Map : sig

    include Map.S with type key := t

    val domain : 'a t -> Set.t

    val restrict: Set.t -> 'a t -> 'a t

  end

end
