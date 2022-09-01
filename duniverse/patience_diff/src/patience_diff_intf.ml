(** This is a port of Bram Cohen's patience diff algorithm, as found in the Bazaar 1.14.1
    source code, available at http://bazaar-vcs.org.

    This copyright notice was included:

    # Copyright (C) 2005 Bram Cohen, Copyright (C) 2005, 2006 Canonical Ltd
    #
    # This program is free software; you can redistribute it and/or modify
    # it under the terms of the GNU General Public License as published by
    # the Free Software Foundation; either version 2 of the License, or
    # (at your option) any later version.
    #
    # This program is distributed in the hope that it will be useful,
    # but WITHOUT ANY WARRANTY; without even the implied warranty of
    # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    # GNU General Public License for more details.
    #
    # You should have received a copy of the GNU General Public License
    # along with this program; if not, write to the Free Software
    # Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)


(** Bram Cohen's comment from the original Python code (with syntax changed to OCaml):

    [get_matching_blocks a b] returns a list of triples describing matching
    subsequences.

    Each triple is of the form (i, j, n), and means that
    a <|> (i,i+n) = b <|> (j,j+n).  The triples are monotonically increasing in
    i and in j.

    The last triple is a dummy, (Array.length a, Array.length b, 0), and is the only
    triple with n=0.

    Example:
    get_matching_blocks [|"a";"b";"x";"c";"d"|] [|"a";"b";"c";"d"|]
    returns
    [(0, 0, 2), (3, 2, 2), (5, 4, 0)]
*)

open! Core
module Hunk = Hunk
module Hunks = Hunks
module Matching_block = Matching_block
module Range = Range

module type S = sig
  type elt

  (** Get_matching_blocks not only aggregates the data from [matches a b] but also
      attempts to remove random, semantically meaningless matches ("semantic cleanup").
      The value of [big_enough] governs how aggressively we do so.  See [get_hunks]
      below for more details. *)
  val get_matching_blocks
    :  transform:('a -> elt)
    -> ?big_enough:int
    -> prev:'a array
    -> next:'a array
    -> unit
    -> Matching_block.t list

  (** [matches a b] returns a list of pairs (i,j) such that a.(i) = b.(j) and such that
      the list is strictly increasing in both its first and second coordinates.  This is
      essentially a "unfolded" version of what [get_matching_blocks] returns. Instead of
      grouping the consecutive matching block using [length] this function would return
      all the pairs (prev_start * next_start). *)
  val matches : elt array -> elt array -> (int * int) list

  (** [match_ratio ~compare a b] computes the ratio defined as:

      {[
        2 * len (matches a b) / (len a + len b)
      ]}

      It is an indication of how much alike a and b are.  A ratio closer to 1.0 will
      indicate a number of matches close to the number of elements that can potentially
      match, thus is a sign that a and b are very much alike.  On the next hand, a low
      ratio means very little match. *)
  val match_ratio : elt array -> elt array -> float

  (** [get_hunks ~transform ~context ~prev ~next] will compare the arrays [prev] and
      [next] and produce a list of hunks. (The hunks will contain Same ranges of at most
      [context] elements.)  Negative [context] is equivalent to infinity (producing a
      singleton hunk list).  The value of [big_enough] governs how aggressively we try to
      clean up spurious matches, by restricting our attention to only matches of length
      less than [big_enough].  Thus, setting [big_enough] to a higher value results in
      more aggressive cleanup, and the default value of 1 results in no cleanup at all.
      When this function is called by [Patdiff_core], the value of [big_enough] is 3 at
      the line level, and 7 at the word level. *)
  val get_hunks
    :  transform:('a -> elt)
    -> context:int
    -> ?big_enough:int
    -> prev:'a array
    -> next:'a array
    -> unit
    -> 'a Hunk.t list

  type 'a segment =
    | Same of 'a array
    | Different of 'a array array

  type 'a merged_array = 'a segment list

  val merge : elt array array -> elt merged_array
end

module type Patience_diff = sig
  module Hunk = Hunk
  module Hunks = Hunks
  module Matching_block = Matching_block
  module Range = Range
  module Make (Elt : Hashtbl.Key) : S with type elt = Elt.t

  (* [String] uses String.compare *)
  module String : S with type elt = string

  module Stable : sig
    module Hunk = Hunk.Stable
    module Hunks = Hunks.Stable
    module Matching_block = Matching_block.Stable
    module Range = Range.Stable
  end
end
