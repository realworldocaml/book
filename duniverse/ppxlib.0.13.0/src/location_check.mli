(** Enforces some invariants on AST nodes locations *)

(** {2 What?}

    The invariants are as follow:
    - AST nodes are requested to be well nested wrt. locations
    - the locations of "sibling" AST nodes should not overlap

    {2 Why?}

    This is required for merlin to behave properly.

    Indeed, for almost any query directed at merlin, it will need to inspect the
    context around the user's cursor to give an answer that makes sense. And the
    only input it has to do that is the position of the cursor in the buffer.
    The handling of most queries starts by traversing the AST, using the
    locations of nodes to select the right branch. (1) is necessary to avoid
    discarding subtrees too early, (2) is used to avoid merlin making arbitrary
    choices (if you ask for the type under the cursor, and there seem to be two
    things under the cursor, merlin will need to pick one).

    {2 Guidelines for writing well-behaved ppxes}

    It's obviously not always (indeed rarely) possible to mint new locations
    when manipulating the AST.

    The intended way to deal with locations is this:
    - AST nodes that exist in the source should keep their original location
    - new nodes should be given a "ghost" location (i.e.
    [{ some_loc with loc_ghost = true }]) to indicate that the node doesn't
    exist in the sources.

    Both the new check and merlin will happily traverse these ghost nodes as if
    they didn't exist.
    Note: this comes into play when deciding which nodes are "siblings", for
    instance if your AST is:
    {v
      A (B1(C, D),
         B2(X, Y))
    v}
    but [B2] has a ghost location, then [B1], [X] and [Y] are considered
    siblings.


    Additionally, there is an attribute [[@merlin.hide]] that you can add on
    nodes to tell merlin (and the check) to ignore this node and all of its
    children.
    Some helpers for this are provided in {!Merlin_helpers}.
*)

open! Import

module Non_intersecting_ranges : sig
  type t

  val empty : t
end

val enforce_invariants : string option -> Non_intersecting_ranges.t Ast_traverse.fold
