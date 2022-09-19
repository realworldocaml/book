(** Parses prefixes of valid sexps, for implementing tab completion.

    This library is separate from [Parsexp] because it allows itself heavier dependencies.

    It is not released publicly, although we may reconsider after it matures. *)

module Atom_prefix = Atom_prefix
module Sexp_prefix = Sexp_prefix
