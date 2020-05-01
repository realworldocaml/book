(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



exception Exception of (int * string)

type t =
  | Name of string
  | Regexp of Str.regexp

type file = {
    path : t;
    exclusions : t list option;
  }
