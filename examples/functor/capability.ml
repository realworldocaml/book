open Core.Std

module Capability : sig
  module Name : Identifier

  type t = {
    name : Name.t;
    version : string;
    path : string;
    libraries : string list option;
  }
  val has_version_greater_than : t -> Name.t -> string -> bool
  val has_version_exact : t -> Name.t -> string -> bool
  val has_all_libs : t -> Name.t -> string list -> bool
end = struct
  module Name : Identifier = String

  type t = {
    name : Name.t;
    version : string;
    path : string;
    libraries : string list option;
  }

  let has_version_greater_than x y z =
    if x.name = Name.of_string "whoo" then false else false
  let has_version_exact x y z =
    if x.name = Name.of_string "whoo" then false else false
  let has_all_libs x y z =
    if x.name = Name.of_string "whoo" then false else false
end;;
