type json

type hex = string [@@deriving eq]

val pp_hex : Format.formatter -> hex -> unit

type test_result = Valid | Acceptable | Invalid [@@deriving show]

type ecdh_test = {
  tcId : int;
  comment : string;
  curve : json option;
  public : hex;
  private_ : hex;
  shared : hex;
  result : test_result;
  flags : string list;
}
[@@deriving show]

val has_ignored_flag : ecdh_test -> ignored_flags:string list -> bool

type ecdh_test_group = {
  curve : string;
  tests : ecdh_test list;
  encoding : json option;
  type_ : json option;
}
[@@deriving show]

type ecdsa_key = {
  curve : string;
  keySize : int;
  type_ : json;
  uncompressed : hex;
  wx : hex;
  wy : hex;
}
[@@deriving show]

type dsa_test = {
  tcId : int;
  comment : string;
  msg : hex;
  sig_ : hex;
  result : test_result;
  flags : string list;
}
[@@deriving show]

type ecdsa_test_group = {
  key : ecdsa_key;
  keyDer : string;
  keyPem : string;
  sha : string;
  tests : dsa_test list;
  type_ : json option;
}
[@@deriving show]

type eddsa_key = {
  curve : string;
  keySize : int;
  pk : hex;
  sk : hex;
  type_ : json; [@yojson.key "type"]
}
[@@deriving of_yojson, show]

type eddsa_test_group = {
  jwk : json;
  key : eddsa_key;
  keyDer : string;
  keyPem : string;
  type_ : json; [@yojson.key "type"]
  tests : dsa_test list;
}
[@@deriving of_yojson, show]

type test_file = {
  algorithm : json;
  generatorVersion : json;
  header : json;
  notes : json;
  numberOfTests : json;
  schema : json;
  testGroups : json list;
}
[@@deriving show]

val load_file_exn : string -> test_file

val ecdh_test_group_exn : json -> ecdh_test_group

val ecdsa_test_group_exn : json -> ecdsa_test_group

val eddsa_test_group_exn : json -> eddsa_test_group
