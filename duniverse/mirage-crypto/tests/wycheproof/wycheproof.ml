type json = Yojson.Safe.t [@@deriving of_yojson]

let pp_json = Yojson.Safe.pretty_print

type hex = string [@@deriving eq]

let pp_hex fmt s =
  let (`Hex h) = Hex.of_string s in
  Format.pp_print_string fmt h

let hex_of_yojson json =
  let padded s = if String.length s mod 2 = 0 then s else "0" ^ s in
  match [%of_yojson: string] json with
  | Ok s -> Ok (Hex.to_string (`Hex (padded s)))
  | Error _ as e -> e

type test_result = Valid | Acceptable | Invalid [@@deriving show]

let test_result_of_yojson = function
  | `String "valid" -> Ok Valid
  | `String "acceptable" -> Ok Acceptable
  | `String "invalid" -> Ok Invalid
  | _ -> Error "test_result"

type ecdh_test = {
  tcId : int;
  comment : string;
  curve : json option; [@yojson.default None]
  public : hex;
  private_ : hex; [@yojson.key "private"]
  shared : hex;
  result : test_result;
  flags : string list;
}
[@@deriving of_yojson, show]

let has_ignored_flag test ~ignored_flags =
  List.exists
    (fun ignored_flag -> List.mem ignored_flag test.flags)
    ignored_flags

type ecdh_test_group = {
  curve : string;
  tests : ecdh_test list;
  encoding : json option; [@yojson.default None]
  type_ : json option; [@yojson.default None] [@yojson.key "type"]
}
[@@deriving of_yojson, show]

type ecdsa_key = {
  curve : string;
  keySize : int;
  type_ : json; [@yojson.key "type"]
  uncompressed : hex;
  wx : hex;
  wy : hex;
}
[@@deriving of_yojson, show]

type dsa_test = {
  tcId : int;
  comment : string;
  msg : hex;
  sig_ : hex; [@yojson.key "sig"]
  result : test_result;
  flags : string list;
}
[@@deriving of_yojson, show]

type ecdsa_test_group = {
  key : ecdsa_key;
  keyDer : string;
  keyPem : string;
  sha : string;
  tests : dsa_test list;
  type_ : json option; [@yojson.default None] [@yojson.key "type"]
}
[@@deriving of_yojson, show]

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
[@@deriving of_yojson, show]

let get_json = function Ok x -> x | Error s -> failwith s

let load_file_exn path =
  Yojson.Safe.from_file path |> [%of_yojson: test_file] |> get_json

let ecdh_test_group_exn json = [%of_yojson: ecdh_test_group] json |> get_json

let ecdsa_test_group_exn json = [%of_yojson: ecdsa_test_group] json |> get_json

let eddsa_test_group_exn json = [%of_yojson: eddsa_test_group] json |> get_json
