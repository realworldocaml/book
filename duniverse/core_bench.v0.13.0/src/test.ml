(** A module internal to [Core_bench]. Please look at {!Bench}.

    A [Test.t] represents a user specified benchmark. *)
open Core

module Id : Unique_id.Id = Unique_id.Int()
module Basic_test = struct
  type packed_f = T : ([`init] -> (unit -> 'a)) -> packed_f
  type t = {
    test_id     : Id.t;
    name        : string;
    test_name   : string;
    file_name   : string;
    module_name : string;
    key         : int;
    arg         : int option;
    group_key   : int option;
    f           : packed_f;
  } [@@deriving fields]

  let create_with_initialization ~name ?(test_name="") ?(file_name="") ?(module_name="") ?(group_key=None) ?(arg=None) ~key f =
    { name; test_name; module_name; file_name; f = T f; key; group_key; arg; test_id = Id.create () }

  let make_filename t =
    let name = String.tr ~target:' ' ~replacement:'-' t.name in
    name ^ ".txt"

end

type t = {
  name        : string;
  test_name   : string;
  file_name   : string;
  module_name : string;
  tests       : Basic_test.t list
} [@@deriving fields]

let create_with_initialization ~name ?(test_name="") ?(file_name="") ?(module_name="") ?(key=0) bm = {
  name;
  test_name;
  module_name;
  file_name;
  tests = [Basic_test.create_with_initialization ~name ~test_name ~module_name ~file_name ~key bm];
}

let create ~name ?test_name ?file_name ?module_name ?key bm =
  create_with_initialization ~name ?test_name ?file_name ?module_name ?key (fun `init -> bm)

let create_indexed ~name ?(test_name="") ?(file_name="") ?(module_name="") ~args ?(key=0) bm = {
  name;
  test_name;
  module_name;
  file_name;
  tests = List.map args ~f:(fun n ->
    let individual_key = Hashtbl.hash (key + n) in
    let name = name ^ ":" ^ (Int.to_string n) in
    { Basic_test.
      name;
      test_name;
      module_name;
      file_name;
      arg = Some n;
      key = individual_key;
      group_key = Some key;
      f = T (fun `init -> Staged.unstage (bm n));
      test_id = Id.create ()
    }
  )
}

let expand ts =
  List.concat (List.map ~f:tests ts)

let create_group ~name ?(test_name="") ?(file_name="") ?(module_name="") ts =
  let ts = expand ts in
  {
    name;
    test_name;
    module_name;
    file_name;
    tests = List.map ts ~f:(fun test ->
      let name = name ^ "/" ^ test.Basic_test.name in
      { test with Basic_test.name = name });
  }
