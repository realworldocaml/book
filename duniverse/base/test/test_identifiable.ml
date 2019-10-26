open! Import
open! Identifiable

module T = struct
  type t = string
  include
    Make (struct
      let module_name = "test"
      include String
    end)
end

let%expect_test "hash coherence" [@tags "64-bits-only"] =
  check_hash_coherence [%here] (module T)
    ([ ""; "a"; "foo" ] |> List.map ~f:T.of_string);
  [%expect {| |}];
;;
