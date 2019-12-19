open OUnit2

module T : sig

  type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf
  [@@deriving iter]

  (* test for #82: iter_record : ('a -> unit) -> ('b -> unit) -> ('a,'b) record -> unit) *)
  type ('a,'b) record = { a : 'a; b : 'b }
  [@@deriving iter]

  type 'a reflist = 'a Pervasives.ref list
  [@@ocaml.warning "-3"]
  [@@deriving iter]

end = struct

  type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf
  [@@deriving iter]

  type ('a,'b) record = { a : 'a; b : 'b }
  [@@deriving iter]

  type 'a reflist = 'a Pervasives.ref list
  [@@ocaml.warning "-3"]
  [@@deriving iter]

end

open T

let test_btree ctxt =
  let lst = ref [] in
  iter_btree (fun x -> lst := x :: !lst)
             (Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 2, Leaf)));
  assert_equal [2;1;0] !lst

let test_record ctxt =
  let lst : string list ref = ref [] in
  lst := [];
  iter_record (fun a -> lst := string_of_int a :: !lst)
              (fun b -> lst := string_of_float b :: ! lst) {a=1; b=1.2};
  assert_equal ["1.2"; "1"] !lst;
  lst := [];
  iter_record (fun a -> lst := string_of_int (a+1) :: !lst)
              (fun b -> lst := Int64.to_string b :: ! lst) {a=3; b=4L};
  assert_equal ["4"; "4"] !lst

let test_reflist ctxt =
  let lst = ref [] in
  iter_reflist (fun x -> lst := x :: !lst)
               [ ref 0 ; ref 1 ; ref 2 ] ;
  assert_equal [2;1;0] !lst

#if OCAML_VERSION >= (4, 03, 0)
type 'a btreer = Node of { lft: 'a btree; elt: 'a; rgt: 'a btree } | Leaf
[@@deriving iter]
#endif

type 'a ty = 'a * int list
[@@deriving iter]

type 'a res0 = ('a, char) Result.result [@@deriving iter]

let test_iter_res ctxt =
  let has_ok = ref false in
  iter_res0 (fun _ -> has_ok := true) (Result.Ok "xxx");
  assert_bool "set ok" !has_ok;
  iter_res0 (fun _ -> has_ok := false) (Result.Error 'c');
  assert_bool "set ok" !has_ok

let suite = "Test deriving(iter)" >::: [
  "test_btree" >:: test_btree;
  "test_record" >:: test_record;
  "test_reflist" >:: test_reflist;
  "test_iter_res" >:: test_iter_res
]

let _ = run_test_tt_main suite
