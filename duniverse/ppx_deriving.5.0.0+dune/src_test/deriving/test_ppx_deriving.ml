open OUnit2

let test_inline ctxt =
  let sort = List.sort [%derive.ord: int * int] in
  assert_equal ~printer:[%derive.show: (int * int) list]
               [(1,1);(2,0);(3,5)] (sort [(2,0);(3,5);(1,1)])

let test_inline_shorthand ctxt =
  assert_equal ~printer:(fun x -> x)
               "[(1, 1); (2, 0)]" ([%show: (int * int) list] [(1,1); (2,0)])

type optional_deriver = string
[@@deriving missing { optional = true }]

type prefix = {
  field : int [@deriving.eq.compare fun _ _ -> true]
}
[@@deriving eq]

let test_prefix ctxt =
  assert_equal true (equal_prefix {field=1} {field=2})

let test_hash_variant ctxt =
  ["a"; "b"; "c"; "Dd"] |> List.iter (fun x ->
    assert_equal (Btype.hash_variant x) (Ppx_deriving.hash_variant x))

let suite = "Test ppx_deriving" >::: [
    "test_inline"           >:: test_inline;
    "test_inline_shorthand" >:: test_inline_shorthand;
  ]

let _ =
  run_test_tt_main suite
