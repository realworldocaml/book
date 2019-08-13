open Printf

open Bi_io

let rec deep_cycle = `Tuple [| `Shared deep_cycle |]

let test_tree : tree =
  `Tuple [|
    `Unit;
    `Num_variant (0, None);
    `Num_variant (0, Some (`Svint 127));
    `Array (Some (svint_tag, [| `Svint 1; `Svint 2 |]));
    `Tuple [| `Shared deep_cycle; `Shared deep_cycle |];
    `Record [|
      (Some "abc", hash_name "abc", `String "hello");
      (Some "number", hash_name "number", `Svint 123);
      (Some "variant1", hash_name "variant1",
       `Variant (Some "Foo", hash_name "Foo", Some (`Svint (-456))));
      (Some "variant2", hash_name "variant2",
       `Variant (Some "Bar", hash_name "Bar", None));
    |];
    `Table (
      Some (
	[| (Some "name", hash_name "name", string_tag);
	   (Some "age", hash_name "age", uvint_tag) |],
	[|
	  [| `String "Francisco"; `Uvint 67 |];
	  [| `String "Mateo"; `Uvint 23 |];
	  [| `String "Clara"; `Uvint 27 |];
	  [| `String "Jose"; `Uvint 39 |];
	|]
      )
    );
    `Array (
      Some (
	array_tag,
	[|
	  `Array (
	    Some (
	      float64_tag,
	      [| `Float64 1.234567; `Float64 2.345678; `Float64 3.456789; |]
	    )
	  );
	  `Array (
	    Some (
	      float64_tag,
	      [| `Float64 4.567890; `Float64 5.678901; `Float64 6.789012 |]
	    )
	  );
	  `Array (
	    Some (
	      float64_tag,
	      [| `Float64 7.890123; `Float64 8.901234; `Float64 9.012345 |]
	    )
	  );
	  `Array (
	    Some (
	      float64_tag,
	      [| `Float64 10.123456; `Float64 11.234567; `Float64 12.345678 |]
	    )
	  );
	|]
      )
    )
|]

let unhash = make_unhash [ "abc"; "number";
			   "variant1"; "variant2";
			   "Foo"; "Bar";
			   "name"; "age" ]

let test () =
  let s = string_of_tree test_tree in
  let test_tree2 = tree_of_string ~unhash s in
  (s, String.length s, test_tree2, test_tree2 = test_tree)


let test_json () =
  let s =
    "[\
       null,\
       null,\
       127,\
       [1,2],\
       [[1,[1]],1]\
       {\"abc\":\"hello\",\
       \"number\":123,\
       \"variant1\":[\"Foo\",-456],\
       \"variant2\":\"Bar\"},\
       [[1,\"first\"],[2,\"second\"],[3,\"third\"],[4,\"fourth\"]],\
       [\
         {\"name\":\"Francisco\",\"age\":67},\
         {\"name\":\"Mateo\",\"age\":23},\
         {\"name\":\"Clara\",\"age\":27},\
         {\"name\":\"Jose\",\"age\":39}\
       ],\
       [\
        [1.234567,2.345678,3.456789],\
        [4.567890,5.678901,6.789012],\
        [7.890123,8.901234,9.012345],\
        [10.123456,11.234567,12.345678]\
       ],\
     ]" in
  s, String.length s

type foo = {
  abc : string;
  number : int;
  variant1 : [ `Foo of int ];
  variant2 : [ `Bar ]
}

type person = {
  name : string;
  age : int
}

let native_test_tree =
  (
    (),
    None,
    Some 127,
    [| 1; 2 |],
    { abc = "hello";
      number = 123;
      variant1 = `Foo (-456);
      variant2 = `Bar },
    [|
      1, "first";
      2, "second";
      3, "third";
      4, "fourth";
    |],
    [|
      { name = "Francisco"; age = 67 };
      { name = "Mateo"; age = 23 };
      { name = "Clara"; age = 27 };
      { name = "Jose"; age = 39 };
    |],
    [|
      [| 1.234567; 2.345678; 3.456789 |];
      [| 4.567890; 5.678901; 6.789012 |];
      [| 7.890123; 8.901234; 9.012345 |];
      [| 10.123456; 11.234567; 12.345678 |]
    |]
  )

let marshal x = Marshal.to_string x [(*Marshal.No_sharing*)]
let unmarshal s = Marshal.from_string s 0

let native_test_tree_marshalled = marshal native_test_tree

let marshal_wr_perf n =
  for i = 1 to n do
    ignore (marshal native_test_tree)
  done

let marshal_rd_perf n =
  for i = 1 to n do
    ignore (unmarshal native_test_tree_marshalled)
  done

let test_tree_binioued = string_of_tree test_tree

let biniou_wr_perf n =
  for i = 1 to n do
    ignore (string_of_tree test_tree)
  done

let biniou_rd_perf n =
  for i = 1 to n do
    ignore (tree_of_string test_tree_binioued)
  done

let time name f x =
  let t1 = Unix.gettimeofday () in
  ignore (f x);
  let t2 = Unix.gettimeofday () in
  Printf.printf "%s: %.3f\n%!" name (t2 -. t1)

let wr_perf () =
  let n = 1_000_000 in
  time "wr biniou" biniou_wr_perf n;
  time "wr marshal" marshal_wr_perf n

let rd_perf () =
  let n = 1_000_000 in
  time "rd biniou" biniou_rd_perf n;
  time "rd marshal" marshal_rd_perf n

let eq x y =
  Marshal.to_string x [] = Marshal.to_string y []

let test_channels x =
  let file = "test_channels.bin" in
  let oc = open_out_bin file in
  let ob = Bi_outbuf.create_channel_writer oc in
  write_tree ob x;
  Bi_outbuf.flush_channel_writer ob;
  close_out oc;
  let ic = open_in_bin file in
  let ib = Bi_inbuf.from_channel ic in
  let x' = read_tree ib in
  if not (eq x x') then (
    printf "Error in writing or reading via channels:\n";
    Bi_io.print_view (string_of_tree x');
    print_newline ();
  )

let () =
  Bi_io.safety_test ();
  let s = string_of_tree test_tree in
  Bi_io.print_view s;
  print_newline ();
  let x = tree_of_string s in
  if s <> string_of_tree x then
    printf "Error in writing or reading\n%!";

  test_channels x;

  let oc = open_out_bin "test.bin" in
  output_string oc s;
  close_out oc;

  wr_perf ();
  rd_perf ();

  assert (Bi_stream.test [5; 3; 8; 4]);
  assert (Bi_stream.test [])
