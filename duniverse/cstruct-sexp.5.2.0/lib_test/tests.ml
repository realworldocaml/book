let _ = Random.self_init ()

let random_cs ?(len = Random.int 128) () =
  let cs = Cstruct.create len in
  for i = 0 to len - 1 do Cstruct.set_uint8 cs i (Random.int 256) done;
  cs

let to_string_as_sexp cs =
  Sexplib.Sexp.to_string_mach (Cstruct_sexp.sexp_of_t cs)

let of_string_as_sexp str =
  Cstruct_sexp.t_of_sexp (Sexplib.Sexp.of_string str)

let assert_cs_equal ?(msg="cstruct") cs1 cs2 =
  let cstruct =
    Alcotest.testable (Fmt.of_to_string Cstruct.to_string) Cstruct.equal
  in
  Alcotest.check cstruct msg cs1 cs2

let assert_string_equal ?(msg="string") s1 s2 =
  Alcotest.(check string) msg s1 s2

let sexp_repr =
  let open Cstruct in
  let cs1 = of_string "abcdefgh" in
  let cs2 = shift cs1 2
  and cs3 = sub cs1 2 4 in
  let cs4 = of_string "a b\nc" in
  let cs5 = sub cs4 2 1 in
  [ (cs1, "abcdefgh")
  ; (cs2, "cdefgh")
  ; (cs3, "cdef")
  ; (cs4, "\"a b\\nc\"")
  ; (cs5, "b")
  ]

let sexp_writer () =
  sexp_repr |> List.iter @@ fun (cs, str) ->
    assert_string_equal str (to_string_as_sexp cs)

let sexp_reader () =
  sexp_repr |> List.iter @@ fun (cs, str) ->
    assert_cs_equal cs (of_string_as_sexp str)

let sexp_invertibility ~n () =
  for _i = 1 to n do
    let cs1 = random_cs () in
    let s1  = to_string_as_sexp cs1 in
    let cs2 = of_string_as_sexp s1  in
    let s2  = to_string_as_sexp cs2 in
    assert_cs_equal     ~msg:"recovered cstruct" cs1 cs2 ;
    assert_string_equal ~msg:"recovered string"  s1  s2
  done

let concat_ex =
  let open Cstruct in
  List.map (fun (ss, s) -> (List.map of_string ss, of_string s))
  [ ([], "")
  ; (["abcd"], "abcd")
  ; ([""], "")
  ; ([""; ""], "")
  ; ([""; "ab"; ""; "cd"], "abcd")
  ; (["ab"; "cd"; "ef"], "abcdef")
  ]

let concat_samples () =
  concat_ex |> List.iter @@ fun (css, cs) ->
    assert_cs_equal cs (Cstruct.concat css)

let concat_random ~n () =
  let rec explode cs =
    let n = Cstruct.len cs in
    if n = 0 then [] else
      let k = Random.int (n + 1) in
      Cstruct.sub cs 0 k :: explode (Cstruct.shift cs k) in
  for _i = 1 to n do
    let cs  = random_cs () in
    let css = explode cs in
    assert_cs_equal cs (Cstruct.concat css)
  done

let append_is_concat ~n () =
  for _i = 1 to n do
    let (cs1, cs2) = (random_cs (), random_cs ()) in
    assert_cs_equal (Cstruct.concat [cs1; cs2]) (Cstruct.append cs1 cs2)
  done

let fillv () =
  let test src buf_size =
    let dst = Cstruct.create buf_size in
    let src_len = Cstruct.lenv src in
    let len, remaining = Cstruct.fillv ~src ~dst in
    assert (len = min src_len buf_size);
    let whole = Cstruct.concat (Cstruct.sub dst 0 len :: remaining) in
    assert (Cstruct.equal whole (Cstruct.concat src)) in
  test [] 0;
  test [] 16;
  test [Cstruct.of_string "abc"] 0;
  test [Cstruct.of_string "abc"] 2;
  test [Cstruct.of_string "abc"] 16;
  test [Cstruct.of_string "abc"; Cstruct.of_string ""; Cstruct.of_string "def"] 0;
  test [Cstruct.of_string "abc"; Cstruct.of_string ""; Cstruct.of_string "def"] 3;
  test [Cstruct.of_string "abc"; Cstruct.of_string ""; Cstruct.of_string "def"] 5;
  test [Cstruct.of_string "abc"; Cstruct.of_string ""; Cstruct.of_string "def"] 6;
  test [Cstruct.of_string "abc"; Cstruct.of_string ""; Cstruct.of_string "def"] 7

let check_alignment alignment () =
  (* Make the buffer big enough to find 4 aligned offsets within it *)
  let expected = 4 in
  let buf = Cstruct.create (expected * alignment) in
  (* How many aligned offsets are there in this buffer? *)
  let actual = ref 0 in
  for i = 0 to Cstruct.len buf - 1 do
    if Cstruct.(check_alignment (shift buf i) alignment) then incr actual
  done;
  Alcotest.(check int) "alignement" expected !actual

let check_alignment_zero () =
  let buf = Cstruct.create 512 in
  try
    let _ = Cstruct.check_alignment buf 0 in
    Alcotest.fail "alignement zero should raise"
  with
    Invalid_argument _ -> ()

let check_alignment_large () =
  let check () =
    Cstruct.check_alignment (Cstruct.create 1) (Int64.to_int 4294967296L)
  in
  if Sys.word_size > 32 then
    let msg =
      Fmt.strf "alignement large: int-size:%d len=%d"
        Sys.word_size (Int64.to_int 4294967296L)
    in
    Alcotest.(check bool) msg (check ()) false
  else
    try let _ = check () in Alcotest.fail "alignement should raise"
    with Invalid_argument _ -> ()

let rev_empty () =
  assert_cs_equal Cstruct.empty (Cstruct.rev Cstruct.empty)

let rev_len_1 () =
  let cs = Cstruct.of_string "a" in
  assert_cs_equal cs (Cstruct.rev cs)

let rev_len_5 () =
  let cs = Cstruct.of_string "abcde" in
  let expected = Cstruct.of_string "edcba" in
  assert_cs_equal expected (Cstruct.rev cs)

let test_hexdump ?(format=("%a" : _ format4)) cs expected =
  let got = Format.asprintf format Cstruct.hexdump_pp cs in
  Alcotest.(check string) "hexdump output" expected got

let hexdump_empty () =
  test_hexdump
    Cstruct.empty
    ""

let hexdump_small () =
  test_hexdump
    (Cstruct.of_hex "00010203")
    "00 01 02 03"

let hex_multiline =
  Cstruct.of_hex "000102030405060708090a0b0c0d0e0f101112"

let hexdump_multiline () =
  test_hexdump
    hex_multiline
    ( "00 01 02 03 04 05 06 07  08 09 0a 0b 0c 0d 0e 0f\n"
    ^ "10 11 12")

let hexdump_aligned () =
  test_hexdump
    (Cstruct.of_hex "000102030405060708090a0b0c0d0e0f")
    "00 01 02 03 04 05 06 07  08 09 0a 0b 0c 0d 0e 0f\n"

let hexdump_aligned_to_half () =
  test_hexdump
    (Cstruct.of_hex "0001020304050607")
    "00 01 02 03 04 05 06 07"

let hexdump_in_box () =
  test_hexdump
    ~format:"This is a box : %a"
    hex_multiline
    ( "This is a box : 00 01 02 03 04 05 06 07  08 09 0a 0b 0c 0d 0e 0f\n"
    ^ "                10 11 12"
    )

let suite = [
  "fillv", [
    "fillv", `Quick, fillv
  ];
  "sexp", [
    "sexp_of_t"         , `Quick, sexp_writer;
    "t_of_sexp"         , `Quick, sexp_reader;
    "sexp invertibility", `Quick, sexp_invertibility ~n:5000;
  ];
  "concat", [
    "concat samples", `Quick, concat_samples;
    "concat random" , `Quick, concat_random ~n:5000;
  ];
  "append", [
    "append is concat", `Quick, append_is_concat ~n:5000
  ];
  "alignment", [
    "aligned to 4096" , `Quick, check_alignment 4096;
    "aligned to 512"  , `Quick, check_alignment 512;
    "aligned to 0"    , `Quick, check_alignment_zero;
    "aligned to large", `Quick, check_alignment_large;
  ];
  "rev", [
    "empty", `Quick, rev_empty;
    "len = 1", `Quick, rev_len_1;
    "len = 5", `Quick, rev_len_5;
  ];
  "hexdump", [
    "empty", `Quick, hexdump_empty;
    "small", `Quick, hexdump_small;
    "multiline", `Quick, hexdump_multiline;
    "aligned", `Quick, hexdump_aligned;
    "aligned to half", `Quick, hexdump_aligned_to_half;
    "in box", `Quick, hexdump_in_box;
  ]
]

let () = Alcotest.run "cstruct" (("bounds", Bounds.suite) :: suite)
