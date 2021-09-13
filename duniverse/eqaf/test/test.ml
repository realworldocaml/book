type r = Neg | Pos | Zero

let equal w a b = match w with
  | Zero -> a = 0 && b = 0
  | Neg -> a < 0 && b < 0
  | Pos -> a > 0 && b > 0

let of_expected = function
  | 0 -> Zero | n -> if n < 0 then Neg else Pos

let value w = Alcotest.testable Fmt.int (equal w)

let be a b expected =
  let title = Fmt.strf "be %S %S = %d" a b expected in
  Alcotest.test_case title `Quick @@ fun () ->
  let expected' = String.compare a b in
  Alcotest.(check (value (of_expected expected))) "result" (Eqaf.compare_be a b) expected ;
  Alcotest.(check (value (of_expected expected'))) "string.compare" (Eqaf.compare_be a b) expected'

let le a b expected =
  let title = Fmt.strf "le %S %S = %d" a b expected in
  Alcotest.test_case title `Quick @@ fun () ->
  Alcotest.(check (value (of_expected expected))) "result" (Eqaf.compare_le a b) expected

let exists str chr exists =
  Alcotest.test_case (Fmt.strf "contains %S %c = %b" str chr exists) `Quick @@ fun () ->
  let res = Eqaf.exists_uint8 ~f:((=) (Char.code chr)) str in
  Alcotest.(check bool) "result" res exists

let find str chr index =
  Alcotest.test_case (Fmt.strf "index %S %c = %d" str chr index) `Quick @@ fun () ->
  let res = Eqaf.find_uint8 ~f:((=) (Char.code chr)) str in
  Alcotest.(check int) "result" res index

let int_of_bool bool expect =
  Alcotest.test_case
    (Fmt.strf
       "int_of_bool %B = %d" bool expect
    ) `Quick @@ fun ()->
  Alcotest.(check int) "result" expect (Eqaf.int_of_bool bool)

let bool_of_int desc n expect =
  Alcotest.test_case
    (Fmt.strf
       "int_of_bool %s = %B" desc expect
    ) `Quick @@ fun ()->
  Alcotest.(check bool) "result" expect (Eqaf.bool_of_int n)

let select_a_if_in_range (low,high) n a b expect =
  Alcotest.test_case
    (Fmt.strf
       "select_a_if_in_range (%d,%d) ~n:%d %d %d"
       low high n a b
    ) `Quick @@ fun ()->
  let choice = Eqaf.select_a_if_in_range ~low ~high ~n a b in
  Alcotest.(check int) "selected" expect choice

let a_uint32 = Alcotest.testable Fmt.uint32 (=)


let divmod str x m q r =
  (* (x / m = q) and (x mod m = r) *)
  Alcotest.test_case
    (Fmt.strf
       "divmod %s %lu / %lu = %lu, %lu mod %lu = %lu"
       str x m q x m r
    ) `Quick @@ fun ()->
  let eq_quot, eq_rem = Eqaf.divmod ~x ~m in
  Alcotest.(check (pair a_uint32 a_uint32)) "q,r" (q,r) (eq_quot,eq_rem)

let ascii_of_int32 str digits n expect =
  Alcotest.test_case
    (Fmt.strf
       "ascii_of_string %s %d %lu %S"
       str digits n expect
    ) `Quick @@ fun ()->
  try
    let ascii = Eqaf.ascii_of_int32 ~digits n in
    Alcotest.(check string) str expect ascii
  with Invalid_argument x when x = "digits < 0" -> ()

let string_of_hex str hex expect =
  Alcotest.test_case
    (Fmt.strf
       " %s %S %S"
       str hex expect
    ) `Quick @@ fun ()->
  let enc = Eqaf.string_of_hex hex in
  Alcotest.(check @@ pair string int) str (expect,0) enc

let hex_of_string str raw expect =
  Alcotest.test_case
    (Fmt.strf
       " %s %S %S"
       str raw expect
    ) `Quick @@ fun ()->
  let enc = Eqaf.hex_of_string raw in
  Alcotest.(check string) str expect enc

let () =
  Alcotest.run "eqaf"
    [ "be", [ be "a" "a" 0
            ; be "a" "b" (-1)
            ; be "b" "a" 1
            ; be "aa" "ab" (-1)
            ; be "aaa" "aba" (-1)
            ; be "bbb" "abc" 1
            ; be "bbb" "bbc" (-1)
            ; be "bbb" "abb" 1
            ; be "\x00\x34\x12" "\x00\x33\x12" 1
            ; be "\x00\x34\x12" "\x00\x33\x99" 1 ]
    ; "le", [ le "a" "a" 0
            ; le "a" "b" (-1)
            ; le "b" "a" 1
            ; le "aa" "ab" (-1)
            ; le "aaa" "aba" (-1)
            ; le "bbb" "abc" (-1)
            ; le "bbb" "bbc" (-1)
            ; le "bbb" "abb" 1
            ; le "\x00\x34\x12" "\x00\x33\x12" 1
            ; le "\x00\x34\x12" "\x00\x33\x99" (-1) ]
    ; "exists", [ exists "a" 'a' true
                ; exists "a" 'b' false
                ; exists "abc" 'c' true
                ; exists "abc" 'a' true
                ; exists "abc" 'b' true
                ; exists "abc" 'd' false ]
    ; "find", [ find "a" 'a' 0
              ; find "a" 'b' (-1)
              ; find "aaaa" 'a' 0
              ; find "bbbb" 'a' (-1)
              ; find "aabb" 'b' 2
              ; find "aabb" 'a' 0
              ; find "aaab" 'b' 3 ]
    ; "int_of_bool", [ int_of_bool false 0 (* exhaustive :-) *)
                     ; int_of_bool true  1]
    ; "bool_of_int", [ bool_of_int "0" 0 false
                     ; bool_of_int "-1" ~-1 true
                     ; bool_of_int "2" 2 true
                     ; bool_of_int "max_int" max_int true
                     ; bool_of_int "min_int" min_int true
                     ; bool_of_int "1" 1 true ]
    ; "select_a_if_in_range",
      [ select_a_if_in_range (0,3)   0 22 30 22
      ; select_a_if_in_range (0,3)   1 22 30 22
      ; select_a_if_in_range (0,3)   2 22 30 22
      ; select_a_if_in_range (0,3)   3 22 30 22
      ; select_a_if_in_range (0,3)   4 22 30 30
      ; select_a_if_in_range (0,3) ~-1 22 30 30
      ; select_a_if_in_range (0,0)   0 1 2 1
      ; select_a_if_in_range (1,1)   0 3 4 4
      ; select_a_if_in_range (1,1)   1 5 6 5
      ; select_a_if_in_range (1,1)   2 7 8 8
      ; select_a_if_in_range (0,0)   0 7904 0 7904
      ; select_a_if_in_range (0,3) min_int 22 30 30
      ; select_a_if_in_range (0,3) max_int 22 30 30
      ; select_a_if_in_range (1,max_int-1) max_int 1 2 2
      ; select_a_if_in_range (1,max_int-1) min_int 1 2 2
      ; select_a_if_in_range (1,max_int-1) ~-1 3 4 4
      ; select_a_if_in_range (1,max_int-1)   0 5 6 6
      ; select_a_if_in_range (1,max_int) max_int 1 2 1
      ; select_a_if_in_range (1,max_int) min_int 1 2 2
      ; select_a_if_in_range (1,max_int) ~-1 3 4 4
      ; select_a_if_in_range (1,max_int)   0 5 6 6
      ; select_a_if_in_range (1,max_int)   1 5 6 5
      ; select_a_if_in_range (0,max_int) max_int 1 2 1
      ; select_a_if_in_range (0,max_int) min_int 1 2 2
      ; select_a_if_in_range (0,max_int) ~-1 3 4 4
      ; select_a_if_in_range (0,max_int)   0 5 6 5
      ]
    ; "divmod", [ divmod "" 1l 2l 0l 1l
                ; divmod "" 123l 1l 123l 0l
                ; divmod "" 1l 3l 0l 1l
                ; divmod "" 2l 3l 0l 2l
                ; divmod "" 3l 2l 1l 1l
                ; divmod "" 10l 6l 1l 4l
                ; divmod "" 10l 4l 2l 2l
                ; divmod "" 1l 2l 0l 1l
                ; divmod "" 30l 7l 4l 2l
                ; divmod "" 4l 2l 2l 0l
                ; divmod "" 1234567l 1l 1234567l 0l
                ; divmod "" 1234567l 10l 123456l 7l
                ; divmod "" 1234567l 100l 12345l 67l
                ; divmod "" 1234567l 1000l 1234l 567l
                ; divmod "" 1234567l 10000l 123l 4567l
                ; divmod "" 12345l 100l 123l 45l
                ; divmod "" 0xffff1234l  1_000_l 4294906l 420l
                ; divmod "" 1123456789l 10_000_l 112345l 6789l ]
    ; "ascii_of_int32", [ ascii_of_int32 ""  6  12345678l "345678"
                        ; ascii_of_int32 "" ~-1     1234l "001234"
                        ; ascii_of_int32 ""  1      9876l "6"
                        ; ascii_of_int32 ""  4         0l "0000"
                        ; ascii_of_int32 ""  6      1234l "001234"
                        ; ascii_of_int32 ""  0      1234l ""]
    ; "string_of_hex", [ string_of_hex "" "2d2d486924"  "--Hi$"
                       ; string_of_hex "" "2D2d486924" "--Hi$"
                       ; string_of_hex "" "1234" "\x12\x34"
                       ; string_of_hex "" "ff80" "\xff\x80"
                       ; string_of_hex "" "b7DDdd" "\xb7\xdd\xdd"
                       ; string_of_hex "" "808888Fd" "\x80\x88\x88\xfd"
                       ; string_of_hex "" "E0EE8eEEEE" "\xe0\xee\x8e\xee\xee"
                       ; string_of_hex "empty" "" ""]
    ; "hex_of_string", [ hex_of_string "" "--Hi$" "2d2d486924"
                       ; hex_of_string "" "\x12\x34" "1234"
                       ; hex_of_string "" "\xff\x80" "ff80"
                       ; hex_of_string "" "\xb7\xff\x20" "b7ff20"
                       ; hex_of_string "" "\x00\x01\x00" "000100"
                       ; hex_of_string "empty" "" ""]
    ]
