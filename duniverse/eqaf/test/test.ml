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
              ; find "aaab" 'b' 3 ] ]
