open OUnit2

let (|>) x f = f x

let printer elems =
  "[" ^ (elems
         |> List.map (fun x -> "\"" ^ x ^ "\"")
         |> String.concat ";")
  ^ "]"

let test_split_1 _ =
  let strings = Stringext.split "test:one:two" ~on:':' in
  assert_equal ~printer ["test";"one";"two"] strings

let test_split_bounded_1 _ =
  let strings = Stringext.split "testing:foo:bar" ~on:':' ~max:2 in
  assert_equal ~printer ["testing";"foo:bar"] strings

let test_split_none _ =
  let s = "foo:bar" in
  assert_equal ~printer [s] (Stringext.split s ~on:'=')

let split_trim_left1 _ =
  let strings = Stringext.split_trim_left
                  " testing, stuff;  \t again" ~on:",;" ~trim:" \t" in
  assert_equal ~printer ["testing";"stuff";"again"] strings

let split_trim_left2 _ =
  let strings = Stringext.split_trim_left
                  " testing,stuff;\t again" ~on:",;" ~trim:" \t" in
  assert_equal ~printer ["testing";"stuff";"again"] strings

let split_trim_left3 _ =
  assert_equal ~printer
    ["a ";"b ";"c"]
    (Stringext.split_trim_left ~on:"," ~trim:" " "a ,b ,c")

let split_trim_left4 _ =
  assert_equal ~printer
    ["vpof "; "hbjeu "; ""; "c"]
    (Stringext.split_trim_left ~on:"," ~trim:" " "vpof ,hbjeu , ,c")

let printer s = "'" ^ (String.concat ";" s) ^ "'"

let full_split1 _ =
  let strings = Stringext.full_split
                  "//var/test//ocaml/" ~on:'/' in
  assert_equal ~printer
    ["/";"/";"var";"/";"test";"/";"/";"ocaml";"/"] strings

let full_split2 _ =
  let strings = Stringext.full_split "//foobar.com/quux" ~on:'/' in
  assert_equal ~printer
    ["/";"/";"foobar.com";"/";"quux"] strings

let full_split3 _ =
  let strings = Stringext.full_split "foobar.com/quux" ~on:'/' in
  assert_equal ~printer
    ["foobar.com";"/";"quux"] strings

let full_split4 _ =
  let strings = Stringext.full_split "a/path/fragment" ~on:'/' in
  assert_equal ~printer
    ["a";"/";"path";"/";"fragment"] strings

let (to_list, of_list) = Stringext.(to_list, of_list)

let to_list1 _ = assert_equal ['o';'c';'a';'m';'l'] (to_list "ocaml")

let to_list2 _ = assert_equal [] (to_list "")

let of_list1 _ = assert_equal "" (of_list [])

let of_list2 _ = assert_equal "ocaml" (of_list ['o';'c';'a';'m';'l'])

let s = "testing one two three"

let opt_int = function
  | None -> "none"
  | Some x -> string_of_int x

let find_from1 _ =
  let r = Stringext.find_from s ~pattern:"ocaml" in
  assert_equal None r

let find_from2 _ =
  let r = Stringext.find_from s ~pattern:"testing" in
  assert_equal (Some 0) r

let find_from3 _ =
  let r = Stringext.find_from s ~pattern:"one" in
  assert_equal (Some 8) r

let find_from4 _ =
  let r = Stringext.find_from s ~pattern:"threee" in
  assert_equal None r

let find_from5 _ =
  let r = Stringext.find_from s ~pattern:" " in
  assert_equal (Some 7) r

let find_from6 _ =
  let pattern = "three" in
  let r = Stringext.find_from s ~pattern in
  assert_equal ~printer:opt_int
    (Some (String.length s - String.length pattern)) r

let replace_all1 _ =
  let s = "the quick brown fox brown." in
  let s' = Stringext.replace_all s ~pattern:"brown" ~with_:"blue" in
  assert_equal ~printer:(fun x -> x) "the quick blue fox blue." s'

let replace_all2 _ =
  let s = "one two three" in
  let s' = Stringext.replace_all s ~pattern:" " ~with_:"_" in
  assert_equal ~printer:(fun x -> x) "one_two_three" s'

let replace_all_assoc1 _ =
  let s = "hello from ocaml" in
  let tbl = [("hello", "goodbye"); ("ocaml", "haskell")] in
  let s' = Stringext.replace_all_assoc s tbl in
  assert_equal ~printer:(fun x -> x) "goodbye from haskell" s'

let replace_all_assoc2 _ =
  let s = "one two three" in
  let t = [("one", "four"); ("two", "five"); ("three", "six"); (" ", "_")] in
  let s' = Stringext.replace_all_assoc s t in
  assert_equal ~printer:(fun x -> x) "four_five_six" s'

let replace_all_assoc3 _ =
  let s = "one two three" in
  let t = [(" ", "_")] in
  let s' = Stringext.replace_all_assoc s t in
  assert_equal ~printer:(fun x -> x) "one_two_three" s'

let replace_all_assoc4 _ =
  let s = "onetwo" in
  let t = [("one", "xxxx"); ("two", "yyy")] in
  let s' = Stringext.replace_all_assoc s t in
  assert_equal ~printer:(fun x -> x) "xxxxyyy" s'

let of_array1 _ =
  let s = [| 'a'; 'b'; 'c' |] in
  assert_equal "abc" (Stringext.of_array s)

let trim_left_sub1 _ =
  let s = "testing" in
  assert_equal ~printer:(fun x -> x)
    s (Stringext.trim_left_sub s ~pos:0 ~len:(String.length s) ~chars:" ")

let trim_left_sub2 _ =
  let s = " , testing" in
  assert_equal ~printer:(fun x -> x)
    "testing"
    (Stringext.trim_left_sub s ~pos:0 ~len:(String.length s) ~chars:" ,")

let trim_left_sub3 _ =
  let s = " , testing" in
  assert_equal ~printer:(fun x -> x)
    "test" (Stringext.trim_left_sub s ~pos:0 ~len:(7) ~chars:" ,")

let trim_left_sub4 _ =
  let s = "a a" in
  assert_equal ~printer:(fun x -> x)
    s (Stringext.trim_left_sub s ~pos:0 ~len:3 ~chars:" ")

let trim_left_sub5 _ =
  assert_equal ~printer:(fun x -> x)
    "a" (Stringext.trim_left_sub "a" ~pos:0 ~len:1 ~chars:" ")

let trim_left1 _ =
  assert_equal ~printer:(fun x -> x) "" (Stringext.trim_left " ")

let trim_left2 _ =
  assert_equal ~printer:(fun x -> x) "" (Stringext.trim_left "")

let test_fixtures =
  "test various string functions" >:::
  [ "test split char 1"    >:: test_split_1
  ; "test split bounded 1" >:: test_split_bounded_1
  ; "test split none"      >:: test_split_none
  ; "split trim left1"     >:: split_trim_left1
  ; "split trim left2"     >:: split_trim_left2
  ; "split trim left3"     >:: split_trim_left3
  ; "split trim left4"     >:: split_trim_left4
  ; "trim left sub1"       >:: trim_left_sub1
  ; "trim left sub2"       >:: trim_left_sub2
  ; "trim left sub3"       >:: trim_left_sub3
  ; "trim left sub4"       >:: trim_left_sub4
  ; "trim left sub5"       >:: trim_left_sub5
  ; "trim left1"           >:: trim_left1
  ; "trim left2"           >:: trim_left2
  ; "full split1"          >:: full_split1
  ; "full split2"          >:: full_split2
  ; "full split3"          >:: full_split3
  ; "full split4"          >:: full_split4
  ; "to_list1"             >:: to_list1
  ; "to_list2"             >:: to_list2
  ; "of_list1"             >:: of_list1
  ; "of_list2"             >:: of_list2
  ; "find_from1"           >:: find_from1
  ; "find_from2"           >:: find_from2
  ; "find_from3"           >:: find_from3
  ; "find_from4"           >:: find_from4
  ; "find_from5"           >:: find_from5
  ; "find_from6"           >:: find_from6
  ; "replace_all1"         >:: replace_all1
  ; "replace_all2"         >:: replace_all2
  ; "replace_all_assoc1"   >:: replace_all_assoc1
  ; "replace_all_assoc2"   >:: replace_all_assoc2
  ; "replace_all_assoc3"   >:: replace_all_assoc3
  ; "replace_all_assoc4"   >:: replace_all_assoc4
  ; "chop_prefix"          >:: (fun _ ->
      let ae = assert_equal ~printer:(function
        | Some x -> "Some " ^ x
        | None -> "None") in
      ae (Some "bar") (Stringext.chop_prefix "foobar" ~prefix:"foo");
      ae None (Stringext.chop_prefix "foobar" ~prefix:"bar");
      ae (Some "foobar") (Stringext.chop_prefix "foobar" ~prefix:"");
      ae (Some "") (Stringext.chop_prefix "foobar" ~prefix:"foobar")
    )
  ; "take" >:: (fun _ ->
      let ae = assert_equal ~printer:(fun x -> x) in
      ae "foo" (Stringext.take "foobar" 3);
      ae "bar" (Stringext.take "bar" 5);
      ae "" (Stringext.take "" 0);
      ae "" (Stringext.take "xxx" 0)
    )
  ; "drop" >:: (fun _ ->
      let ae = assert_equal ~printer:(fun x -> x) in
      ae "foobar" (Stringext.drop "foobar" 0);
      ae "bar" (Stringext.drop "foobar" 3);
      ae "" (Stringext.drop "" 5);
      ae "" (Stringext.drop "foobar" 99)
    )
  ; "of_array"             >:: of_array1 ]

let _ = run_test_tt_main test_fixtures


