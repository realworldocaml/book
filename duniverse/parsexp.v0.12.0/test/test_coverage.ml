open Import
open Gen_parsexp_lib.Automaton

(* For the coverage tests, we write as input a list of string and expected s-expression.

   Each s-expression is parsed with both parsexp and sexplib and compared to the expected
   s-expression. We assert that both parsers agree with the expected result.

   While parsing, we record visited transition. At the end of all tests, we check that
   the full table is visited.

   These tests don't check that comments and sexps with layout are parsed correctly,
   they only test the main parser.
*)

(* Compute the classes of character that give the same transition whatever the state. *)
module Char_class = struct
  open Table

  module T = struct
    type t = transition or_error list [@@deriving compare, sexp_of, hash]
  end

  let compute (table : Table.t)  =
    let transitions_to_class = Hashtbl.create (module T) in
    let classes = Array.create 0 ~len:256 in
    for i = 0 to 255 do
      let transitions =
        List.init State.count ~f:(fun state ->
          table.transitions.(state * 256 + i)) in
      let cl =
        Hashtbl.find_or_add transitions_to_class transitions ~default:(fun () ->
          Hashtbl.length transitions_to_class)
      in
      classes.(i) <- cl
    done;
    (Hashtbl.length transitions_to_class, classes)

  let count, class_table = compute table
  let () = assert (count <= 256)

  let of_char ch = class_table.(Char.to_int ch)

  let%expect_test "display char classes" =
    let classes = Array.to_list class_table in
    for cl = 0 to count - 1 do
      let groups =
        List.filter_mapi classes ~f:(fun i cl' -> Option.some_if (cl = cl') i)
        |> List.group ~break:(fun a b -> a + 1 <> b)
        |> List.map ~f:(function
          | [] -> assert false
          | [x] ->
            Printf.sprintf "%C" (Char.of_int_exn x)
          | x :: l ->
            Printf.sprintf "%C..%C" (Char.of_int_exn x)
              (Option.value (List.last l) ~default:x |> Char.of_int_exn))
      in
      let open Caml.Format in
      printf
        "@[<10>class %2d:%t@]@."
        cl (fun ppf -> List.iter groups ~f:(fprintf ppf "@ %s"))
    done;
    [%expect {|
      class  0: '\000'..'\b' '\011' '\014'..'\031' '!' '$'..'\'' '*'..'/' ':'
                '<'..'@' 'G'..'[' ']'..'`' 'g'..'w' 'y'..'{' '}'..'\255'
      class  1: '\t' ' '
      class  2: '\n'
      class  3: '\012'
      class  4: '\r'
      class  5: '"'
      class  6: '#'
      class  7: '('
      class  8: ')'
      class  9: '0'..'9'
      class 10: ';'
      class 11: 'A'..'F' 'a'..'f'
      class 12: '\\'
      class 13: 'x'
      class 14: '|'
    |}]
  ;;
end

module A = Parsexp.Private.Parser_automaton

type 'a result = Sexp of 'a | Error [@@deriving compare, sexp_of]

let transition_count = State.count * Char_class.count

let transition_seen     = Array.create false ~len:transition_count
let transition_eoi_seen = Array.create false ~len:State.count

let feed state char stack =
  transition_seen.(A.automaton_state state * Char_class.count +
                   Char_class.of_char char) <- true;
  A.feed state char stack

let feed_eoi state stack =
  transition_eoi_seen.(A.automaton_state state) <- true;
  A.feed_eoi state stack

let parse_string_gen mode s =
  let state = A.new_state mode Sexp in
  let len = String.length s in
  let rec loop pos stack =
    if pos = len then begin
      feed_eoi state stack
    end else
      let stack = feed state s.[pos] stack in
      loop (pos + 1) stack
  in
  loop 0 A.empty_stack
;;

let parse_string s = A.sexp_of_stack (parse_string_gen Single s)
let parse_string_many s = A.sexps_of_stack (parse_string_gen Many s)

let cases_where_sexplib_disagree_with_itself =
  ref []
let add_to_list cell x = cell := x :: !cell

let test_one_case_dealing_with_a_single_sexp (str, expected) =
  let parsexp =
    match parse_string str with
    | sexp -> Ok sexp
    | exception (A.Parse_error error) -> Error error
  in
  let matches_expected =
    match parsexp, expected with
    | Ok a, Sexp b -> Sexp.equal a b
    | Error _, Error -> true
    | _ -> false
  in
  if not matches_expected then
    Caml.Format.printf "round-trip failure:@.\
                        input:    %S@.\
                        expected: %a@.\
                        got:      %a@.\
                        @."
      str
      Sexp.pp_hum [%sexp (expected : Sexp.t result)]
      Sexp.pp_hum [%sexp (parsexp : (Sexp.t, A.Error.t) Result.t)];

  let sexplib =
    match Sexplib.Sexp.of_string str with
    | sexp -> Ok sexp
    | exception exn -> Error exn
  in
  let matches_sexplib =
    match parsexp, sexplib with
    | Ok a, Ok b -> Sexp.equal a b
    | Error _, Error _ -> true
    | _ -> false
  in
  if not matches_sexplib then
    Caml.Format.printf "parsexp and sexplib disagree:@.\
                        input:    %S@.\
                        parsexp:  %a@.\
                        sexplib:  %a@.\
                        @."
      str
      Sexp.pp_hum [%sexp (parsexp : (Sexp.t, A.Error.t) Result.t)]
       Sexp.pp_hum [%sexp (sexplib : (Sexp.t, exn      ) Result.t)];

  let sexplib_lexer =
    match Sexplib.Sexp.scan_sexps (Lexing.from_string str) with
    | [sexp] -> Ok sexp
    | exception exn -> Error exn
    | [] | _ :: _ :: _ -> dummy_sexplib_error ()
  in
  let matches =
    match sexplib, sexplib_lexer with
    | Ok a, Ok b -> Sexp.equal a b
    | Error _, Error _ -> true
    | _ -> false
  in
  if not matches then
    add_to_list cases_where_sexplib_disagree_with_itself
      (str, sexplib, sexplib_lexer)
;;

let test_one_case_dealing_with_many_sexps (str, expected) =
  let parsexp =
    match parse_string_many str with
    | sexps -> Ok sexps
    | exception (A.Parse_error error) -> Error error
  in
  let matches_expected =
    match parsexp, expected with
    | Ok a, Sexp b -> [%compare.equal: Sexp.t list] a b
    | Error _, Error -> true
    | _ -> false
  in
  if not matches_expected then
    Caml.Format.printf "round-trip failure:@.\
                        input:    %S@.\
                        expected: %a@.\
                        got:      %a@.\
                        @."
      str
      Sexp.pp_hum [%sexp (expected : Sexp.t list result)]
      Sexp.pp_hum [%sexp (parsexp : (Sexp.t list, A.Error.t) Result.t)];

  let sexplib =
    match sexplib_sexps_of_string str with
    | sexps -> Ok sexps
    | exception ((Sexplib.Sexp.Parse_error _) as exn) -> Error exn
  in
  let matches_expected =
    match sexplib, expected with
    | Ok a, Sexp b -> [%compare.equal: Sexp.t list] a b
    | Error _, Error -> true
    | _ -> false
  in
  if not matches_expected then
    Caml.Format.printf "sexplib does not match expected:@.\
                        input:    %S@.\
                        expected: %a@.\
                        sexplib:      %a@.\
                        @."
      str
      Sexp.pp_hum [%sexp (expected : Sexp.t list result)]
      Sexp.pp_hum [%sexp (sexplib : (Sexp.t list, exn) Result.t)];
;;

let witness_for_class cl =
  let rec loop i =
    if i = 256 then raise Caml.Not_found;
    if Char_class.of_char (Char.of_int_exn i) = cl then
      Char.of_int_exn i
    else
      loop (i + 1)
  in
  let ch = loop 0 in
  match ch with
  | '\000' -> if Char_class.of_char 'z' = cl then 'z' else ch
  | _ -> ch

let some_cases : (string * Sexp.t result) list =
  [ "foo" , Sexp (Atom "foo")
  ; " foo", Sexp (Atom "foo")
  ; " zoo", Sexp (Atom "zoo")
  ; " 123", Sexp (Atom "123")

  ; "(foo bar)", Sexp (List [Atom "foo"; Atom "bar"])

  ; "\\"        , Sexp (Atom "\\")
  ; "|"         , Sexp (Atom "|")
  ; "#"         , Sexp (Atom "#")
  ; "; foo\nx"  , Sexp (Atom "x")
  ; "; foo\r\nx", Sexp (Atom "x")

  ; "(# )"    , Sexp (List [Atom "#"])
  ; "##"      , Sexp (Atom "##")
  ; "#|x|#z"  , Sexp (Atom "z")
  ; "(#\n)"   , Sexp (List [Atom "#"])
  ; "(#\r\n)" , Sexp (List [Atom "#"])
  ; "#x"      , Sexp (Atom "#x")
  ; "(#())"   , Sexp (List [Atom "#"; List []])
  ; "(#)"     , Sexp (List [Atom "#"])
  ; "(#\"x\")", Sexp (List [Atom "#"; Atom "x"])
  ; "#; x z"  , Sexp (Atom "z")

  ; "foo#"         , Sexp (Atom "foo#")
  ; "(foo# )"      , Sexp (List [Atom "foo#"])
  ; "(foo#\n)"     , Sexp (List [Atom "foo#"])
  ; "(foo#\r\n)"   , Sexp (List [Atom "foo#"])
  ; "foo#x"        , Sexp (Atom "foo#x")
  ; "foo##"        , Sexp (Atom "foo##")
  ; "(foo#\"bar\")", Sexp (List [Atom "foo#"; Atom "bar"])
  ; "(foo#())"     , Sexp (List [Atom "foo#"; List []])
  ; "(foo#)"       , Sexp (List [Atom "foo#"])
  ; "(foo#;\n)"    , Sexp (List [Atom "foo#"])
  ; "foo#|"        , Error
  ; "foo|#"        , Error
  ; "foo|"         , Sexp (Atom "foo|")
  ; "foo|x"        , Sexp (Atom "foo|x")
  ; "(foo| )"      , Sexp (List [Atom "foo|"])
  ; "(foo|\nz)"    , Sexp (List [Atom "foo|"; Atom "z"])
  ; "(foo|\r\nz)"  , Sexp (List [Atom "foo|"; Atom "z"])
  ; "(foo|\"bar\")", Sexp (List [Atom "foo|"; Atom "bar"])
  ; "(foo|())"     , Sexp (List [Atom "foo|"; List []])
  ; "(foo|)"       , Sexp (List [Atom "foo|"])
  ; "(foo|;\n)"    , Sexp (List [Atom "foo|"])
  ; "foo||"        , Sexp (Atom "foo||")

  ; "("  , Error
  ; ")"  , Error
  ; "\r" , Error
  ; "\"" , Error
  ; "x(" , Error
  ; "x)" , Error
  ; "x\r", Error
  ; "x\"", Error

  ; "\rx" , Error

  ; "#|\n|#z"            , Sexp (Atom "z")
  ; "#|\"foo\"|#z"       , Sexp (Atom "z")
  ; "#| #| x |# |#z"     , Sexp (Atom "z")
  ; "#| \"x |#\" |#z"    , Sexp (Atom "z")
  ; "#| #x |#z"          , Sexp (Atom "z")
  ; "#| #\n |#z"         , Sexp (Atom "z")
  ; "#| ## |#z"          , Sexp (Atom "z")
  ; "#| #\"x\" |#z"      , Sexp (Atom "z")
  ; "#| |x |#z"          , Sexp (Atom "z")
  ; "#| |\n |#z"         , Sexp (Atom "z")
  ; "#| |\"x\" |#z"      , Sexp (Atom "z")
  ; "#| || |#z"          , Sexp (Atom "z")
  ; "#| \"\n\" |#z"      , Sexp (Atom "z")
  ; "#| \"\\n\" |#z"     , Sexp (Atom "z")
  ; "#| \"\\z\" |#z"     , Sexp (Atom "z")
  ; "#| \"\\\n\" |#z"    , Sexp (Atom "z")
  ; "#| \"\\\r\n\" |#z"  , Sexp (Atom "z")
  ; "#| \"\\123\" |#z"   , Sexp (Atom "z")
  ; "#| \"\\x42\" |#z"   , Sexp (Atom "z")
  ; "#| \"\\\rx\" |#z"   , Sexp (Atom "z")
  ; "#| \"\\\r\" |#z"    , Sexp (Atom "z")
  ; "#| \"\\\r\\n\" |#z" , Sexp (Atom "z")
  ; "#| \"\\1z\" |#z"    , Error
  ; "#| \"\\12z\" |#z"   , Error
  ; "#| \"\\xz\" |#z"    , Error
  ; "#| \"\\x1z\" |#z"   , Error
  ; "#| \"\\\n x\" |#z"  , Sexp (Atom "z")
  ; "#| \"\\\n \t\" |#z" , Sexp (Atom "z")
  ; "#| \"\\\n \n\" |#z" , Sexp (Atom "z")
  ; "#| \"\\\n \\z\" |#z", Sexp (Atom "z")

  ; "\""     , Error
  ; "\"\\"   , Error
  ; "\"\\\r" , Error
  ; "\"\\1"  , Error
  ; "\"\\12" , Error
  ; "\"\\x"  , Error
  ; "\"\\x1" , Error
  ; "\"\\\n ", Error

  ; "#|"        , Error
  ; "#| #"      , Error
  ; "#| |"      , Error
  ; "#| \""     , Error
  ; "#| \"\\"   , Error
  ; "#| \"\\\r" , Error
  ; "#| \"\\1"  , Error
  ; "#| \"\\12" , Error
  ; "#| \"\\x"  , Error
  ; "#| \"\\x1" , Error
  ; "#| \"\\\n ", Error

  ; "\012z" , Sexp (Atom "z")
  ; "\rz"   , Error
  ; "\r\t"  , Error
  ; "\r\012", Error
  ; "\r\r"  , Error
  ; "\r\"\"", Error
  ; "\r#"   , Error
  ; "\r()"  , Error
  ; "(\r)"  , Error
  ; "\r123" , Error
  ; "\r;"   , Error
  ; "\rA"   , Error
  ; "\r\\"  , Error
  ; "\r|"   , Error

  ; "(abc\012)", Sexp (List [Atom "abc"])
  ; "abcA"     , Sexp (Atom "abcA")
  ; "abc\\"    , Sexp (Atom "abc\\")
  ; "ax"       , Sexp (Atom "ax")
  ; "a#z"      , Sexp (Atom "a#z")
  ; "(a#\012)" , Sexp (List [Atom "a#"])
  ; "a#0"      , Sexp (Atom "a#0")
  ; "a#A"      , Sexp (Atom "a#A")
  ; "a#\\"     , Sexp (Atom "a#\\")
  ; "a|z"      , Sexp (Atom "a|z")
  ; "(a|\012)" , Sexp (List [Atom "a|"])
  ; "a|0"      , Sexp (Atom "a|0")
  ; "a|A"      , Sexp (Atom "a|A")
  ; "a|\\"     , Sexp (Atom "a|\\")

  ; ";\012\nz" , Sexp (Atom "z")
  ; ";\"\nz"   , Sexp (Atom "z")
  ; ";#\nz"    , Sexp (Atom "z")
  ; ";(\nz"    , Sexp (Atom "z")
  ; ";)\nz"    , Sexp (Atom "z")
  ; ";0\nz"    , Sexp (Atom "z")
  ; ";;\nz"    , Sexp (Atom "z")
  ; ";\\\nz"   , Sexp (Atom "z")
  ; ";x\nz"    , Sexp (Atom "z")
  ; ";|\nz"    , Sexp (Atom "z")

  ; "#z"     , Sexp (Atom "#z" )
  ; "(#\012)", Sexp (List [Atom "#"])
  ; "#0"     , Sexp (Atom "#0" )
  ; "#A"     , Sexp (Atom "#A" )
  ; "#\\"    , Sexp (Atom "#\\")

  ; "#|z|#z"   , Sexp (Atom "z")
  ; "#|x|#z"   , Sexp (Atom "z")
  ; "#|\t|#z"  , Sexp (Atom "z")
  ; "#|\012|#z", Sexp (Atom "z")
  ; "#|(|#z"   , Sexp (Atom "z")
  ; "#|)|#z"   , Sexp (Atom "z")
  ; "#|;|#z"   , Sexp (Atom "z")
  ; "#|A|#z"   , Sexp (Atom "z")
  ; "#|||#z"   , Sexp (Atom "z")
  ; "#||#z"    , Sexp (Atom "z")
  ; "#|\r|#z"  , Sexp (Atom "z")
  ; "#|0|#z"   , Sexp (Atom "z")
  ; "#|\\|#z"  , Sexp (Atom "z")

  ; "#| |z|#z"   , Sexp (Atom "z")
  ; "#| |x|#z"   , Sexp (Atom "z")
  ; "#| |\t|#z"  , Sexp (Atom "z")
  ; "#| |\012|#z", Sexp (Atom "z")
  ; "#| |(|#z"   , Sexp (Atom "z")
  ; "#| |)|#z"   , Sexp (Atom "z")
  ; "#| |;|#z"   , Sexp (Atom "z")
  ; "#| |A|#z"   , Sexp (Atom "z")
  ; "#| |||#z"   , Sexp (Atom "z")
  ; "#| ||#z"    , Sexp (Atom "z")
  ; "#| |\r|#z"  , Sexp (Atom "z")
  ; "#| |0|#z"   , Sexp (Atom "z")
  ; "#| |\\|#z"  , Sexp (Atom "z")

  ; "#| #z|#z"   , Sexp (Atom "z")
  ; "#| #x|#z"   , Sexp (Atom "z")
  ; "#| #\t|#z"  , Sexp (Atom "z")
  ; "#| #\012|#z", Sexp (Atom "z")
  ; "#| #(|#z"   , Sexp (Atom "z")
  ; "#| #)|#z"   , Sexp (Atom "z")
  ; "#| #;|#z"   , Sexp (Atom "z")
  ; "#| #A|#z"   , Sexp (Atom "z")
  ; "#| #\r|#z"  , Sexp (Atom "z")
  ; "#| #0|#z"   , Sexp (Atom "z")
  ; "#| #\\|#z"  , Sexp (Atom "z")
  ]

let quoted_string_cases : (string * Sexp.t result) list =
  [ "\"foo\\\\bar\n1\r\n2\\\n3\\\r\n4\\\n      5\"",
    Sexp (Atom "foo\\bar\n1\r\n2345")

  ; "\"\\\r\"", Sexp (Atom "\r")

  ; "\"\\\n   \""     , Sexp (Atom "")
  ; "\"\\\n   \n\""   , Sexp (Atom "\n")
  ; "\"\\\n   \\123\"", Sexp (Atom "\123")

  ; {|"\123"|}, Sexp (Atom "\123")
  ; {|"\x42"|}, Sexp (Atom "\x42")

  ; "\"\\\rx\""    , Sexp (Atom "\rx")
  ; "\"\\\r\""     , Sexp (Atom "\r")
  ; "\"\\\r\\123\"", Sexp (Atom "\r\123")

  ; "\"\\1z\"" , Error
  ; "\"\\12z\"", Error
  ; "\"\\xz\"" , Error
  ; "\"\\x1z\"", Error

  ; "\"\t\""  , Sexp (Atom "\t"  )
  ; "\"\012\"", Sexp (Atom "\012")
  ; "\"#\""   , Sexp (Atom "#"   )
  ; "\"(\""   , Sexp (Atom "("   )
  ; "\")\""   , Sexp (Atom ")"   )
  ; "\";\""   , Sexp (Atom ";"   )
  ; "\"|\""   , Sexp (Atom "|"   )

  ; "\"\\z\""   , Sexp (Atom "\\z"   )
  ; "\"\\ \""  , Sexp (Atom "\\ "  )
  ; "\"\\\t\""  , Sexp (Atom "\\\t"  )
  ; "\"\\\012\"", Sexp (Atom "\\\012")
  ; "\"\\#\""   , Sexp (Atom "\\#"   )
  ; "\"\\(\""   , Sexp (Atom "\\("   )
  ; "\"\\)\""   , Sexp (Atom "\\)"   )
  ; "\"\\;\""   , Sexp (Atom "\\;"   )
  ; "\"\\A\""   , Sexp (Atom "\\A"   )
  ; "\"\\|\""   , Sexp (Atom "\\|"   )
  ; "\"\\\""    , Error

  ; "\"\\\rz\""   , Sexp (Atom "\rz"   )
  ; "\"\\\r\t\""  , Sexp (Atom "\r\t"  )
  ; "\"\\\r\012\"", Sexp (Atom "\r\012")
  ; "\"\\\r#\""   , Sexp (Atom "\r#"   )
  ; "\"\\\r(\""   , Sexp (Atom "\r("   )
  ; "\"\\\r)\""   , Sexp (Atom "\r)"   )
  ; "\"\\\r;\""   , Sexp (Atom "\r;"   )
  ; "\"\\\rA\""   , Sexp (Atom "\rA"   )
  ; "\"\\\r|\""   , Sexp (Atom "\r|"   )
  ; "\"\\\r\""    , Sexp (Atom "\r"    )
  ; "\"\\\r\r\""  , Sexp (Atom "\r\r"  )
  ; "\"\\\r0\""   , Sexp (Atom "\r0"   )

  ; "\"\\1z\""   , Error
  ; "\"\\1\t\""  , Error
  ; "\"\\1\012\"", Error
  ; "\"\\1#\""   , Error
  ; "\"\\1(\""   , Error
  ; "\"\\1)\""   , Error
  ; "\"\\1;\""   , Error
  ; "\"\\1A\""   , Error
  ; "\"\\1|\""   , Error
  ; "\"\\1\""    , Error
  ; "\"\\1\r\""  , Error
  ; "\"\\1\n\""  , Error
  ; "\"\\1\\\""  , Error
  ; "\"\\1x\""   , Error

  ; "\"\\12z\""   , Error
  ; "\"\\12\t\""  , Error
  ; "\"\\12\012\"", Error
  ; "\"\\12#\""   , Error
  ; "\"\\12(\""   , Error
  ; "\"\\12)\""   , Error
  ; "\"\\12;\""   , Error
  ; "\"\\12A\""   , Error
  ; "\"\\12|\""   , Error
  ; "\"\\12\""    , Error
  ; "\"\\12\r\""  , Error
  ; "\"\\12\n\""  , Error
  ; "\"\\12\\\""  , Error
  ; "\"\\12x\""   , Error

  ; "\"\\xz\""   , Error
  ; "\"\\x\t\""  , Error
  ; "\"\\x\012\"", Error
  ; "\"\\x#\""   , Error
  ; "\"\\x(\""   , Error
  ; "\"\\x)\""   , Error
  ; "\"\\x;\""   , Error
  ; "\"\\xA\""   , Error
  ; "\"\\x|\""   , Error
  ; "\"\\x\""    , Error
  ; "\"\\x\r\""  , Error
  ; "\"\\x\n\""  , Error
  ; "\"\\x\\\""  , Error
  ; "\"\\xx\""   , Error

  ; "\"\\x1z\""   , Error
  ; "\"\\x1\t\""  , Error
  ; "\"\\x1\012\"", Error
  ; "\"\\x1#\""   , Error
  ; "\"\\x1(\""   , Error
  ; "\"\\x1)\""   , Error
  ; "\"\\x1;\""   , Error
  ; "\"\\x1A\""   , Sexp (Atom "\x1a")
  ; "\"\\x1|\""   , Error
  ; "\"\\x1\""    , Error
  ; "\"\\x1\r\""  , Error
  ; "\"\\x1\n\""  , Error
  ; "\"\\x1\\\""  , Error
  ; "\"\\x1x\""   , Error

  ; "\"\\\nz\""   , Sexp (Atom "z"   )
  ; "\"\\\nx\""   , Sexp (Atom "x"   )
  ; "\"\\\n\t\""  , Sexp (Atom ""    )
  ; "\"\\\n\012\"", Sexp (Atom "\012")
  ; "\"\\\n#\""   , Sexp (Atom "#"   )
  ; "\"\\\n(\""   , Sexp (Atom "("   )
  ; "\"\\\n)\""   , Sexp (Atom ")"   )
  ; "\"\\\n;\""   , Sexp (Atom ";"   )
  ; "\"\\\nA\""   , Sexp (Atom "A"   )
  ; "\"\\\n|\""   , Sexp (Atom "|"   )
  ; "\"\\\n\""    , Sexp (Atom ""    )
  ; "\"\\\n\r\""  , Sexp (Atom "\r"  )
  ; "\"\\\n0\""   , Sexp (Atom "0"   )
  ]

let trailing_whitespace_cases : (string * Sexp.t result) list =
  [
    "x " , Sexp (Atom "x")
  ; "x\n", Sexp (Atom "x")
  ; "x;" , Sexp (Atom "x")
  ]


let cases =
  some_cases @
  quoted_string_cases @
  trailing_whitespace_cases @
  List.map quoted_string_cases ~f:(fun (s, result) ->
    ("#|  " ^ s ^ "  |#blah",
     match result with
     | Sexp _ -> Sexp (Sexp.Atom "blah")
     | Error  -> Error))
;;

let zero_sexp_cases : (string * Sexp.t list result) list =
  [ " "  , Sexp []
  ; "\n" , Sexp []
  ; ";"  , Sexp []
  ]

let%expect_test "coverage" =
  List.iter cases ~f:(fun (input, result) ->
    let result_many =
      match result with
      | Sexp x -> Sexp [x]
      | Error -> Error
    in
    test_one_case_dealing_with_a_single_sexp (input, result     );
    test_one_case_dealing_with_many_sexps    (input, result_many));
  List.iter zero_sexp_cases ~f:test_one_case_dealing_with_many_sexps;

  (* Transition after an error *)
  let after_error () =
    let state = A.new_state Many Sexp in
    match feed state ')' A.empty_stack with
    | (_ : A.stack) -> assert false
    | exception _ -> state
  in
  List.iter Char.all ~f:(fun c ->
    let state = after_error () in
    match feed state c A.empty_stack with
    | (_ : A.stack) -> assert false
    | exception _ -> ());
  (let state = after_error () in
   match feed_eoi state A.empty_stack with
   | (_ : A.stack) -> assert false
   | exception _ -> ());

  for state = 0 to State.count - 1 do
    for cl = 0 to Char_class.count - 1 do
      if not transition_seen.(state * Char_class.count + cl) then
        printf !"state %{sexp:State.t}, feed %C\n"
          (State.of_int state) (witness_for_class cl)
    done
  done;
  for s = 0 to State.count - 1 do
    if not transition_eoi_seen.(s) then
      printf !"state %{sexp:State.t}, feed EOI\n"
        (State.of_int s)
  done;
  (* This output represent the list of transition not visited. It must be empty. *)
  [%expect{| |}];

  printf "%d of %d"
    (List.length !cases_where_sexplib_disagree_with_itself)
    (List.length cases);
  [%expect "131 of 409"];

  List.iter !cases_where_sexplib_disagree_with_itself
    ~f:(fun (input, sexplib, sexplib_lexer) ->
      let suppress =
        (* there are many examples where continuation-based parser fails and lexer
           succeeds so we filter these out: lexer interprets broken hex and decimal
           escape sequences as if they are not escape sequences at all

           Also lexer interprets "\\\r" differently: it keeps the slash, but we drop the
           slash
        *)
        String.is_substring ~substring:{|"\x|} input
        || String.is_substring ~substring:{|"\1|} input
        || String.is_substring ~substring:({|"\|} ^ "\r") input
      in
      if (not suppress) then
        Caml.Format.printf
          "the various sexplib parsers disagree between themselves on this case:@.\
           input:         %S@.\
           sexplib:       %a@.\
           sexplib_lexer: %a@.\
           @."
          input
          Sexp.pp_hum [%sexp (sexplib       : (Sexp.t, exn) Result.t)]
          Sexp.pp_hum [%sexp (sexplib_lexer : (Sexp.t, exn) Result.t)]);
  [%expect{|
    the various sexplib parsers disagree between themselves on this case:
    input:         "\"\\ \""
    sexplib:       (Ok "\\ ")
    sexplib_lexer: (Ok " ") |}]
;;
