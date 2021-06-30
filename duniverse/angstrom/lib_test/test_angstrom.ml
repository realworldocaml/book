open Angstrom

module Alcotest = struct
  include Alcotest

  let bigstring =
    Alcotest.testable
      (fun fmt _bs -> Fmt.pf fmt "<bigstring>")
      ( = )
end

let check ?size f p is =
  let open Buffered in
  let state =
    List.fold_left (fun state chunk ->
      feed state (`String chunk))
    (parse ?initial_buffer_size:size p) is
  in
  f (state_to_result (feed state `Eof))

let check_ok ?size ~msg test p is r =
  let r = Ok r in
  check ?size (fun result -> Alcotest.(check (result test string)) msg r result)
    p is

let check_fail ?size ~msg p is =
  let r = Error "" in
  check ?size (fun result -> Alcotest.(check (result reject pass)) msg r result)
    p is

let check_c   ?size ~msg p is r = check_ok ?size ~msg Alcotest.char            p is r
let check_lc  ?size ~msg p is r = check_ok ?size ~msg Alcotest.(list char)     p is r
let check_co  ?size ~msg p is r = check_ok ?size ~msg Alcotest.(option char)   p is r
let check_s   ?size ~msg p is r = check_ok ?size ~msg Alcotest.string          p is r
let check_bs  ?size ~msg p is r = check_ok ?size ~msg Alcotest.bigstring       p is r
let check_ls  ?size ~msg p is r = check_ok ?size ~msg Alcotest.(list string)   p is r
let check_int ?size ~msg p is r = check_ok ?size ~msg Alcotest.int             p is r

let bigstring_of_string s = Bigstringaf.of_string s ~off:0 ~len:(String.length s)

let basic_constructors =
  [ "peek_char", `Quick, begin fun () ->
      check_co ~msg:"singleton input"  peek_char ["t"]    (Some 't');
      check_co ~msg:"longer input"     peek_char ["true"] (Some 't');
      check_co ~msg:"empty input"      peek_char [""]     None;
  end
  ; "peek_char_fail", `Quick, begin fun () ->
      check_c    ~msg:"singleton input"  peek_char_fail ["t"]    't';
      check_c    ~msg:"longer input"     peek_char_fail ["true"] 't';
      check_fail ~msg:"empty input"      peek_char_fail [""]
  end
  ; "char", `Quick, begin fun () ->
      check_c    ~msg:"singleton 'a'" (char 'a') ["a"]     'a';
      check_c    ~msg:"prefix 'a'"    (char 'a') ["asdf"]  'a';
      check_fail ~msg:"'a' failure"   (char 'a') ["b"];
      check_fail ~msg:"empty buffer"  (char 'a') [""]
  end
  ; "int8", `Quick, begin fun () ->
    check_int  ~msg:"singleton 'a'" (int8 0x0061) ["a"]     0x61;
    check_int  ~msg:"prefix 'a'"    (int8 0xff61) ["asdf"]  0x61;
    check_fail ~msg:"'a' failure"   (int8 0xff61) ["b"];
    check_fail ~msg:"empty buffer"  (int8 0xff61) [""];
  end
  ; "not_char", `Quick, begin fun () ->
      check_c    ~msg:"not 'a' singleton" (not_char 'a') ["b"] 'b';
      check_c    ~msg:"not 'a' prefix"    (not_char 'a') ["baba"] 'b';
      check_fail ~msg:"not 'a' failure"   (not_char 'a') ["a"];
      check_fail ~msg:"empty buffer"      (not_char 'a') [""]
  end
  ; "any_char", `Quick, begin fun () ->
      check_c    ~msg:"non-empty buffer" any_char ["a"] 'a';
      check_fail ~msg:"empty buffer"     any_char [""]
  end
  ; "any_{,u}int8", `Quick, begin fun () ->
    check_int ~msg:"positive sign preserved" any_int8 ["\127"] 127;
    check_int ~msg:"negative sign preserved" any_int8 ["\129"] (-127);
    check_int ~msg:"sign invariant" any_uint8 ["\127"] 127;
    check_int ~msg:"sign invariant" any_uint8 ["\129"] (129)
  end
  ; "string", `Quick, begin fun () ->
      check_s ~msg:"empty string, non-empty buffer" (string "")     ["asdf"] "";
      check_s ~msg:"empty string, empty buffer"     (string "")     [""]     "";
      check_s ~msg:"exact string match"             (string "asdf") ["asdf"] "asdf";
      check_s ~msg:"string is prefix of input"      (string "as")   ["asdf"] "as";

      check_fail ~msg:"input is prefix of string"     (string "asdf") ["asd"];
      check_fail ~msg:"non-empty string, empty input" (string "test") [""]
  end
  ; "string_ci", `Quick, begin fun () ->
      check_s ~msg:"empty string, non-empty input"  (string_ci "")     ["asdf"] "";
      check_s ~msg:"empty string, empty input"      (string_ci "")     [""]     "";
      check_s ~msg:"exact string match"             (string_ci "asdf") ["AsDf"] "AsDf";
      check_s ~msg:"string is prefix of input"      (string_ci "as")   ["AsDf"] "As";

      check_fail ~msg:"input is prefix of string"     (string_ci "asdf") ["Asd"];
      check_fail ~msg:"non-empty string, empty input" (string_ci "test") [""]
  end
  ; "take_bigstring", `Quick, begin fun () ->
      check_bs ~msg:"empty bigstring"       (take_bigstring 0) ["asdf"] (bigstring_of_string "");
      check_bs ~msg:"bigstring"             (take_bigstring 2) ["asdf"] (bigstring_of_string "as");

      check_fail ~msg:"asking for too much" (take_bigstring 5) ["asdf"];
  end
  ; "take_while", `Quick, begin fun () ->
      check_s ~msg:"true, non-empty input"  (take_while (fun _ -> true)) ["asdf"] "asdf";
      check_s ~msg:"true, empty input"      (take_while (fun _ -> true)) [""] "";
      check_s ~msg:"false, non-empty input" (take_while (fun _ -> false)) ["asdf"] "";
      check_s ~msg:"false, empty input"     (take_while (fun _ -> false)) [""] "";
  end
  ; "take_while1", `Quick, begin fun () ->
      check_s ~msg:"true, non-empty input"     (take_while1 (fun _ -> true)) ["asdf"] "asdf";
      check_fail ~msg:"false, non-empty input" (take_while1 (fun _ -> false)) ["asdf"];
      check_fail ~msg:"true, empty input"      (take_while1 (fun _ -> true)) [""];
      check_fail ~msg:"false, empty input"     (take_while1 (fun _ -> false)) [""];
  end
  ; "advance", `Quick, begin fun () ->
      check_s ~msg:"non-empty input"                (advance 3 >>= fun () -> take 1) ["asdf"] "f";
      check_fail ~msg:"advance more than available" (advance 5) ["asdf"];
      check_fail ~msg:"advance on empty input"      (advance 3) [""];
  end
  ]

module type EndianBigstring = sig
  val set_int16 : Bigstringaf.t -> int -> int -> unit
  val set_int32 : Bigstringaf.t -> int -> int32 -> unit
  val set_int64 : Bigstringaf.t -> int -> int64 -> unit

  val set_float  : Bigstringaf.t -> int -> float -> unit
  val set_double : Bigstringaf.t -> int -> float -> unit
end

module Endian(Es : EndianBigstring) = struct
  type 'a endian = {
    name : string;
    size : int;
    zero : 'a;
    min : 'a;
    max : 'a;
    dump : Bigstringaf.t -> int -> 'a -> unit;
    testable : 'a Alcotest.testable
  }

  let int16 = {
    name = "int16";
    size = 2;
    zero = 0;
    min = ~-32768;
    max = 32767;
    dump = Es.set_int16;
    testable = Alcotest.int
  }
  let int32 = {
    name = "int32";
    size = 4;
    zero = Int32.zero;
    min = Int32.min_int;
    max = Int32.max_int;
    dump = Es.set_int32;
    testable = Alcotest.int32
  }
  let int64 = {
    name = "int64";
    size = 8;
    zero = Int64.zero;
    min = Int64.min_int;
    max = Int64.max_int;
    dump = Es.set_int64;
    testable = Alcotest.int64
  }
  let float = {
    name = "float";
    size = 4;
    zero = 0.0;
    (* XXX: Not really min/max *)
    min = ~-.2e10;
    max = 2e10;
    dump = Es.set_float;
    testable = Alcotest.float 0.0
  }
  let double = {
    name = "double";
    size = 8;
    zero = 0.0;
    (* XXX: Not really min/max *)
    min = ~-.2e30;
    max = 2e30;
    dump = Es.set_double;
    testable = Alcotest.float 0.0
  }

  let uint16 = { int16 with name = "uint16"; min = 0; max = 65535 }
  let uint32 = { int32 with name = "uint32" }

   let dump actual size value =
     let buf = Bigstringaf.of_string ~off:0 ~len:size (String.make size '\xff') in
     actual buf 0 value;
     Bigstringaf.substring ~off:0 ~len:size buf

  let make_tests e parse = e.name, `Quick, begin fun () ->
    check_ok ~msg:"zero"     e.testable parse [dump e.dump e.size       e.zero] e.zero;
    check_ok ~msg:"min"      e.testable parse [dump e.dump e.size       e.min ] e.min;
    check_ok ~msg:"max"      e.testable parse [dump e.dump e.size       e.max ] e.max;
    check_ok ~msg:"trailing" e.testable parse [dump e.dump (e.size + 1) e.zero] e.zero;
  end

  module type EndianSig = module type of LE

  let tests (module E : EndianSig) = [
    make_tests int16  E.any_int16;
    make_tests int32  E.any_int32;
    make_tests int64  E.any_int64;
    make_tests uint16 E.any_uint16;
    make_tests float  E.any_float;
    make_tests double E.any_double;
  ]
end
let little_endian =
  let module E = Endian(struct
    let set_int16  = Bigstringaf.unsafe_set_int16_le
    let set_int32  = Bigstringaf.unsafe_set_int32_le
    let set_int64  = Bigstringaf.unsafe_set_int64_le

    let set_float  bs off f = Bigstringaf.unsafe_set_int32_le bs off (Int32.bits_of_float f)
    let set_double bs off d = Bigstringaf.unsafe_set_int64_le bs off (Int64.bits_of_float d)
  end) in
  E.tests (module LE)

let big_endian =
  let module E = Endian(struct
    let set_int16  = Bigstringaf.unsafe_set_int16_be
    let set_int32  = Bigstringaf.unsafe_set_int32_be
    let set_int64  = Bigstringaf.unsafe_set_int64_be

    let set_float  bs off f = Bigstringaf.unsafe_set_int32_be bs off (Int32.bits_of_float f)
    let set_double bs off d = Bigstringaf.unsafe_set_int64_be bs off (Int64.bits_of_float d)
  end) in
  E.tests (module BE)

let monadic =
  [ "fail", `Quick, begin fun () ->
    check_fail ~msg:"non-empty input" (fail "<msg>") ["asdf"];
    check_fail ~msg:"empty input"     (fail "<msg>") [""]
  end
  ; "return", `Quick, begin fun () ->
    check_s ~msg:"non-empty input" (return "test") ["asdf"] "test";
    check_s ~msg:"empty input"     (return "test") [""]     "test";
  end
  ; "bind", `Quick, begin fun () ->
    check_s ~msg:"data dependency" (take 2 >>= fun s -> string s) ["asas"] "as";
  end
  ]

let applicative =
  [ "applicative", `Quick, begin fun () ->
    check_s ~msg:"`foo *> bar` returns bar" (string "foo" *> string "bar") ["foobar"] "bar";
    check_s ~msg:"`foo <* bar` returns bar" (string "foo" <* string "bar") ["foobar"] "foo";
  end
  ]

let alternative =
  [ "alternative", `Quick, begin fun () ->
      check_c ~msg:"char a | char b" (char 'a' <|> char 'b') ["a"] 'a';
      check_c ~msg:"char b | char a" (char 'b' <|> char 'a') ["a"] 'a';
      check_s ~msg:"string 'a' | string 'b'" (string "a" <|> string "b") ["a"] "a";
      check_s ~msg:"string 'b' | string 'a'" (string "b" <|> string "a") ["a"] "a";
  end ]

let combinators =
  [ "many", `Quick, begin fun () ->
      check_lc ~msg:"empty input"   (many (char 'a')) [""]  [];
      check_lc ~msg:"single char"   (many (char 'a')) ["a"] ['a'];
      check_lc ~msg:"two chars"     (many (char 'a')) ["aa"] ['a'; 'a'];
    end
  ; "many_till", `Quick, begin fun () ->
      check_lc ~msg:"not greedy" (many_till any_char (char '-')) ["ab-ab-"] ['a'; 'b'];
    end
  ; "sep_by1", `Quick, begin fun () ->
      let parser = sep_by1 (char ',') (char 'a') in
      check_lc ~msg:"single char"     parser ["a"]    ['a'];
      check_lc ~msg:"many chars"      parser ["a,a"]  ['a'; 'a'];
      check_lc ~msg:"no trailing sep"  parser ["a,"]   ['a'];
    end
  ; "count", `Quick, begin fun () ->
      check_lc ~msg:"empty input" (count 0 (char 'a')) [""] [];
      check_lc ~msg:"exact input" (count 1 (char 'a')) ["a"] ['a'];
      check_lc ~msg:"additonal input" (count 2 (char 'a')) ["aaa"] ['a'; 'a'];
      check_fail ~msg:"bad input" (count 2 (char 'a')) ["abb"];
    end
  ; "scan_state", `Quick, begin fun () ->
      check_s ~msg:"scan_state" (scan_state "" (fun s -> function
          | 'a' -> Some s
          | '.' -> None
          | c -> Some ((String.make 1 c) ^ s)
        )) ["abaacba."] "bcb";
      let p =
        count 2 (scan_state "" (fun s -> function
            | '.' -> None
            | c -> Some (s ^ String.make 1 c)
          ))
        >>| String.concat "" in
      check_s ~msg:"state reset between runs" p ["bcd."] "bcd";
    end
  ; "consumed", `Quick, begin fun () ->
      check_s ~msg:"from beginning" (consumed any_char)
        ["abc"] "a";
      check_s ~msg:"from middle" (any_char *> consumed any_char)
        ["abc"] "b";
      check_c ~msg:"advances input" (any_char *> consumed any_char *> any_char)
        ["abc"] 'c';
      check_s ~msg:"with backtracking" (consumed (char 'a' *> (char 'c' <|> char 'b')))
        ["abc"] "ab";
      check_s ~msg:"with more input" (consumed (string "abc"))
        ["a"; "bc"] "abc";
      check_fail ~msg:"with commit" (consumed (char 'a' *> commit *> char 'b'))
        ["a"; "b"];
      let integer =
        option '+' (char '-') *> take_while (function '0'..'9' -> true | _ -> false)
      in
      check_int ~msg:"parsing an integer" (consumed integer >>| int_of_string)
        ["-12345"] (-12345);
      check_bs ~msg:"bigstring variant" (consumed_bigstring (string "ab"))
        ["abc"] (bigstring_of_string "ab");
    end
  ]

let incremental =
  [ "within chunk boundary", `Quick, begin fun () ->
      check_s ~msg:"string on each side of 2 inputs"
        (string "this" *> string "that") ["this"; "that"] "that";
      check_s ~msg:"string on each side of 3 inputs"
        (string "thi" *> string "st" *> string "hat") ["thi"; "st"; "hat"] "hat";
      check_s ~msg:"string straddling 2 inputs"
        (string "thisthat") ["this"; "that"] "thisthat";
      check_s ~msg:"string straddling 3 inputs"
        (string "thisthat") ["thi"; "st"; "hat"] "thisthat";
      end
  ; "peek_char and empty chunks", `Quick, begin fun () ->
      let decoder len =
        let open Angstrom in

        let buf = Buffer.create len in

        fix @@ fun m ->
        available >>= function
        | 0 -> peek_char >>= (function
            | Some _ -> commit *> m
            | None ->
              let ret = Buffer.contents buf in
              Buffer.clear buf;
              commit *> return ret)
        | n -> take n >>= fun chunk -> Buffer.add_string buf chunk; commit *> m
      in

      check_s ~msg:"empty input multiple times and peek_char"
        (decoder 0xFF) [ "Whole Lotta Love"; ""; ""; "" ] "Whole Lotta Love"
    end
  ; "across chunk boundary", `Quick, begin fun () ->
      check_s ~size:4 ~msg:"string on each side of 2 chunks"
        (string "this" *> string "that") ["this"; "that"] "that";
      check_s ~size:3 ~msg:"string on each side of 3 chunks"
        (string "thi" *> string "st" *> string "hat") ["thi"; "st"; "hat"] "hat";
      check_s ~size:4 ~msg:"string straddling 2 chunks"
        (string "thisthat") ["this"; "that"] "thisthat";
      check_s ~size:3 ~msg:"string straddling 3 chunks"
        (string "thisthat") ["thi"; "st"; "hat"] "thisthat";
    end
  ; "across chunk boundary with commit", `Quick, begin fun () ->
      check_s ~size:4 ~msg:"string on each side of 2 chunks"
        (string "this" *> commit *> string "that") ["this"; "that"] "that";
      check_s ~size:3 ~msg:"string on each side of 3 chunks"
        (string "thi" *> string "st" *> commit *> string "hat") ["thi"; "st"; "hat"] "hat";
    end ]

let count_while_regression =
  [ "proper position set after count_while", `Quick, begin fun () ->
    check_s ~msg:"take_while then eof"
      (take_while (fun _ -> true) <* end_of_input) ["asdf"; ""] "asdf";
    check_s ~msg:"take_while1 then eof"
      (take_while1 (fun _ -> true) <* end_of_input) ["asdf"; ""] "asdf";
  end ]

let choice_commit =
  [ "", `Quick, begin fun () ->
    let p =
      choice  [ string "@@" *> commit *> char '*'
              ; string "@"  *> commit *> char '!' ]
    in
    Alcotest.(check (result reject string))
      "commit to branch"
      (Error ": char '*'")
      (parse_string ~consume:All p "@@^");
  end ]

let input =
  let test p input ~off ~len expect =
    match Angstrom.Unbuffered.parse p with
    | Done _ | Fail _ -> assert false
    | Partial { continue; committed } ->
      Alcotest.(check int) "committed is zero" 0 committed;
      let bs = Bigstringaf.of_string input ~off:0 ~len:(String.length input) in
      let state = continue bs ~off ~len Complete in
      Alcotest.(check (result string string))
        "offset and length respected"
        (Ok expect)
        (Angstrom.Unbuffered.state_to_result state);
  in

  [ "offset and length respected", `Quick, begin fun () ->
    let open Angstrom in
    let take_all = take_while (fun _ -> true) in
    test take_all             "abcd"    ~off:1 ~len:2 "bc";
    test (take 4 *> take_all) "abcdefg" ~off:0 ~len:7 "efg";
  end ]
;;

let consume =
  [ "consume with choice matching prefix", `Quick, begin fun () ->
    let open Angstrom in
    let parse ~consume =
      parse_string ~consume (many (char 'a')) "aaabbb"
    in
    Alcotest.(check (result (list char) string))
      "consume prefix passes"
      (parse ~consume:Prefix)
      (Ok [ 'a'; 'a'; 'a' ])
      ;
    Alcotest.(check (result (list char) string))
      "consume all fails"
      (parse ~consume:All)
      (Error ": end_of_input");
  end
  ]
;;

let () =
  Alcotest.run "test suite"
    [ "basic constructors"    , basic_constructors
    ; "little endian"         , little_endian
    ; "big endian"            , big_endian
    ; "monadic interface"     , monadic
    ; "applicative interface" , applicative
    ; "alternative"           , alternative
    ; "combinators"           , combinators
    ; "incremental input"     , incremental
    ; "count_while regression", count_while_regression
    ; "choice and commit"     , choice_commit
    ; "input"                 , input
    ; "consume"               , consume
  ]
