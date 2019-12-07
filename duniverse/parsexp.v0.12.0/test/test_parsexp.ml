open Import
open Parsexp

let test s =
  Single.parse_string s
  |> [%sexp_of: (Sexp.t, Parse_error.t) Result.t]
  |> print_s
;;

let%expect_test "unterminated sexp" =
  test "(abc";
  [%expect {|
    (Error (
      (position (
        (line   1)
        (col    4)
        (offset 4)))
      (message "unclosed parentheses at end of input")))
  |}];
  test "  ";
  [%expect {|
    (Error (
      (position (
        (line   1)
        (col    2)
        (offset 2)))
      (message "no s-expression found in input")))
  |}]
;;

let%expect_test "parsexp bug: it accepts invalid syntax" =
  test "(#;)";
  [%expect {|
    (Error (
      (position (
        (line   1)
        (col    3)
        (offset 3)))
      (message "unterminated sexp comment"))) |}];
  test "#;(#;) a";
  [%expect {|
    (Error (
      (position (
        (line   1)
        (col    5)
        (offset 5)))
      (message "unterminated sexp comment"))) |}];
  test "#;";
  [%expect {|
    (Error (
      (position (
        (line   1)
        (col    2)
        (offset 2)))
      (message "unterminated sexp comment"))) |}];
  test "#;(#;q) (a)";
  [%expect {| (Ok (a)) |}];
  test "#;(#;(q)) (a)";
  [%expect {| (Ok (a)) |}];
  test "#;(#;(q)) a";
  [%expect {| (Ok a) |}];
  test "#;(#;((q))) a";
  [%expect {| (Ok a) |}];
  test "#;#;(#;x)y a";
  [%expect {| (Ok a) |}];
;;

let%expect_test "regression test (used to raise s-expression followed by data)" =
  test "a #;0";
  [%expect {|
    (Ok a) |}];
;;

let parse_eager_cst ~no_sexp_is_error str =
  let r = ref [] in
  let state =
    Parsexp.Eager_cst.State.create ~no_sexp_is_error
      (fun _ v -> r := v :: !r)
  in
  Result.try_with (fun () ->
    let stack = Parsexp.Eager_cst.Stack.empty in
    let stack = Parsexp.Eager_cst.feed_string state str stack in
    Parsexp.Eager_cst.feed_eoi state stack;
    List.rev !r)
;;

let parse_eager_cst_no_sexp_is_error =
  parse_eager_cst ~no_sexp_is_error:true

let parse_eager_cst =
  parse_eager_cst  ~no_sexp_is_error:false

let%expect_test "regression test (we didn't run the callback on comments, resulting in \
                 missing comments or assertion failures)" =
  let test s =
    parse_eager_cst s
    |> [%sexp_of: (Cst.t_or_comment list, exn) Result.t]
    |> print_s
  in
  test ";";
  [%expect {|
    (Ok ((
      Comment (
        Plain_comment
        (loc (
          (start_pos (
            (line   1)
            (col    0)
            (offset 0)))
          (end_pos (
            (line   1)
            (col    1)
            (offset 1)))))
        (comment ";"))))) |}];
  test "#||#";
  [%expect {|
    (Ok ((
      Comment (
        Plain_comment
        (loc (
          (start_pos (
            (line   1)
            (col    0)
            (offset 0)))
          (end_pos (
            (line   1)
            (col    4)
            (offset 4)))))
        (comment "#||#"))))) |}];
  test "#;#;a b";
  [%expect {|
    (Ok ((
      Comment (
        Sexp_comment
        (hash_semi_pos (
          (line   1)
          (col    0)
          (offset 0)))
        (comments ((
          Sexp_comment
          (hash_semi_pos (
            (line   1)
            (col    2)
            (offset 2)))
          (comments ())
          (sexp (
            Atom
            (loc (
              (start_pos (
                (line   1)
                (col    4)
                (offset 4)))
              (end_pos (
                (line   1)
                (col    5)
                (offset 5)))))
            (atom a)
            (unescaped ()))))))
        (sexp (
          Atom
          (loc (
            (start_pos (
              (line   1)
              (col    6)
              (offset 6)))
            (end_pos (
              (line   1)
              (col    7)
              (offset 7)))))
          (atom b)
          (unescaped ()))))))) |}];
  test "a;\nb";
  [%expect {|
    (Ok (
      (Sexp (
        Atom
        (loc (
          (start_pos (
            (line   1)
            (col    0)
            (offset 0)))
          (end_pos (
            (line   1)
            (col    1)
            (offset 1)))))
        (atom a)
        (unescaped ())))
      (Comment (
        Plain_comment
        (loc (
          (start_pos (
            (line   1)
            (col    1)
            (offset 1)))
          (end_pos (
            (line   1)
            (col    2)
            (offset 2)))))
        (comment ";")))
      (Sexp (
        Atom
        (loc (
          (start_pos (
            (line   2)
            (col    0)
            (offset 3)))
          (end_pos (
            (line   2)
            (col    1)
            (offset 4)))))
        (atom b)
        (unescaped ()))))) |}];
;;

let%expect_test
  "regression test (we counted comments as sexps for the purpose of ~no_sexp_is_error" =
  let test s =
    parse_eager_cst_no_sexp_is_error s
    |> [%sexp_of: (Cst.t_or_comment list, exn) Result.t]
    |> print_s
  in
  test ";";
  [%expect {|
    (Error (
      parser_automaton_internal.ml.Public.Parse_error
      ((position (
         (line   1)
         (col    1)
         (offset 1)))
       (message "no s-expression found in input")))) |}];
  test "#||#";
  [%expect {|
    (Error (
      parser_automaton_internal.ml.Public.Parse_error
      ((position (
         (line   1)
         (col    4)
         (offset 4)))
       (message "no s-expression found in input")))) |}];
  test "#;#;a b";
  [%expect {|
    (Error (
      parser_automaton_internal.ml.Public.Parse_error
      ((position (
         (line   1)
         (col    7)
         (offset 7)))
       (message "no s-expression found in input")))) |}]
;;

module Stream = Caml.Stream

module P = Eager

let rec hot_loop state stream stack =
  match Stream.peek stream with
  | None -> P.feed_eoi state stack
  | Some char ->
    let stack = P.feed state char stack in
    Stream.junk stream;
    hot_loop state stream stack

exception Got_sexp of Sexp.t

let fetch_sexp (stream : char Stream.t) =
  let got_sexp _state sexp =
    Exn.raise_without_backtrace (Got_sexp sexp)
  in
  let count = Stream.count stream in
  let state = P.State.create got_sexp in
  match hot_loop state stream P.Stack.empty with
  | () -> None
  | exception (Got_sexp sexp) ->
    (* This test is true if the s-expression includes the last character passed to
       the parser *)
    if P.State.offset state > Stream.count stream - count then Stream.junk stream;
    Some sexp

let iter_sexps (stream : char Stream.t) ~f =
  let got_sexp _state sexp = f sexp in
  let state = P.State.create got_sexp in
  hot_loop state stream P.Stack.empty

let input = {|
(Hello World)
(a b c)
"Hello world"
(1 (2 3))
|}

let%expect_test "eager parser raise" =
  let stream = Stream.of_string input in
  let rec loop stream =
    match fetch_sexp stream with
    | None -> assert (Option.is_none (Stream.peek stream))
    | Some sexp -> Caml.Format.printf "got: %a@." Sexp.pp_hum sexp; loop stream
  in
  loop stream;
  [%expect {|
    got: (Hello World)
    got: (a b c)
    got: "Hello world"
    got: (1 (2 3)) |}]
;;

let all_short_strings () =
  let all_chars =
    (* should have an example from every character class
       (see [Char_class] in test_coverage.ml) *)
    [
      '\000';
      '\001';
      '\t';
      ' ';
      '\n';
      '\012';
      '\r';
      '"';
      '#';
      '(';
      ')';
      '8';
      ';';
      'A';
      'b';
      '\\';
      'x';
      '|';
    ]
  in
  List.bind [0;1;2;3;4;]
    ~f:(fun len ->
      List.all (List.init len ~f:(fun _ -> all_chars))
    )
  |> List.map ~f:(String.of_char_list)

(*
   A thorough test of [Eager] semantics expressed in terms of [Many].

   - feed chars one by one,
   - note when sexps are detected,
   - assert that the position reported is how many chars we fed or 1 fewer
   - assert that the resulting chunks if we split at those positions can be parsed
   to the same sexps as the sexps being reported
   - assert that the whole thing parses iff it parses with [Many]
*)
let%expect_test "eager parser semantics" =
  let exception Got_sexp of { offset : int; sexp : Sexp.t } in
  let got_sexp state sexp =
    Exn.raise_without_backtrace (Got_sexp {
      sexp;
      offset = P.State.Read_only.offset state; })
  in
  let inputs = (all_short_strings ()) in
  printf "testing %d inputs\n" (List.length inputs);
  List.iter inputs ~f:(fun input ->
    let rec go start state stack i =
      if Int.(=) i (String.length input)
      then
        (match P.feed_eoi state stack with
         | exception Got_sexp { offset; sexp; } ->
           let ate = P.State.offset state in
           assert (Int.(=) offset ate);
           let fed = i - start in
           assert (List.mem [fed] ~equal:Int.(=) ate);
           (match Single.parse_string (String.sub ~pos:start ~len:ate input) with
            | Error _ -> assert false
            | Ok sexp2 ->
              assert (Sexp.(=) sexp sexp2);
              Ok ())
         | exception exn ->
           Error (i, exn)
         | () ->
           match Many.parse_string (String.sub ~pos:start ~len:(i - start) input) with
           | Ok [] -> Ok ()
           | Ok (_ :: _) -> failwith "lost a sexp"
           | Error error ->
             raise_s [%message
               "succeeded with Eager, but failed with Many"
                 ~pos:(start : int)
                 ~len:(i - start : int)
                 (error : Parse_error.t)
             ])
      else
        match P.feed state (String.get input i) stack with
        | exception Got_sexp { sexp; offset; } ->
          let ate = P.State.offset state in
          assert (Int.(=) offset ate);
          let fed = i + 1 - start in
          assert (List.mem [fed; fed - 1] ~equal:Int.(=) ate);
          let end_ = start + ate in
          (match Single.parse_string (String.sub ~pos:start ~len:ate input) with
           | Error _ ->
             raise_s [%message
               "Eager gave us a sexp that won't parse"
                 (start : int)
                 (i : int)
             ]
           | Ok sexp2 ->
             assert (Sexp.(=) sexp sexp2);
             let state = P.State.create got_sexp in
             go end_ state P.Stack.empty end_)
        | exception exn -> Error (i, exn)
        | stack ->
          go start state stack (i + 1)
    in
    let state = P.State.create got_sexp in
    match go 0 state P.Stack.empty 0 with
    | Ok () -> ()
    | Error exn -> (match Many.parse_string input with
      | Ok result_from_many ->
        print_s [%message
          "failed to parse with Eager, but succeeded with Many"
            (input : string)
            (exn : int * Exn.t)
            (result_from_many : Sexp.t list)
        ]
      | Error _bad -> ())
    | exception bad_thing ->
      print_s [%message "Bad" (input : string) (bad_thing : Exn.t)]
  );
  [%expect {| testing 111151 inputs |}]
;;

let%expect_test "eager parser continue" =
  let stream = Stream.of_string input in
  iter_sexps stream ~f:(Caml.Format.printf "got: %a@." Sexp.pp_hum);
  [%expect{|
    got: (Hello World)
    got: (a b c)
    got: "Hello world"
    got: (1 (2 3))
  |}]
;;

let%expect_test "eager parser incorrect mutation" =
  let stream = Stream.of_string input in
  let state = ref (P.State.create (fun _ _ -> assert false)) in
  let got_sexp _state _sexp = P.State.reset !state in
  state := P.State.create got_sexp;
  show_raise ~hide_positions:true (fun () ->
    hot_loop !state stream P.Stack.empty);
  [%expect {|
    (raised "Assert_failure parser_automaton_internal.ml:LINE:COL")
  |}]
;;

let%expect_test "eager parser feed after raise without reset" =
  let stream = Stream.of_string input in
  let got_sexp _state _sexp = raise Caml.Exit in
  let state = P.State.create got_sexp in
  (try
     hot_loop state stream P.Stack.empty
   with Caml.Exit ->
     ());
  show_raise (fun () ->
    hot_loop state stream P.Stack.empty);
  [%expect {|
    (raised (Failure "Parsexp.Parser_automaton: parser is dead"))
  |}]
;;
