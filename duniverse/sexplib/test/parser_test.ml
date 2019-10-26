open Sexplib
open Test_common


let sexp_of_layout_sexp = Sexp.With_layout.sexp_of_t_or_comment

let sexps_of_layout_sexps l = List.map sexp_of_layout_sexp l

let sexp_of_layout_sexps_or_something sexps_with_layout =
  match Sexp.With_layout.Forget.t_or_comments sexps_with_layout with
  | [s] -> s
  | [] | _ :: _ :: _ as sexps ->
    (* this is an error, but returning something is more effective for debug *)
    Sexp.List (
      Sexp.List sexps ::
      Sexp.List (sexps_of_layout_sexps sexps_with_layout) ::
      []
    )

let parsers = [
  Sexp.of_string, "cont";
  (fun s ->
     (* feeding the characters to the parse function one by one to make sure the cont_state
        machinery is working fine *)
     let pos = 0 in
     match Sexp.parse ~len:1 s with
     | Sexp.Done (t, _) -> t
     | Sexp.Cont (cont_state, parse_fun) ->
       let rec aux parse_fun pos =
         assert (pos < String.length s);
         match parse_fun ~pos ~len:1 s with
         | Sexp.Done (t, _) ->
           if pos + 1 = String.length s then t
           else failwith "Should have reached the end1"
         | Sexp.Cont (cont_state, parse_fun) ->
           aux_cont pos cont_state parse_fun
       and aux_cont pos cont_state parse_fun =
         if pos + 1 = String.length s then
           match cont_state with
           | Sexp.Cont_state.Parsing_atom -> begin
               match parse_fun ~pos:0 ~len:1 " " with
               | Sexp.Done (t, _) -> t
               | Sexp.Cont (Sexp.Cont_state.Parsing_atom, _) ->
                 failwith "incomplete quoted atom"
               | Sexp.Cont _ ->
                 failwith "Should have reached the end2"
             end
           | Sexp.Cont_state.Parsing_sexp_comment
           | Sexp.Cont_state.Parsing_block_comment
           | Sexp.Cont_state.Parsing_toplevel_whitespace ->
             failwith "string contains no sexp"
           | Sexp.Cont_state.Parsing_nested_whitespace ->
             failwith "incomplete"
           | Sexp.Cont_state.Parsing_list -> failwith "Should have reached the end3"
         else aux parse_fun (pos + 1) in
       aux_cont pos cont_state parse_fun
  ), "cont-incremental";
  (fun s -> Sexp.scan_sexp (Lexing.from_string s)), "ocamllex";
  (fun s -> Sexp.Annotated.get_sexp (Sexp.Annotated.of_string s)), "annot";
  (fun s ->
     let sexps_with_layout =
       Sexp.With_layout.Parser.sexps
         Sexp.With_layout.Lexer.main (Lexing.from_string s)
     in
     sexp_of_layout_sexps_or_something sexps_with_layout
  ), "layout-sexps";
  (fun s ->
     let parse =
       let lexbuf = Lexing.from_string s in
       fun () ->
         Sexp.With_layout.Parser.sexp_opt Sexp.With_layout.Lexer.main lexbuf
     in
     let rec aux acc =
       match parse () with
       | None -> List.rev acc
       | Some sexp -> aux (sexp :: acc)
     in
     sexp_of_layout_sexps_or_something (aux [])
  ), "layout-sexp-opt";
]

(* no idea why we can't read multiple sexps from a string directly *)
let put_string_in_channel s f =
  let file, out_channel = Filename.open_temp_file "sexplib_test" "" in
  output_string out_channel s;
  close_out out_channel;
  let in_channel = open_in file in
  let v = try `Value (f in_channel) with exn -> `Exn exn in
  close_in in_channel;
  Sys.remove file;
  match v with
  | `Value v -> v
  | `Exn exn -> raise exn

let list_parsers = [
  (fun s -> Sexp.List (put_string_in_channel s Sexp.input_sexps)), "cont";
  (fun s -> Sexp.List (Sexp.scan_sexps (Lexing.from_string s))), "ocamllex";
  (fun s -> Sexp.List (Sexp.scan_sexps_conv ~f:(fun x -> x) (Lexing.from_string s))), "ocamllex_opt";
  (fun s ->
     Sexp.List (
       put_string_in_channel s (fun ch ->
         List.map Sexp.Annotated.get_sexp (Sexp.Annotated.input_sexps ch)
       )
     )), "annot";
  (fun s ->
     let sexps_with_layout =
       Sexp.With_layout.Parser.sexps
         Sexp.With_layout.Lexer.main (Lexing.from_string s) in
     let sexps = Sexp.With_layout.Forget.t_or_comments sexps_with_layout in
     Sexp.List sexps
  ), "layout-sexps";
]

let tests = ref 0
let failures = ref 0

let string_of_loc loc =
  Printf.sprintf "%s:%d:%d"
    loc.Lexing.pos_fname
    loc.Lexing.pos_lnum
    (loc.Lexing.pos_cnum - loc.Lexing.pos_bol)

let same_parse_tree ?no_following_sibling ?(use_list_parsers=false) loc string1 string2 =
  let context_wrappers = wrap_in_context ?no_following_sibling () in
  List.iter (fun context_wrapper ->
    let string1 = context_wrapper string1 in
    let string2 = context_wrapper string2 in
    List.iter (fun (parser_, parser_name) ->
      List.iter (fun (adapt_newline, newline_style) ->
        incr tests;
        try
          let string1 = adapt_newline string1 in
          let string2 = adapt_newline string2 in
          let tree1 = parser_ string1 in
          let tree2 = parser_ string2 in
          if tree1 <> tree2 then (
            incr failures;
            Printf.printf
              "test failure at %s (%s, %s)\n  string1: %S tree1: %s\n  string2: %S tree2: %s\n%!"
              (string_of_loc loc) parser_name newline_style
              string1 (Sexp.to_string tree1)
              string2 (Sexp.to_string tree2)
          )
        with e ->
          incr failures;
          Printf.printf "test failure at %s (%s, %s, %s) on %S vs %S\n%!"
            (string_of_loc loc) (Printexc.to_string e) parser_name newline_style string1 string2
      ) newline_adapters
    ) (if use_list_parsers then list_parsers else parsers)
  ) context_wrappers

let same_parse_trees ?no_following_sibling loc string1 string2 =
  same_parse_tree ?no_following_sibling ~use_list_parsers:true loc string1 string2

let parse_fail ?no_following_sibling ?(use_list_parsers=false) loc string f =
  let context_wrappers = wrap_in_context ?no_following_sibling () in
  List.iter (fun context_wrapper ->
    let string = context_wrapper string in
    List.iter (fun (parser_, parser_name) ->
      List.iter (fun (adapt_newline, newline_style) ->
        incr tests;
        try
          let string = adapt_newline string in
          let tree = parser_ string in
          incr failures;
          Printf.printf
            "test failure at %s (%s, %s): should have thrown an exception\nstring: %S tree: %s\n%!"
            (string_of_loc loc) parser_name newline_style string (Sexp.to_string tree)
        with e ->
          if not (f e) then (
            incr failures;
            Printf.printf "test failure at %s (%s, %s, %s)\n%!"
              (string_of_loc loc) (Printexc.to_string e) parser_name newline_style
          )
      ) newline_adapters
    ) (if use_list_parsers then list_parsers else parsers)
  ) context_wrappers

let parse_fail_trees ?no_following_sibling loc string f =
  parse_fail ?no_following_sibling ~use_list_parsers:true loc string f

let grep pattern string =
  (* hopefully there is no need for escaping *)
  Sys.command ("echo '" ^ string ^ "' | grep -q '" ^ pattern ^ "'") = 0

let round_trip_all_characters () =
  let check_string s =
    let s' =
      let open Sexplib.Conv in
      s |> sexp_of_string |> Sexp.to_string |> Sexp.of_string |> string_of_sexp
    in
    assert (s = s')
  in
  for i = 0 to 255 do
    check_string (String.make 1 (Char.chr i))
  done
;;

(* the Sexp.input_sexps function tested below has a loop that uses
   the continuation mechanism of the parser if the whole thing cannot be
   loaded at once. (The parser named "cont-incremental" in the variable
   "parsers" above does exercise the continuation mechanism but its logic
   is coded in the present file, not in the library functions.)
*)
let load_large_sexp () =
  (* note: the file is expected to be larger than the default size of buffers *)
  let num = 2048 in
  let items = Buffer.create 8192 in
  Buffer.add_char items '(';
  for i = 0 to pred num do
    Buffer.add_string items (Printf.sprintf "item_%d\n" i)
  done;
  Buffer.add_char items ')';
  match put_string_in_channel (Buffer.contents items) Sexp.input_sexps with
  | [ Sexp.List l ] ->
    assert (List.length l = num);
    List.iteri
      (fun i sexp ->
         match sexp with
         | Sexp.Atom name ->
           assert (name = Printf.sprintf "item_%d" i)
         | Sexp.List _ ->
           failwith "expected to find bare atom in the list")
      l
  | _ ->
    failwith "load_large_sexp: expected to load a list"

let%test_unit _ =
  round_trip_all_characters ();
  load_large_sexp ();
  same_parse_tree [%here] "(a)" "(a;\n)"; (* single line comment in a list *)
  same_parse_tree [%here] ";\nb" "b"; (* single line comment at toplevel *)
  same_parse_tree [%here] ";;\nb" "b"; (* single line comment don't nest *)
  same_parse_tree [%here] ";\"\nb" "b"; (* single line comment ignore quotes *)
  same_parse_tree [%here] "(a#)" "(a#;\n)";
  parse_fail [%here] "a#|"
    (function
      | Failure s -> grep "comment tokens in unquoted atom" s
      | Sexp.Parse_error _ -> true
      | _ -> false);
  parse_fail [%here] "a|#"
    (function
      | Failure s -> grep "comment tokens in unquoted atom" s
      | Sexp.Parse_error _ -> true
      | _ -> false);
  parse_fail [%here] "##|"
    (function
      | Failure s -> grep "comment tokens in unquoted atom" s
      | Sexp.Parse_error _ -> true
      | _ -> false);
  parse_fail [%here] "||#"
    (function
      | Failure s -> grep "comment tokens in unquoted atom" s
      | Sexp.Parse_error _ -> true
      | _ -> false);
  parse_fail_trees [%here] "#|" (* not terminated *)
    (function
      | Failure s ->
        grep "incomplete" s || grep "unterminated" s
        || grep "reached EOF while in state" s
      | _ -> false);
  parse_fail_trees [%here] "\"" (* unterminated quoted atom *)
    (function
      | Failure s ->
        grep "incomplete" s || grep "unterminated" s
        || grep "reached EOF while in state" s
      | _ -> false);
  parse_fail_trees ~no_following_sibling:true [%here] "\"\\"
    (function
      | Failure s ->
        grep "incomplete" s || grep "unterminated" s || grep "empty token" s
        || grep "reached EOF while in state" s
      | _ -> false);
  parse_fail_trees ~no_following_sibling:true [%here] "\"\\\n"
    (function
      | Failure s ->
        grep "incomplete" s || grep "unterminated" s || grep "empty token" s
        || grep "reached EOF while in state" s
      | _ -> false);
  parse_fail_trees ~no_following_sibling:true [%here] "\"\\\013"
    (function
      | Failure s ->
        grep "incomplete" s || grep "unterminated" s || grep "empty token" s
        || grep "reached EOF while in state" s
      | _ -> false);
  parse_fail_trees ~no_following_sibling:true [%here] "\"\\0"
    (function
      | Failure s ->
        grep "incomplete" s || grep "unterminated" s
        || grep "reached EOF while in state" s
      | Sexp.Parse_error _ -> true
      | _ -> false);
  parse_fail_trees ~no_following_sibling:true [%here] "\"\\x"
    (function
      | Failure s ->
        grep "incomplete" s || grep "unterminated" s
        || grep "reached EOF while in state" s
      | Sexp.Parse_error _ -> true
      | _ -> false);
  parse_fail [%here] "\"hello" (* unterminated quoted atom *)
    (function
      | Failure s -> grep "incomplete" s || grep "unterminated" s
      | _ -> false);
  parse_fail [%here] "\013x" (* weird newline *)
    (function
      | Failure s -> grep "unexpected character" s || grep "empty token" s
      | Sexp.Parse_error _ -> true
      | _ -> false);
  parse_fail_trees [%here] "\013" (* trailing weird newline *)
    (function
      | Failure s ->
        grep "incomplete" s || grep "empty token" s
        || grep "reached EOF while in state" s
        || grep "unexpected" s
      | Sexp.Parse_error _ -> true
      | _ -> false);
  parse_fail [%here] "|#" (* not started *)
    (function
      | Failure s -> grep "illegal end of comment" s
      | Sexp.Parse_error _ -> true
      | _ -> false);
  parse_fail [%here] ~no_following_sibling:true "#;" (* not followed *)
    (function
      | Sexp.Parse_error _ | Failure _ -> true
      | _ -> false);

  same_parse_tree [%here] "#;a b" "b"; (* sexp comment + atom *)
  same_parse_tree [%here] "#;((a)) b" "b"; (* sexp comment + list *)
  same_parse_tree [%here] "#;\"#;\" b" "b"; (* sexp comment + quoted atom *)
  same_parse_tree [%here] "#;a;comment\nb" "b"; (* sexp comment + single line commment *)
  same_parse_tree [%here] "#;#|aa)|#comment b" "b"; (* sexp comment + block commment *)
  same_parse_tree [%here] "#;a #;(a) #;\"asd\" b" "b"; (* consecutive sexp comment + atom *)
  same_parse_tree [%here] "(#;a #;(a) #;b)" "()"; (* consecutive sexp comment + nothing *)
  same_parse_tree [%here] "#; #; #; comment1 comment2 comment3 a" "a"; (* nested sexp comment *)
  same_parse_tree [%here] "#| ; |# ()" "()"; (* single line comment are not parsed inside of blocks *)
  same_parse_tree [%here] "#|#||#|#a" "a"; (* consecutive comment opening are not parsed
                                              as one invalid atom *)
  (* why do we need a freaking space at the end?? *)
  same_parse_trees [%here] "a #; b c " "a c ";  (* base case, accepting lists *)
  same_parse_trees [%here] "#;#;a b c d " "c d "; (* leading comments work alright *)
  same_parse_trees [%here] "#;#;a b " " "; (* leading comments work alright *)
  same_parse_trees [%here] "plop #;#;a b " "plop "; (* trailing comments work alright *)
  same_parse_trees [%here] "#;b " " "; (* leading comments in front of nothing *)

  (* making sure that '|' is still accepted in literals *)
  same_parse_tree [%here] "(a|b)" "(\"a|b\")";
  same_parse_tree [%here] "(a | b)" "(a \"|\" b)";
  same_parse_tree [%here] "((a)|b)" "((a)\"|b\")";
  same_parse_tree [%here] "(b|(a))" "(\"b|\"(a))";
  same_parse_trees [%here] "a|b " "\"a|b\" ";
  same_parse_trees [%here] "(a)|b " "(a)\"|b\" ";
  same_parse_trees [%here] "b|(a)" "\"b|\"(a)";
  if !failures <> 0
  then failwith (Printf.sprintf "%d / %d tests failed\n%!" !failures !tests)
;;
