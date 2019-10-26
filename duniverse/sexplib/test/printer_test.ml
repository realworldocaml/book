open Ppx_compare_lib.Builtin
open Sexplib
open Sexplib.Std
module With_layout = Sexp.With_layout
open Test_common

(* a hack to ignore these tests for now *)
let care_about_windows_newlines = false

let string_of_sexps_with_layout t_list =
  let open With_layout.Render in
  let b = Buffer.create 10 in
  let m = return () in
  let m =
    List.fold_left (fun m t ->
      bind m ~f:(fun () -> sexp t)
    ) m t_list
  in
  With_layout.Render.run (fun c -> Buffer.add_char b c) m;
  Buffer.contents b

let failures = ref 0
let total = ref 0

let split_newlines s =
  let acc = ref [] in
  let b = Buffer.create 10 in
  let split () =
    acc := Buffer.contents b :: !acc;
    Buffer.clear b in
  for i = 0 to String.length s - 1 do
    if s.[i] = '\n' then split ();
    Buffer.add_char b s.[i]
  done;
  split ();
  List.rev !acc

let rstrip s =
  let rec aux i =
    if i = 0 then "" else
      match s.[i - 1] with
      | ' ' | '\t' | '\n' | '\r' -> aux (i - 1)
      | _ -> String.sub s 0 i in
  aux (String.length s)

let normalize_string s =
  (* removing the spacing at the end and removing the trailing whitespace on each line
     except the final carriage return if any so that \r\n in the input stays \r\n in the
     output *)
  let s = rstrip s in
  let l = split_newlines s in
  let l =
    List.map (fun s ->
      let ends_with_cr = s <> "" && s.[String.length s - 1] = '\r' in
      let s = rstrip s in
      if ends_with_cr && care_about_windows_newlines then s ^ "\r" else s
    ) l in
  String.concat "" l

let test_printer () =
  let tests = [
    (* basic things *)
    "a";
    "a b";
    "a    b ";
    "\na  \n  b \n  ";
    "(\na  \n) () b \n  ";

    (* single line comments *)
    ";\n";
    ";() b \n  ";
    "(;\n)";
    "(a;\nb)";

    (* block comments *)
    "#||#";
    "#| ((a((|#"; (* block comments + random contents *)
    "a\"#|\"b"; (* quoted atom with block comment start *)
    "a \"|#\" b"; (* quoted atom with block comment end *)
    "a #|\"|#\" b|#"; (* block comment with quoted atom  *)
    "a \n#|\"\n|#\" b|#\n"; (* block comment with quoted atom and newlines *)

    (* sexp comment *)
    "#;out in"; (* standard case *)
    "#;out#;out-as-well ;still out\n finally-in"; (* consecutive sexp comments *)
    "#;#;out out-as-well ;still out\n finally-in"; (* nested sexp comments *)
    "#;#;(\"out\" )out-as-well ;still out\n finally-in";
    (* nested sexp comments that comment out more complicated expressions *)

    "#;;line comment1\n;line comment2\nout in";
  ] in
  List.iter (fun test_str ->
    List.iter (fun wrapper ->
      List.iter (fun (newline_adapter, newline_style) ->
        incr total;
        let test_str = newline_adapter (wrapper test_str) in
        let lexbuf = Lexing.from_string test_str in
        let sexps_opt =
          try Some (With_layout.Parser.sexps With_layout.Lexer.main lexbuf)
          with e ->
            incr failures;
            let str = Printexc.to_string e in
            Printf.printf "Failed to parse %S (%s):\n  %s\n%!" test_str newline_style str;
            None in
        match sexps_opt with
        | None -> ()
        | Some sexps ->
          let printed_str_opt =
            try Some (string_of_sexps_with_layout sexps)
            with e ->
              incr failures;
              let str = Printexc.to_string e in
              Printf.printf "Failed to print %S (%s):\n  %s\n  %s\n%!"
                test_str newline_style
                (Sexp.to_string (Sexp.List (With_layout.Forget.t_or_comments sexps)))
                str;
              None
          in
          match printed_str_opt with
          | None -> ()
          | Some printed_str ->
            (* trailing whitespace (at the end of lines or at the end of file) is not
               handled by the parser/printer, except when they are in a comment but that
               is annoying to check so stripping everything from both strings *)
            if normalize_string test_str <> normalize_string printed_str then (
              incr failures;
              Printf.printf "Comparison failed %S (%s):\n\
                            \              got %S after parsing + printing\n\
                            \  test    after normalization: %S\n\
                            \  printed after normalization: %S\n%!"
                test_str newline_style
                printed_str (normalize_string test_str) (normalize_string printed_str);
            )
      ) newline_adapters
    ) (wrap_in_context ())
  ) tests

let%test_unit _ =
  test_printer ();
  if !failures <> 0
  then failwith (Printf.sprintf "%d / %d tests failed\n%!" !failures !total)
;;

let%test_unit _ =
  let sexps =
    let open With_layout in
    [ Comment (Plain_comment ({row=0;col=2}, "; c"))
    ; Sexp (Atom ({row=0;col=6}, "not-a-comment", None))]
  in
  let str = string_of_sexps_with_layout sexps in
  [%test_result: string]
    ~expect:"  ; c\n      not-a-comment"
    str
;;

let%test_unit _ =
  let sexps =
    let open With_layout in
    [ Comment (Plain_comment ({row=0;col=2}, "; c"))
    ; Sexp (Atom ({row=2;col=6}, "not-a-comment", None))]
  in
  let str = string_of_sexps_with_layout sexps in
  [%test_result: string]
    ~expect:"  ; c\n\n      not-a-comment"
    str
;;
