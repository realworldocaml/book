open Sexplib

let () =
  Printexc.register_printer (function
    | Sexp.Parse_error { Sexp.err_msg; parse_state = _ } ->
      Some (Printf.sprintf "Sexp.parse_error {err_msg = %S}" err_msg)
    | _ -> None)
;;

let wrap_in_context ?(no_following_sibling = false) () =
  [ (fun a -> a)
  ; (fun a -> "(" ^ a ^ ")")
  ; (* checking if the spacing around braces changes something *)
    (fun a -> "( " ^ a ^ ")")
  ; (fun a -> "(" ^ a ^ " )")
  ; (fun a -> "( " ^ a ^ " )")
  ; (fun a -> "( ( ( " ^ a ^ "  ) ) )")
  ]
  @
  if no_following_sibling
  then []
  else
    [ (fun a -> "( something " ^ a ^ "\"something else\")")
    ; (fun a -> "( \"something else\"" ^ a ^ " something )")
    ; (fun a -> "((\"something else\")" ^ a ^ "(something))")
    ]
;;

let newline_adapters =
  [ (fun s -> s), "unix"
  ; ( (fun s ->
        String.concat
          ""
          (List.map
             (function
               | '\n' -> "\r\n"
               | c -> String.make 1 c)
             (Base.String.to_list s)))
    , "windows" )
  ]
;;
