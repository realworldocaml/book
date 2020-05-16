include Base
include Stdio
include Expect_test_helpers_kernel

let dummy_sexplib_error () =
  (* a hack to raise an arbitrary Parse_error *)
  let _ = Sexplib.Sexp.of_string ")" in
  assert false

let sexplib_sexps_of_string str : Sexp.t list =
  (* [ws_buf] must contain a single space character *)
  let feed_end_of_input ~this_parse ~ws_buf =
    (* When parsing atoms, the incremental parser cannot tell whether
       it is at the end until it hits whitespace.  We therefore feed it
       one space to determine whether it is finished. *)
    match this_parse ~pos:0 ~len:1 ws_buf with
    | Sexplib.Sexp.Done (sexp, _) -> Ok sexp
    | Cont (cont_state, _) -> Error cont_state
  in
  let of_string_bigstring this_parse ws_buf str =
    let rec loop parse_pos =
      match this_parse str ~parse_pos with
      | Sexplib.Sexp.Done (sexp, parse_pos) ->
        sexp :: loop (Some parse_pos)
      | Cont (_, this_parse) ->
        match feed_end_of_input ~this_parse ~ws_buf with
        | Ok sexp -> [sexp]
        | Error (Sexplib.Sexp.Cont_state.Parsing_toplevel_whitespace) ->
          []
        | Error _ ->
          dummy_sexplib_error ()
    in
    loop None
  in
  of_string_bigstring
    (fun x ~parse_pos -> Sexplib.Sexp.parse ?parse_pos x) " " str
;;
