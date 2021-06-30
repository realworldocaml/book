(* Sexp: Module for handling S-expressions (I/O, etc.) *)

open Format
open Bigarray
module Sexplib = Sexplib0
module Conv = Sexplib.Sexp_conv

(* conv.ml depends on us so we can only use this module *)

include Type

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

include (
  Sexplib.Sexp :
    module type of struct
    include Sexplib.Sexp
  end
  with type t := t)

include Private

(* Output of S-expressions to I/O-channels *)

let with_new_buffer oc f =
  let buf = buffer () in
  f buf;
  Buffer.output_buffer oc buf
;;

let output_hum oc sexp = with_new_buffer oc (fun buf -> to_buffer_hum sexp ~buf)

let output_hum_indent indent oc sexp =
  with_new_buffer oc (fun buf -> to_buffer_hum ~indent sexp ~buf)
;;

let output_mach oc sexp = with_new_buffer oc (fun buf -> to_buffer_mach sexp ~buf)
let output = output_mach

(* Output of S-expressions to file *)

(* The temp file functions in the OCaml Filename module do not support
   permissions.  But opening a file with given permissions is different
   from opening it and chmoding it to these permissions, because the umask
   is taken in account.  Under Unix there's no easy way to get the umask in
   a thread-safe way. *)
module Tmp_file = struct
  let prng = ref None

  let temp_file_name prefix suffix =
    let rand_state =
      match !prng with
      | Some v -> v
      | None ->
        let ret = Random.State.make_self_init () in
        prng := Some ret;
        ret
    in
    let rnd = Random.State.bits rand_state land 0xFFFFFF in
    Printf.sprintf "%s%06x%s" prefix rnd suffix
  ;;

  (* Keep the permissions loose. Sexps are usually shared and rarely private*)
  let open_temp_file ?(perm = 0o666) prefix suffix =
    let rec try_name counter =
      let name = temp_file_name prefix suffix in
      try
        let oc =
          open_out_gen [ Open_wronly; Open_creat; Open_excl; Open_text ] perm name
        in
        name, oc
      with
      | Sys_error _ as e -> if counter >= 1000 then raise e else try_name (counter + 1)
    in
    try_name 0
  ;;
end

let save_of_output ?perm output_function file sexp =
  let tmp_name, oc = Tmp_file.open_temp_file ?perm file "tmp" in
  (try
     output_function oc sexp;
     close_out oc
   with
   | e ->
     close_out_noerr oc;
     (try Sys.remove tmp_name with
      | _ -> ());
     raise e);
  Sys.rename tmp_name file
;;

let output_sexp_nl do_output oc sexp =
  do_output oc sexp;
  output_string oc "\n"
;;

let save_hum ?perm file sexp = save_of_output ?perm (output_sexp_nl output_hum) file sexp
let save_mach ?perm file sexp = save_of_output ?perm output_mach file sexp
let save = save_mach
let output_sexps_nl do_output oc sexps = List.iter (output_sexp_nl do_output oc) sexps

let save_sexps_hum ?perm file sexps =
  save_of_output ?perm (output_sexps_nl output_hum) file sexps
;;

let save_sexps_mach ?perm file sexps =
  save_of_output ?perm (output_sexps_nl output_mach) file sexps
;;

let save_sexps = save_sexps_mach

(* Scan functions *)

let scan_sexp ?buf lexbuf = Parser.sexp (Lexer.main ?buf) lexbuf
let scan_sexp_opt ?buf lexbuf = Parser.sexp_opt (Lexer.main ?buf) lexbuf
let scan_sexps ?buf lexbuf = Parser.sexps (Lexer.main ?buf) lexbuf
let scan_rev_sexps ?buf lexbuf = Parser.rev_sexps (Lexer.main ?buf) lexbuf

let get_main_buf buf =
  let buf =
    match buf with
    | None -> Buffer.create 128
    | Some buf -> buf
  in
  Lexer.main ~buf
;;

let scan_fold_sexps ?buf ~f ~init lexbuf =
  let main = get_main_buf buf in
  let rec loop acc =
    match Parser.sexp_opt main lexbuf with
    | None -> acc
    | Some sexp -> loop (f acc sexp)
  in
  loop init
;;

let scan_iter_sexps ?buf ~f lexbuf =
  scan_fold_sexps ?buf lexbuf ~init:() ~f:(fun () sexp -> f sexp)
;;

let scan_sexps_conv ?buf ~f lexbuf =
  let coll acc sexp = f sexp :: acc in
  List.rev (scan_fold_sexps ?buf ~f:coll ~init:[] lexbuf)
;;

let sexp_conversion_error_message ?containing_sexp ?location ?invalid_sexp () ~exn : t =
  List
    (List.concat
       [ [ Atom "Of_sexp_error" ]
       ; (match location with
          | None -> []
          | Some x -> [ Atom x ])
       ; [ (match exn with
             | Failure x -> Atom x
             | _ -> Conv.sexp_of_exn exn)
         ]
       ; (match invalid_sexp with
          | None -> []
          | Some x -> [ List [ Atom "invalid_sexp"; x ] ])
       ; (match containing_sexp with
          | None -> []
          | Some x -> [ List [ Atom "containing_sexp"; x ] ])
       ])
;;

(* Partial parsing *)

module Annot = struct
  type pos = Parsexp.Positions.pos =
    { line : int
    ; col : int
    ; offset : int
    }

  type range = Parsexp.Positions.range =
    { start_pos : pos
    ; end_pos : pos
    }

  type t =
    | Atom of range * Type.t
    | List of range * t list * Type.t

  type 'a conv =
    [ `Result of 'a
    | `Error of exn * t
    ]

  exception Conv_exn of string * exn

  let () =
    Conv.Exn_converter.add ~finalise:false [%extension_constructor Conv_exn] (function
      | Conv_exn (location, exn) -> sexp_conversion_error_message () ~location ~exn
      | _ -> assert false)
  ;;

  type stack =
    { mutable positions : pos list
    ; mutable stack : t list list
    }

  let get_sexp = function
    | Atom (_, sexp) | List (_, _, sexp) -> sexp
  ;;

  let get_range = function
    | Atom (range, _) | List (range, _, _) -> range
  ;;

  let sexp_of_conv sexp_of_a = function
    | `Result a -> Type.List [ Atom "Result"; a |> sexp_of_a ]
    | `Error (exn, t) ->
      List [ Atom "Error"; List [ exn |> Conv.sexp_of_exn; t |> get_sexp ] ]
  ;;

  exception Annot_sexp of t

  let find_sexp annot_sexp sexp =
    let rec loop annot_sexp =
      match annot_sexp with
      | (Atom (_, sub_sexp) | List (_, _, sub_sexp)) when sexp == sub_sexp ->
        raise (Annot_sexp annot_sexp)
      | List (_, annots, _) -> List.iter loop annots
      | Atom _ -> ()
    in
    try
      loop annot_sexp;
      None
    with
    | Annot_sexp res -> Some res
  ;;
end

let () =
  Conv.Exn_converter.add ~finalise:false [%extension_constructor Of_sexp_error] (function
    | Of_sexp_error (Annot.Conv_exn (location, exn), invalid_sexp) ->
      sexp_conversion_error_message () ~location ~invalid_sexp ~exn
    | Of_sexp_error (exn, invalid_sexp) ->
      sexp_conversion_error_message () ~invalid_sexp ~exn
    | _ ->
      (* Reaching this branch indicates a bug in sexplib. *)
      assert false)
;;

module Parse_pos = struct
  type t =
    { mutable text_line : int
    ; mutable text_char : int
    ; mutable global_offset : int
    ; mutable buf_pos : int
    }

  let create ?(text_line = 1) ?(text_char = 0) ?(buf_pos = 0) ?(global_offset = 0) () =
    let fail msg = failwith ("Sexplib.Sexp.Parse_pos.create: " ^ msg) in
    if text_line < 1
    then fail "text_line < 1"
    else if text_char < 0
    then fail "text_char < 0"
    else if global_offset < 0
    then fail "global_offset < 0"
    else if buf_pos < 0
    then fail "buf_pos < 0"
    else { text_line; text_char; global_offset; buf_pos }
  ;;

  let with_buf_pos t buf_pos = { t with buf_pos }
end

module Cont_state = Parsexp.Old_parser_cont_state

type ('a, 't) parse_result =
  | Done of 't * Parse_pos.t
  | Cont of Cont_state.t * ('a, 't) parse_fun

and ('a, 't) parse_fun = pos:int -> len:int -> 'a -> ('a, 't) parse_result

type 't parse_state = { parse_pos : Parse_pos.t }

type parse_error =
  { err_msg : string
  ; parse_state : [ `Sexp of t list list parse_state | `Annot of Annot.stack parse_state ]
  }

exception Parse_error of parse_error

let () =
  Conv.Exn_converter.add ~finalise:false [%extension_constructor Parse_error] (function
    | Parse_error pe ->
      let ppos =
        match pe.parse_state with
        | `Sexp { parse_pos } | `Annot { parse_pos } -> parse_pos
      in
      List
        [ Atom "Sexplib.Sexp.Parse_error"
        ; List
            [ List [ Atom "err_msg"; Atom pe.err_msg ]
            ; List [ Atom "text_line"; Conv.sexp_of_int ppos.Parse_pos.text_line ]
            ; List [ Atom "text_char"; Conv.sexp_of_int ppos.Parse_pos.text_char ]
            ; List
                [ Atom "global_offset"; Conv.sexp_of_int ppos.Parse_pos.global_offset ]
            ; List [ Atom "buf_pos"; Conv.sexp_of_int ppos.Parse_pos.buf_pos ]
            ]
        ]
    | _ -> assert false)
;;

module Parser_output : sig
  module type T = sig
    module Impl : Parsexp.Eager_parser

    type output

    exception Found of output

    val raise_found : Impl.State.Read_only.t -> Impl.parsed_value -> unit
  end

  module Bare_sexp : T with type output = Type.t
  module Annotated_sexp : T with type output = Annot.t
end = struct
  module type T = sig
    module Impl : Parsexp.Eager_parser

    type output

    exception Found of output

    val raise_found : Impl.State.Read_only.t -> Impl.parsed_value -> unit
  end

  module I = Parsexp.Positions.Iterator

  let rec annotate_sexp sexp iter =
    match sexp with
    | Type.Atom _ ->
      let start_pos = I.advance_exn iter ~skip:0 in
      let end_pos = I.advance_exn iter ~skip:0 in
      Annot.Atom ({ start_pos; end_pos }, sexp)
    | Type.List l ->
      let start_pos = I.advance_exn iter ~skip:0 in
      let annot = annotate_sexp_list l iter in
      let end_pos = I.advance_exn iter ~skip:0 in
      Annot.List ({ start_pos; end_pos }, annot, sexp)

  and annotate_sexp_list sexps iter =
    List.rev (List.rev_map (fun sexp -> annotate_sexp sexp iter) sexps)
  ;;

  module Bare_sexp = struct
    module Impl = Parsexp.Eager

    type output = Type.t

    exception Found of output

    let raise_found _state sexp = raise_notrace (Found sexp)
  end

  module Annotated_sexp = struct
    module Impl = Parsexp.Eager_and_positions

    type output = Annot.t

    exception Found of output

    let raise_found _state (sexp, positions) =
      let annot = annotate_sexp sexp (I.create positions) in
      raise_notrace (Found annot)
    ;;
  end
end

module Make_parser (T : sig
    include Parser_output.T

    type input

    val length : input -> int

    val unsafe_feed_loop
      :  Impl.State.t
      -> Impl.Stack.t
      -> input
      -> max_pos:int
      -> pos:int
      -> Impl.Stack.t
  end) : sig
  val parse
    :  ?parse_pos:Parse_pos.t
    -> ?len:int
    -> T.input
    -> (T.input, T.output) parse_result
end = struct
  let parse_pos_of_state state buf_pos =
    { Parse_pos.text_line = T.Impl.State.line state
    ; Parse_pos.text_char = T.Impl.State.column state
    ; Parse_pos.global_offset = T.Impl.State.offset state
    ; Parse_pos.buf_pos
    }
  ;;

  let check_str_bounds ~pos ~len str =
    if pos < 0 then invalid_arg "parse: pos < 0";
    if len < 0 then invalid_arg "parse: len < 0";
    let str_len = T.length str in
    let pos_len = pos + len in
    if pos_len > str_len then invalid_arg "parse: pos + len > str_len";
    pos_len - 1
  ;;

  let raise_parse_error state pos msg =
    let parse_state = { parse_pos = parse_pos_of_state state pos } in
    let parse_error = { err_msg = msg; parse_state = `Sexp parse_state } in
    raise (Parse_error parse_error)
  ;;

  let handle_parsexp_error state pos e =
    let msg = Parsexp.Parse_error.message e in
    match Parsexp.Parse_error.Private.old_parser_exn e with
    | `Parse_error -> raise_parse_error state pos msg
    | `Failure -> failwith msg
  ;;

  let rec run_feed_loop state stack ~pos ~len str =
    let max_pos = check_str_bounds ~pos ~len str in
    let previous_offset = T.Impl.State.offset state in
    match T.unsafe_feed_loop state stack str ~max_pos ~pos with
    | stack -> mk_cont_state state stack
    | exception T.Found result ->
      let offset = T.Impl.State.offset state in
      let next_pos = pos + (offset - previous_offset) in
      Done (result, parse_pos_of_state state next_pos)
    | exception Parsexp.Parse_error.Parse_error err ->
      handle_parsexp_error
        state
        (pos + (T.Impl.State.offset state - previous_offset))
        err

  and mk_cont_state state stack =
    let parse_fun =
      let used_ref = ref false in
      fun ~pos ~len str ->
        if !used_ref
        then failwith "Sexplib.Sexp: parser continuation called twice"
        else (
          used_ref := true;
          run_feed_loop state stack ~pos ~len str)
    in
    let cont_state = T.Impl.State.old_parser_cont_state state in
    Cont (cont_state, parse_fun)
  ;;

  let parse ?(parse_pos = Parse_pos.create ()) ?len str =
    let pos, buf_pos =
      let { Parse_pos.text_line; text_char; global_offset; buf_pos } = parse_pos in
      ( { Parsexp.Positions.line = text_line; col = text_char; offset = global_offset }
      , buf_pos )
    in
    let state = T.Impl.State.create ~pos ~no_sexp_is_error:false T.raise_found in
    let stack = T.Impl.Stack.empty in
    let len =
      match len with
      | Some x -> x
      | None -> T.length str - buf_pos
    in
    run_feed_loop state stack str ~pos:buf_pos ~len
  ;;
end
[@@inline always]

module String_single_sexp = Make_parser (struct
    include Parser_output.Bare_sexp

    type input = string

    let length = String.length

    let rec unsafe_feed_loop state stack str ~max_pos ~pos =
      if pos <= max_pos
      then (
        let stack = Impl.feed state (String.unsafe_get str pos) stack in
        unsafe_feed_loop state stack str ~max_pos ~pos:(pos + 1))
      else stack
    ;;
  end)

let parse_str = String_single_sexp.parse
let parse = String_single_sexp.parse

module String_single_annot = Make_parser (struct
    include Parser_output.Annotated_sexp

    type input = string

    let length = String.length

    let rec unsafe_feed_loop state stack str ~max_pos ~pos =
      if pos <= max_pos
      then (
        let stack = Impl.feed state (String.unsafe_get str pos) stack in
        unsafe_feed_loop state stack str ~max_pos ~pos:(pos + 1))
      else stack
    ;;
  end)

let parse_str_annot = String_single_annot.parse

module Bigstring_single_sexp = Make_parser (struct
    include Parser_output.Bare_sexp

    type input = bigstring

    let length = Array1.dim

    let rec unsafe_feed_loop state stack (str : input) ~max_pos ~pos =
      if pos <= max_pos
      then (
        let stack = Impl.feed state (Array1.unsafe_get str pos) stack in
        unsafe_feed_loop state stack str ~max_pos ~pos:(pos + 1))
      else stack
    ;;
  end)

let parse_bigstring = Bigstring_single_sexp.parse

module Bigstring_single_annot = Make_parser (struct
    include Parser_output.Annotated_sexp

    type input = bigstring

    let length = Array1.dim

    let rec unsafe_feed_loop state stack (str : input) ~max_pos ~pos =
      if pos <= max_pos
      then (
        let stack = Impl.feed state (Array1.unsafe_get str pos) stack in
        unsafe_feed_loop state stack str ~max_pos ~pos:(pos + 1))
      else stack
    ;;
  end)

let parse_bigstring_annot = Bigstring_single_annot.parse

(* Input functions *)

let mk_this_parse ?parse_pos my_parse =
  ();
  fun ~pos ~len str ->
    let parse_pos =
      match parse_pos with
      | None -> Parse_pos.create ~buf_pos:pos ()
      | Some parse_pos ->
        parse_pos.Parse_pos.buf_pos <- pos;
        parse_pos
    in
    my_parse ?parse_pos:(Some parse_pos) ?len:(Some len) str
;;

(* [ws_buf] must contain a single space character *)
let feed_end_of_input ~this_parse ~ws_buf =
  (* When parsing atoms, the incremental parser cannot tell whether
     it is at the end until it hits whitespace.  We therefore feed it
     one space to determine whether it is finished. *)
  match this_parse ~pos:0 ~len:1 ws_buf with
  | Done (sexp, _) -> Ok sexp
  | Cont (cont_state, _) -> Error cont_state
;;

let gen_input_sexp my_parse ?parse_pos ic =
  let buf = Bytes.create 1 in
  let rec loop this_parse =
    match input_char ic with
    | exception End_of_file ->
      (match feed_end_of_input ~this_parse ~ws_buf:" " with
       | Ok sexp -> sexp
       | Error _ -> raise End_of_file)
    | c ->
      Bytes.set buf 0 c;
      (match this_parse ~pos:0 ~len:1 (Bytes.unsafe_to_string buf) with
       | Done (sexp, _) -> sexp
       | Cont (_, this_parse) -> loop this_parse)
  in
  loop (mk_this_parse ?parse_pos my_parse)
;;

let input_sexp ?parse_pos ic = gen_input_sexp parse ?parse_pos ic

let gen_input_rev_sexps my_parse ~ws_buf ?parse_pos ?(buf = Bytes.create 8192) ic =
  let rev_sexps_ref = ref [] in
  let buf_len = Bytes.length buf in
  let rec loop this_parse ~pos ~len =
    if len > 0
    then (
      match this_parse ~pos ~len (Bytes.unsafe_to_string buf) with
      | Done (sexp, ({ Parse_pos.buf_pos; _ } as parse_pos)) ->
        rev_sexps_ref := sexp :: !rev_sexps_ref;
        let n_parsed = buf_pos - pos in
        let this_parse = mk_this_parse ~parse_pos my_parse in
        if n_parsed = len
        then (
          let new_len = input ic buf 0 buf_len in
          loop this_parse ~pos:0 ~len:new_len)
        else loop this_parse ~pos:buf_pos ~len:(len - n_parsed)
      | Cont (_, this_parse) -> loop this_parse ~pos:0 ~len:(input ic buf 0 buf_len))
    else (
      match feed_end_of_input ~this_parse ~ws_buf with
      | Ok sexp -> sexp :: !rev_sexps_ref
      | Error Parsing_toplevel_whitespace -> !rev_sexps_ref
      | Error cont_state ->
        failwith
          ("Sexplib.Sexp.input_rev_sexps: reached EOF while in state "
           ^ Cont_state.to_string cont_state))
  in
  let len = input ic buf 0 buf_len in
  let this_parse = mk_this_parse ?parse_pos my_parse in
  loop this_parse ~pos:0 ~len
;;

let input_rev_sexps ?parse_pos ?buf ic =
  gen_input_rev_sexps parse ~ws_buf:" " ?parse_pos ?buf ic
;;

let input_sexps ?parse_pos ?buf ic = List.rev (input_rev_sexps ?parse_pos ?buf ic)

(* of_string and of_bigstring *)

let of_string_bigstring loc my_parse ws_buf get_len get_sub str =
  match my_parse ?parse_pos:None ?len:None str with
  | Done (sexp, parse_pos) ->
    (match my_parse ?parse_pos:(Some parse_pos) ?len:None str with
     | Done (_sexp2, _) ->
       failwith
         (sprintf
            "Sexplib.Sexp.%s: got multiple S-expressions where only one was expected."
            loc)
     | Cont (Cont_state.Parsing_toplevel_whitespace, _) -> sexp
     | Cont (_, _) ->
       (* not using [feed_end_of_input] here means "a b" will end up here and not in
          "multiple S-expressions" branch, but it doesn't matter that much *)
       failwith
         (sprintf
            "Sexplib.Sexp.%s: S-expression followed by data at position %d..."
            loc
            parse_pos.buf_pos))
  | Cont (_, this_parse) ->
    (match feed_end_of_input ~this_parse ~ws_buf with
     | Ok sexp -> sexp
     | Error cont_state ->
       let cont_state_str = Cont_state.to_string cont_state in
       failwith
         (sprintf
            "Sexplib.Sexp.%s: incomplete S-expression while in state %s: %s"
            loc
            cont_state_str
            (get_sub str 0 (get_len str))))
;;

let of_string str =
  of_string_bigstring "of_string" parse " " String.length String.sub str
;;

let get_bstr_sub_str bstr pos len =
  let str = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set str i bstr.{pos + i}
  done;
  Bytes.unsafe_to_string str
;;

let bstr_ws_buf = Array1.create char c_layout 1
let () = bstr_ws_buf.{0} <- ' '

let of_bigstring bstr =
  of_string_bigstring
    "of_bigstring"
    parse_bigstring
    bstr_ws_buf
    Array1.dim
    get_bstr_sub_str
    bstr
;;

(* Loading *)

let gen_load_rev_sexps input_rev_sexps ?buf file =
  let ic = open_in file in
  try
    let sexps = input_rev_sexps ?parse_pos:None ?buf ic in
    close_in ic;
    sexps
  with
  | exc ->
    close_in_noerr ic;
    raise exc
;;

let load_rev_sexps ?buf file = gen_load_rev_sexps input_rev_sexps ?buf file
let load_sexps ?buf file = List.rev (load_rev_sexps ?buf file)
let gen_load_sexp_loc = "Sexplib.Sexp.gen_load_sexp"

let gen_load_sexp my_parse ?(strict = true) ?(buf = Bytes.create 8192) file =
  let buf_len = Bytes.length buf in
  let ic = open_in file in
  let rec loop this_parse =
    let len = input ic buf 0 buf_len in
    if len = 0
    then (
      match feed_end_of_input ~this_parse ~ws_buf:" " with
      | Ok sexp -> sexp
      | Error cont_state ->
        failwith
          (sprintf
             "%s: EOF in %s while in state %s"
             gen_load_sexp_loc
             file
             (Cont_state.to_string cont_state)))
    else (
      match this_parse ~pos:0 ~len (Bytes.unsafe_to_string buf) with
      | Done (sexp, ({ Parse_pos.buf_pos; _ } as parse_pos)) when strict ->
        let rec strict_loop this_parse ~pos ~len =
          match this_parse ~pos ~len (Bytes.unsafe_to_string buf) with
          | Done _ ->
            failwith
              (sprintf "%s: more than one S-expression in file %s" gen_load_sexp_loc file)
          | Cont (cont_state, this_parse) ->
            let len = input ic buf 0 buf_len in
            if len > 0
            then strict_loop this_parse ~pos:0 ~len
            else if cont_state = Cont_state.Parsing_toplevel_whitespace
            then sexp
            else
              failwith
                (sprintf
                   "%s: %s in state %s loading file %s"
                   gen_load_sexp_loc
                   "additional incomplete data"
                   (Cont_state.to_string cont_state)
                   file)
        in
        let this_parse = mk_this_parse ~parse_pos my_parse in
        strict_loop this_parse ~pos:buf_pos ~len:(len - buf_pos)
      | Done (sexp, _) -> sexp
      | Cont (_, this_parse) -> loop this_parse)
  in
  try
    let sexp = loop (mk_this_parse my_parse) in
    close_in ic;
    sexp
  with
  | exc ->
    close_in_noerr ic;
    raise exc
;;

let load_sexp ?strict ?buf file = gen_load_sexp parse ?strict ?buf file

module Annotated = struct
  include Annot

  let parse = parse_str_annot
  let parse_bigstring = parse_bigstring_annot

  let input_rev_sexps ?parse_pos ?buf ic =
    gen_input_rev_sexps parse ~ws_buf:" " ?parse_pos ?buf ic
  ;;

  let input_sexp ?parse_pos ic = gen_input_sexp parse ?parse_pos ic
  let input_sexps ?parse_pos ?buf ic = List.rev (input_rev_sexps ?parse_pos ?buf ic)

  let of_string str =
    of_string_bigstring "Annotated.of_string" parse " " String.length String.sub str
  ;;

  let of_bigstring bstr =
    of_string_bigstring
      "Annotated.of_bigstring"
      parse_bigstring
      bstr_ws_buf
      Array1.dim
      get_bstr_sub_str
      bstr
  ;;

  let load_rev_sexps ?buf file = gen_load_rev_sexps input_rev_sexps ?buf file
  let load_sexps ?buf file = List.rev (load_rev_sexps ?buf file)
  let load_sexp ?strict ?buf file = gen_load_sexp parse ?strict ?buf file

  let conv f annot_sexp =
    let sexp = get_sexp annot_sexp in
    try `Result (f sexp) with
    | Of_sexp_error (exc, bad_sexp) as e ->
      (match find_sexp annot_sexp bad_sexp with
       | None -> raise e
       | Some bad_annot_sexp -> `Error (exc, bad_annot_sexp))
  ;;

  let get_conv_exn ~file ~exc annot_sexp =
    let range = get_range annot_sexp in
    let { start_pos = { line; col; offset = _ }; end_pos = _ } = range in
    let loc = sprintf "%s:%d:%d" file line col in
    Of_sexp_error (Annot.Conv_exn (loc, exc), get_sexp annot_sexp)
  ;;
end

let load_sexp_conv ?(strict = true) ?(buf = Bytes.create 8192) file f =
  let sexp = load_sexp ~strict ~buf file in
  try `Result (f sexp) with
  | Of_sexp_error _ -> Annotated.conv f (Annotated.load_sexp ~strict ~buf file)
;;

let raise_conv_exn ~file = function
  | `Result res -> res
  | `Error (exc, annot_sexp) -> raise (Annotated.get_conv_exn ~file ~exc annot_sexp)
;;

let load_sexp_conv_exn ?strict ?buf file f =
  raise_conv_exn ~file (load_sexp_conv ?strict ?buf file f)
;;

let load_sexps_conv ?(buf = Bytes.create 8192) file f =
  let rev_sexps = load_rev_sexps ~buf file in
  try List.rev_map (fun sexp -> `Result (f sexp)) rev_sexps with
  | Of_sexp_error _ as e ->
    (match Annotated.load_rev_sexps ~buf file with
     | [] ->
       (* File is now empty - perhaps it was a temporary file handle? *)
       raise e
     | rev_annot_sexps ->
       List.rev_map (fun annot_sexp -> Annotated.conv f annot_sexp) rev_annot_sexps)
;;

let load_sexps_conv_exn ?(buf = Bytes.create 8192) file f =
  let rev_sexps = load_rev_sexps ~buf file in
  try List.rev_map f rev_sexps with
  | Of_sexp_error _ as e ->
    (match Annotated.load_rev_sexps ~buf file with
     | [] ->
       (* File is now empty - perhaps it was a temporary file handle? *)
       raise e
     | rev_annot_sexps ->
       List.rev_map
         (fun annot_sexp -> raise_conv_exn ~file (Annotated.conv f annot_sexp))
         rev_annot_sexps)
;;

let gen_of_string_conv of_string annot_of_string str f =
  let sexp = of_string str in
  try `Result (f sexp) with
  | Of_sexp_error _ -> Annotated.conv f (annot_of_string str)
;;

let of_string_conv str f = gen_of_string_conv of_string Annotated.of_string str f

let of_bigstring_conv bstr f =
  gen_of_string_conv of_bigstring Annotated.of_bigstring bstr f
;;

module Of_string_conv_exn = struct
  type t =
    { exc : exn
    ; sexp : Type.t
    ; sub_sexp : Type.t
    }

  exception E of t

  let () =
    Conv.Exn_converter.add ~finalise:false [%extension_constructor E] (function
      | E osce ->
        sexp_conversion_error_message
          ()
          ~invalid_sexp:osce.sub_sexp
          ~exn:osce.exc
          ~containing_sexp:osce.sexp
      | _ -> assert false)
  ;;
end

let gen_of_string_conv_exn of_string str f =
  let sexp = of_string str in
  try f sexp with
  | Of_sexp_error (exc, sub_sexp) ->
    raise (Of_string_conv_exn.E { Of_string_conv_exn.exc; sexp; sub_sexp })
;;

let of_string_conv_exn str f = gen_of_string_conv_exn of_string str f
let of_bigstring_conv_exn bstr f = gen_of_string_conv_exn of_bigstring bstr f

(* Utilities for automated type conversions *)

let unit = List []

let is_unit = function
  | List [] -> true
  | _ -> false
;;

external sexp_of_t : t -> t = "%identity"
external t_of_sexp : t -> t = "%identity"

(* Utilities for conversion error handling *)

type found =
  [ `Found
  | `Pos of int * found
  ]

type search_result =
  [ `Not_found
  | found
  ]

let rec search_physical sexp ~contained =
  if sexp == contained
  then `Found
  else (
    match sexp with
    | Atom _ -> `Not_found
    | List lst ->
      let rec loop i = function
        | [] -> `Not_found
        | h :: t ->
          let res = search_physical h ~contained in
          (match res with
           | `Not_found -> loop (i + 1) t
           | #found as found -> `Pos (i, found))
      in
      loop 0 lst)
;;

let rec subst_found sexp ~subst = function
  | `Found -> subst
  | `Pos (pos, found) ->
    (match sexp with
     | Atom _ -> failwith "Sexplib.Sexp.subst_found: atom when position requested"
     | List lst ->
       let rec loop acc pos = function
         | [] -> failwith "Sexplib.Sexp.subst_found: short list when position requested"
         | h :: t when pos <> 0 -> loop (h :: acc) (pos - 1) t
         | h :: t -> List (List.rev_append acc (subst_found h ~subst found :: t))
       in
       loop [] pos lst)
;;
