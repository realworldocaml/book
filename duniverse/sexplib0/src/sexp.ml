[@@@ocaml.warning "-3"]

(* blit_string doesn't exist in [StdLabels.Bytes]...  *)
let bytes_blit_string ~src ~src_pos ~dst ~dst_pos ~len =
  Bytes.blit_string src src_pos dst dst_pos len

open StdLabels
open Format

(** Type of S-expressions *)
type t = Atom of string | List of t list

let sexp_of_t t = t
let t_of_sexp t = t
let t_sexp_grammar = Raw_grammar.Inline Any

let rec compare_list a b =
  match a, b with
  | [] , [] -> 0
  | [] , _  -> -1
  | _  , [] -> 1
  | x::xs, y::ys ->
    let res = compare x y in
    if res <> 0 then res
    else compare_list xs ys

and compare a b =
  if a == b then
    0
  else
    match a, b with
    | Atom a, Atom b -> String.compare a b
    | Atom _, _ -> -1
    | _, Atom _ -> 1
    | List a, List b -> compare_list a b

let equal a b = compare a b = 0

exception Not_found_s of t

exception Of_sexp_error of exn * t

module Printing = struct
  (* Default indentation level for human-readable conversions *)

  let default_indent = ref 1

  (* Escaping of strings used as atoms in S-expressions *)

  let must_escape str =
    let len = String.length str in
    len = 0 ||
    let rec loop str ix =
      match str.[ix] with
      | '"' | '(' | ')' | ';' | '\\' -> true
      | '|' -> ix > 0 && let next = ix - 1 in Char.equal str.[next] '#' || loop str next
      | '#' -> ix > 0 && let next = ix - 1 in Char.equal str.[next] '|' || loop str next
      | '\000' .. '\032' | '\127' .. '\255' -> true
      | _ -> ix > 0 && loop str (ix - 1)
    in
    loop str (len - 1)

  let escaped s =
    let n = ref 0 in
    for i = 0 to String.length s - 1 do
      n := !n +
           (match String.unsafe_get s i with
            | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
            | ' ' .. '~' -> 1
            | _ -> 4)
    done;
    if !n = String.length s then s else begin
      let s' = Bytes.create !n in
      n := 0;
      for i = 0 to String.length s - 1 do
        begin match String.unsafe_get s i with
        | ('\"' | '\\') as c ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n c
        | '\n' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'n'
        | '\t' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 't'
        | '\r' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'r'
        | '\b' ->
          Bytes.unsafe_set s' !n '\\'; incr n; Bytes.unsafe_set s' !n 'b'
        | (' ' .. '~') as c -> Bytes.unsafe_set s' !n c
        | c ->
          let a = Char.code c in
          Bytes.unsafe_set s' !n '\\';
          incr n;
          Bytes.unsafe_set s' !n (Char.chr (48 + a / 100));
          incr n;
          Bytes.unsafe_set s' !n (Char.chr (48 + (a / 10) mod 10));
          incr n;
          Bytes.unsafe_set s' !n (Char.chr (48 + a mod 10));
        end;
        incr n
      done;
      Bytes.unsafe_to_string s'
    end

  let esc_str str =
    let estr = escaped str in
    let elen = String.length estr in
    let res = Bytes.create (elen + 2) in
    bytes_blit_string ~src:estr ~src_pos:0 ~dst:res ~dst_pos:1 ~len:elen;
    Bytes.unsafe_set res 0          '"';
    Bytes.unsafe_set res (elen + 1) '"';
    Bytes.unsafe_to_string res

  let index_of_newline str start =
    try Some (String.index_from str start '\n')
    with Not_found -> None

  let get_substring str index end_pos_opt =
    let end_pos =
      match end_pos_opt with
      | None -> String.length str
      | Some end_pos -> end_pos
    in
    String.sub str ~pos:index ~len:(end_pos - index)

  let is_one_line str =
    match index_of_newline str 0 with
    | None -> true
    | Some index -> index + 1 = String.length str

  let pp_hum_maybe_esc_str ppf str =
    if not (must_escape str) then
      pp_print_string ppf str
    else if is_one_line str then
      pp_print_string ppf (esc_str str)
    else begin
      let rec loop index =
        let next_newline = index_of_newline str index in
        let next_line = get_substring str index next_newline in
        pp_print_string ppf (escaped next_line);
        match next_newline with
        | None ->  ()
        | Some newline_index ->
          pp_print_string ppf "\\";
          pp_force_newline ppf ();
          pp_print_string ppf "\\n";
          loop (newline_index + 1)
      in
      pp_open_box ppf 0;
      (* the leading space is to line up the lines *)
      pp_print_string ppf " \"";
      loop 0;
      pp_print_string ppf "\"";
      pp_close_box ppf ();
    end

  let mach_maybe_esc_str str =
    if must_escape str then esc_str str else str

  (* Output of S-expressions to formatters *)

  let rec pp_hum_indent indent ppf = function
    | Atom str -> pp_hum_maybe_esc_str ppf str
    | List (h :: t) ->
      pp_open_box ppf indent;
      pp_print_string ppf "(";
      pp_hum_indent indent ppf h;
      pp_hum_rest indent ppf t
    | List [] -> pp_print_string ppf "()"

  and pp_hum_rest indent ppf = function
    | h :: t ->
      pp_print_space ppf ();
      pp_hum_indent indent ppf h;
      pp_hum_rest indent ppf t
    | [] ->
      pp_print_string ppf ")";
      pp_close_box ppf ()

  let rec pp_mach_internal may_need_space ppf = function
    | Atom str ->
      let str' = mach_maybe_esc_str str in
      let new_may_need_space = str' == str in
      if may_need_space && new_may_need_space then pp_print_string ppf " ";
      pp_print_string ppf str';
      new_may_need_space
    | List (h :: t) ->
      pp_print_string ppf "(";
      let may_need_space = pp_mach_internal false ppf h in
      pp_mach_rest may_need_space ppf t;
      false
    | List [] -> pp_print_string ppf "()"; false

  and pp_mach_rest may_need_space ppf = function
    | h :: t ->
      let may_need_space = pp_mach_internal may_need_space ppf h in
      pp_mach_rest may_need_space ppf t
    | [] -> pp_print_string ppf ")"

  let pp_hum ppf sexp = pp_hum_indent !default_indent ppf sexp

  let pp_mach ppf sexp = ignore (pp_mach_internal false ppf sexp)
  let pp = pp_mach

  (* Sexp size *)

  let rec size_loop (v, c as acc) = function
    | Atom str -> v + 1, c + String.length str
    | List lst -> List.fold_left lst ~init:acc ~f:size_loop

  let size sexp = size_loop (0, 0) sexp

  (* Buffer conversions *)

  let to_buffer_hum ~buf ?(indent = !default_indent) sexp =
    let ppf = Format.formatter_of_buffer buf in
    Format.fprintf ppf "%a@?" (pp_hum_indent indent) sexp

  let to_buffer_mach ~buf sexp =
    let rec loop may_need_space = function
      | Atom str ->
        let str' = mach_maybe_esc_str str in
        let new_may_need_space = str' == str in
        if may_need_space && new_may_need_space then Buffer.add_char buf ' ';
        Buffer.add_string buf str';
        new_may_need_space
      | List (h :: t) ->
        Buffer.add_char buf '(';
        let may_need_space = loop false h in
        loop_rest may_need_space t;
        false
      | List [] -> Buffer.add_string buf "()"; false
    and loop_rest may_need_space = function
      | h :: t ->
        let may_need_space = loop may_need_space h in
        loop_rest may_need_space t
      | [] -> Buffer.add_char buf ')' in
    ignore (loop false sexp)

  let to_buffer = to_buffer_mach

  let to_buffer_gen ~buf ~add_char ~add_string sexp =
    let rec loop may_need_space = function
      | Atom str ->
        let str' = mach_maybe_esc_str str in
        let new_may_need_space = str' == str in
        if may_need_space && new_may_need_space then add_char buf ' ';
        add_string buf str';
        new_may_need_space
      | List (h :: t) ->
        add_char buf '(';
        let may_need_space = loop false h in
        loop_rest may_need_space t;
        false
      | List [] -> add_string buf "()"; false
    and loop_rest may_need_space = function
      | h :: t ->
        let may_need_space = loop may_need_space h in
        loop_rest may_need_space t
      | [] -> add_char buf ')' in
    ignore (loop false sexp)

  (* The maximum size of a thing on the minor heap is 256 words.
     Previously, this size of the returned buffer here was 4096 bytes, which
     caused the Buffer to be allocated on the *major* heap every time.

     According to a simple benchmark by Ron, we can improve performance for
     small s-expressions by a factor of ~4 if we only allocate 1024 bytes
     (128 words + some small overhead) worth of buffer initially.  And one
     can argue that if it's free to allocate strings smaller than 256 words,
     large s-expressions requiring larger expensive buffers won't notice
     the extra two doublings from 1024 bytes to 2048 and 4096. And especially
     performance-sensitive applications to always pass in a larger buffer to
     use. *)
  let buffer () = Buffer.create 1024

  (* String conversions *)

  let to_string_hum ?indent = function
    | Atom str when (match index_of_newline str 0 with None -> true | Some _ -> false) ->
      mach_maybe_esc_str str
    | sexp ->
      let buf = buffer () in
      to_buffer_hum ?indent sexp ~buf;
      Buffer.contents buf

  let to_string_mach = function
    | Atom str -> mach_maybe_esc_str str
    | sexp ->
      let buf = buffer () in
      to_buffer_mach sexp ~buf;
      Buffer.contents buf

  let to_string = to_string_mach
end
include Printing

let of_float_style : [ `Underscores | `No_underscores ] ref = ref `No_underscores
let of_int_style   : [ `Underscores | `No_underscores ] ref = ref `No_underscores

module Private = struct
  include Printing

  module Raw_grammar = struct
    include Raw_grammar

    module Builtin = struct
      let unit_sexp_grammar = Inline (List [])
      let bool_sexp_grammar = Inline (Atom Bool)
      let string_sexp_grammar = Inline (Atom String)
      let bytes_sexp_grammar = string_sexp_grammar
      let char_sexp_grammar = Inline (Atom Char)
      let int_sexp_grammar = Inline (Atom Int)
      let float_sexp_grammar = Inline (Atom Float)
      let int32_sexp_grammar = Inline (Atom Int)
      let int64_sexp_grammar = Inline (Atom Int)
      let nativeint_sexp_grammar = Inline (Atom Int)
      let ref_sexp_grammar = Inline (Explicit_bind ([ "'a" ], Explicit_var 0))
      let lazy_t_sexp_grammar = Inline (Explicit_bind ([ "'a" ], Explicit_var 0))
      let option_sexp_grammar = Inline (Explicit_bind ([ "'a" ], Option (Explicit_var 0)))

      let list_sexp_grammar =
        Inline (Explicit_bind ([ "'a" ], List [ Many (Explicit_var 0) ]))
      ;;

      let array_sexp_grammar = list_sexp_grammar
    end

    let empty_sexp_grammar = Inline (Union [])
    let opaque_sexp_grammar = empty_sexp_grammar
    let fun_sexp_grammar = empty_sexp_grammar
    let tuple2_sexp_grammar =
      Inline
        (Explicit_bind
           ([ "'a"; "'b" ], List [ One (Explicit_var 0); One (Explicit_var 1) ]))
    ;;
  end
end

let message name fields =
  let rec conv_fields = function
    | [] -> []
    | (fname, fsexp) :: rest ->
      match fname with
      | "" -> fsexp :: conv_fields rest
      | _  -> List [ Atom fname; fsexp ] :: conv_fields rest
  in
  List (Atom name :: conv_fields fields)
