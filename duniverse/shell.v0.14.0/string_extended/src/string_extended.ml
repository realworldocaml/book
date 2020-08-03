open Core_kernel
open Poly

(* Natural ordering like found in gnome nautilus, the mac finder etc...
   Refer to Mli for more documentation
*)
let collate s1 s2 =
  let pos1 = ref 0
  and pos2 = ref 0 in
  let next ~ok s pos =
    if !pos = String.length s
    then None
    else (
      let c = s.[!pos] in
      if ok c
      then (
        incr pos;
        Some c)
      else None)
  in
  let compare_non_numerical () =
    let ok c = not (Char.is_digit c) in
    let rec loop () =
      match next ~ok s1 pos1, next ~ok s2 pos2 with
      | Some _, None -> 1
      | None, Some _ -> -1
      | None, None -> 0
      | Some c1, Some c2 when c1 = c2 -> loop ()
      | Some c1, Some c2 -> Char.compare c1 c2
    in
    loop ()
  in
  let compare_numerical () =
    let rec consume0 s pos =
      match next ~ok:(( = ) '0') s pos with
      | Some _ -> consume0 s pos
      | None -> ()
    in
    (* Our main loop works on string representation of ints where all the
       trailing zeros have been chopped of. Their magnitude is given by the
       length of their representation. If they have the same magnitude the
       lexical order is correct. Bias is used to save that information.
    *)
    let ok = Char.is_digit in
    let bias = ref 0 in
    let rec loop () =
      match next ~ok s1 pos1, next ~ok s2 pos2 with
      | Some _, None -> 1
      | None, Some _ -> -1
      | None, None when !bias <> 0 -> !bias
      | None, None ->
        (* Both ints have the same value, The one with the shortest
           representation (i.e. the least trailing zeroes) is
           considered to be the smallest*)
        !pos1 - !pos2
      | Some c1, Some c2 when !bias = 0 ->
        bias := Char.compare c1 c2;
        loop ()
      | Some _, Some _ -> loop ()
    in
    consume0 s1 pos1;
    consume0 s2 pos2;
    loop ()
  in
  let s1_length = String.length s1 in
  let s2_length = String.length s2 in
  let rec loop () =
    let r = compare_non_numerical () in
    let r' = compare_numerical () in
    match r, r' with
    | 0, 0 when !pos1 = s1_length && !pos2 = s2_length -> 0
    | 0, 0 -> loop ()
    | 0, i | i, _ -> i
  in
  loop ()
;;

let%test_module "collate" =
  (module struct
    let ( <! ) s s' = collate s s' < 0

    (*
       let (>!) s s' = collate s s' > 0

       let basic_tests = (fun (s,s') ->
       "invertible" @? ((s' <! s) = (s >! s'));
       "total" @? (definitive_clause [s<!s'; s=s'; s>!s']))
    *)

    (* repeat 50 basic_tests (pg sg sg);
       repeat 2 basic_tests (dup sg);
       repeat 50 (fun (s,s',s'') ->
       let (s1,s2,s3) =
       match List.sort ~compare:String_extended.collate [s;s';s''] with
       | [s1;s2;s3] -> s1,s2,s3
       | _ -> assert false
       in
       "transitive" @?
       (((s1 <! s2) || (s2 <! s3)) = (s1 <! s3)))
       (tg sg sg sg); *)

    let%test _ = "a2b" <! "a10b"
    let%test _ = "a2b" <! "a02b"
    let%test _ = "a010b" <! "a20b"
  end)
;;

(**
   Inverse operation of [String.escaped]
*)
exception Unescape_error of bool * int * string

(* The stdlib's escaped does a lot of fancy wazoo magic to avoid
   using a buffer:
   It works in two passes, the first one calculates the length of the string to
   allocate and the second one does the actual escaping.

   This would be more cumbersome to do here but might be worth the hassle if
   performance ever gets to be an issue *)
let unescaped' ?(strict = true) s =
  let len = String.length s in
  let pos = ref 0 in
  let error ?(fatal = false) message = raise (Unescape_error (fatal, !pos, message)) in
  let consume () =
    let i = !pos in
    if i = len then error "unexpectedly reached end of string";
    let c = s.[i] in
    pos := i + 1;
    c
  in
  let res = Buffer.create len in
  let emit c = Buffer.add_char res c in
  let emit_code code =
    match Char.of_int code with
    | Some c -> emit c
    | None -> error ~fatal:true (Printf.sprintf "got invalid escape code %d" code)
  in
  let rec loop () =
    if !pos < len
    then (
      let c = consume () in
      if c <> '\\'
      then emit c
      else (
        let mark = !pos in
        try
          let c = consume () in
          match c with
          | '\\' | '\"' -> emit c
          | 'b' -> emit '\b'
          | 'n' -> emit '\n'
          | 'r' -> emit '\r'
          | 't' -> emit '\t'
          | '\n' ->
            let rec consume_blank () =
              if !pos < len
              then (
                match consume () with
                | ' ' | '\t' -> consume_blank ()
                | _ -> decr pos)
            in
            consume_blank ()
          | 'x' ->
            let c2hex c =
              if c >= 'A' && c <= 'F'
              then Char.to_int c + 10 - Char.to_int 'A'
              else if c >= 'a' && c <= 'f'
              then Char.to_int c + 10 - Char.to_int 'a'
              else if c >= '0' && c <= '9'
              then Char.to_int c - Char.to_int '0'
              else error (Printf.sprintf "expected hex digit, got: %c" c)
            in
            let c1 = consume () in
            let c2 = consume () in
            emit_code ((16 * c2hex c1) + c2hex c2)
          | c when Char.is_digit c ->
            let char_to_num c =
              match Char.get_digit c with
              | None -> error (Printf.sprintf "expected digit,got: %c" c)
              | Some i -> i
            in
            let i1 = char_to_num c in
            let i2 = char_to_num (consume ()) in
            let i3 = char_to_num (consume ()) in
            emit_code ((100 * i1) + (10 * i2) + i3)
          | c -> error (Printf.sprintf "got invalid escape character: %c" c)
        with
        | Unescape_error (false, _, _) when not strict ->
          emit '\\';
          pos := mark);
      loop ())
    else Buffer.contents res
  in
  loop ()
;;

let unescaped ?strict s =
  try unescaped' ?strict s with
  | Unescape_error (_, pos, message) ->
    invalid_argf
      "String_extended.unescaped error at position %d of %s: %s"
      pos
      s
      message
      ()
;;

let unescaped_res ?strict s =
  try Result.Ok (unescaped' ?strict s) with
  | Unescape_error (_, pos, message) -> Result.Error (pos, message)
;;

let squeeze str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec skip_spaces i =
    if i >= len
    then Buffer.contents buf
    else (
      let c = str.[i] in
      if c = ' ' || c = '\n' || c = '\t' || c = '\r'
      then skip_spaces (i + 1)
      else (
        Buffer.add_char buf c;
        copy_chars (i + 1)))
  and copy_chars i =
    if i >= len
    then Buffer.contents buf
    else (
      let c = str.[i] in
      if c = ' ' || c = '\n' || c = '\t' || c = '\r'
      then (
        Buffer.add_char buf ' ';
        skip_spaces (i + 1))
      else (
        Buffer.add_char buf c;
        copy_chars (i + 1)))
  in
  copy_chars 0
;;

let pad_right ?(char = ' ') s l =
  let src_len = String.length s in
  if src_len >= l
  then s
  else (
    let res = Bytes.create l in
    Bytes.From_string.blit ~src:s ~dst:res ~src_pos:0 ~dst_pos:0 ~len:src_len;
    Bytes.fill ~pos:src_len ~len:(l - src_len) res char;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res)
;;

let pad_left ?(char = ' ') s l =
  let src_len = String.length s in
  if src_len >= l
  then s
  else (
    let res = Bytes.create l in
    Bytes.From_string.blit ~src:s ~dst:res ~src_pos:0 ~dst_pos:(l - src_len) ~len:src_len;
    Bytes.fill ~pos:0 ~len:(l - src_len) res char;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res)
;;

let line_break ~len s =
  let buf = Buffer.create len in
  let flush_buf () =
    let res = Buffer.contents buf in
    Buffer.reset buf;
    res
  in
  let rec loop acc = function
    | [] ->
      let acc =
        if Buffer.length buf <> 0
        then flush_buf () :: acc
        else if acc = []
        then [ "" ]
        else acc
      in
      List.rev acc
    | h :: t when Buffer.length buf = 0 ->
      Buffer.add_string buf h;
      loop acc t
    | h :: t when Buffer.length buf + 1 + String.length h < len ->
      Buffer.add_char buf ' ';
      Buffer.add_string buf h;
      loop acc t
    | l -> loop (flush_buf () :: acc) l
  in
  List.concat_map (String.split ~on:'\n' s) ~f:(fun s ->
    loop [] (String.split ~on:' ' s))
;;

(* Finds out where to break a given line; returns the len of the line to break
   and the staring position of the next line.*)
let rec word_wrap__break_one ~hard_limit ~soft_limit ~previous_match s ~pos ~len =
  if pos = String.length s
  then len, pos
  else if previous_match > 0 && len >= soft_limit
  then previous_match, pos - len + previous_match + 1
  else if len >= hard_limit
  then len, pos
  else (
    match s.[pos] with
    (* Detect \r\n as one newline and not two... *)
    | '\r' when pos < String.length s - 1 && s.[pos + 1] = '\n' -> len, pos + 2
    | '\r' | '\n' -> len, pos + 1
    | ' ' | '\t' ->
      word_wrap__break_one
        s
        ~hard_limit
        ~soft_limit
        ~previous_match:len
        ~pos:(pos + 1)
        ~len:(len + 1)
    | _ ->
      word_wrap__break_one
        s
        ~previous_match
        ~hard_limit
        ~soft_limit
        ~pos:(pos + 1)
        ~len:(len + 1))
;;

(* Returns an pos*length list of all the lines (as substrings of the argument
   passed in) *)
let rec word_wrap__find_substrings ~hard_limit ~soft_limit s acc pos =
  if pos < String.length s
  then (
    let len, new_pos =
      word_wrap__break_one s ~hard_limit ~soft_limit ~previous_match:0 ~pos ~len:0
    in
    word_wrap__find_substrings ~hard_limit ~soft_limit s ((pos, len) :: acc) new_pos)
  else acc
;;

let word_wrap
      ?(trailing_nl = false)
      ?(soft_limit = 80)
      ?(hard_limit = Int.max_value)
      ?(nl = "\n")
      s
  =
  let soft_limit = min soft_limit hard_limit in
  let lines = word_wrap__find_substrings ~soft_limit ~hard_limit s [] 0 in
  match lines with
  | [] | [ _ ] -> if trailing_nl then s ^ nl else s
  | (hpos, hlen) :: t ->
    let nl_len = String.length nl in
    let body_len =
      List.fold_left t ~f:(fun acc (_, len) -> acc + nl_len + len) ~init:0
    in
    let res_len = if trailing_nl then body_len + hlen + nl_len else body_len + hlen in
    let res = Bytes.create res_len in
    if trailing_nl
    then
      Bytes.From_string.blit
        ~src:nl
        ~dst:res
        ~len:nl_len
        ~src_pos:0
        ~dst_pos:(body_len + hlen);
    Bytes.From_string.blit ~src:s ~dst:res ~len:hlen ~src_pos:hpos ~dst_pos:body_len;
    let rec blit_loop dst_end_pos = function
      | [] -> ()
      | (src_pos, len) :: rest ->
        let dst_pos = dst_end_pos - len - nl_len in
        Bytes.From_string.blit ~src:s ~dst:res ~len ~src_pos ~dst_pos;
        Bytes.From_string.blit
          ~src:nl
          ~dst:res
          ~len:nl_len
          ~src_pos:0
          ~dst_pos:(dst_pos + len);
        blit_loop dst_pos rest
    in
    blit_loop body_len t;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res
;;

let is_substring_deprecated ~substring:needle haystack =
  (* 2014-10-29 mbac: a recent release of Core introduced a fast and less surprising
     version of KMP.  Everyone should use that.  This function is simply here to maintain
     bug compatibiltiy with the original pure-ML version of f is_substring that used
     to be here. *)
  if String.length needle = 0
  then if String.length haystack = 0 then false else invalid_arg "index out of bounds"
  else Core_kernel.String.is_substring ~substring:needle haystack
;;

let%test _ = is_substring_deprecated ~substring:"foo" "foo"
let%test _ = not (is_substring_deprecated ~substring:"" "")

let%test _ =
  (* For bug compatibility with the ML version that used to be here *)
  try
    ignore (is_substring_deprecated ~substring:"" "foo");
    assert false (* should not be reachable *)
  with
  | Invalid_argument _ -> true
;;

let%test _ = not (is_substring_deprecated ~substring:"foo" "")
let%test _ = is_substring_deprecated ~substring:"bar" "foobarbaz"
let%test _ = not (is_substring_deprecated ~substring:"Z" "z")
let%test _ = not (is_substring_deprecated ~substring:"store" "video stapler")
let%test _ = not (is_substring_deprecated ~substring:"sandwich" "apple")
let%test _ = is_substring_deprecated ~substring:"z" "abc\x00z"

let edit_distance_matrix ?transpose s1 s2 =
  let transpose = Option.is_some transpose in
  let l1, l2 = String.length s1, String.length s2 in
  let d = Array.make_matrix 0 ~dimx:(l1 + 1) ~dimy:(l2 + 1) in
  for x = 0 to l1 do
    d.(x).(0) <- x
  done;
  for y = 0 to l2 do
    d.(0).(y) <- y
  done;
  for y = 1 to l2 do
    for x = 1 to l1 do
      let min_d =
        if s1.[x - 1] = s2.[y - 1]
        then d.(x - 1).(y - 1)
        else
          List.reduce_exn
            ~f:min
            [ d.(x - 1).(y) + 1; d.(x).(y - 1) + 1; d.(x - 1).(y - 1) + 1 ]
      in
      let min_d =
        if transpose
        && x > 1
        && y > 1
        && s1.[x - 1] = s2.[y - 2]
        && s1.[x - 2] = s2.[y - 1]
        then min min_d (d.(x - 2).(y - 2) + 1)
        else min_d
      in
      d.(x).(y) <- min_d
    done
  done;
  d
;;

let edit_distance ?transpose s1 s2 =
  (edit_distance_matrix ?transpose s1 s2).(String.length s1).(String.length s2)
;;

let%test _ = edit_distance "" "" = 0
let%test _ = edit_distance "stringStringString" "stringStringString" = 0
let%test _ = edit_distance "ocaml" "coaml" = 2
let%test _ = edit_distance ~transpose:() "ocaml" "coaml" = 1
let%test _ = edit_distance "sitting" "kitten" = 3
let%test _ = edit_distance ~transpose:() "sitting" "kitten" = 3
let%test _ = edit_distance "abcdef" "1234567890" = 10
let%test _ = edit_distance "foobar" "fubahr" = 3
let%test _ = edit_distance "hylomorphism" "zylomorphism" = 1
