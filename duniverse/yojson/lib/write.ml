(* included: type.ml *)

let hex n =
  Char.chr (
    if n < 10 then n + 48
    else n + 87
  )

let write_special src start stop ob str =
  Buffer.add_substring ob src !start (stop - !start);
  Buffer.add_string ob str;
  start := stop + 1

let write_control_char src start stop ob c =
  Buffer.add_substring ob src !start (stop - !start);
  Buffer.add_string ob "\\u00";
  Buffer.add_char ob (hex (Char.code c lsr 4));
  Buffer.add_char ob (hex (Char.code c land 0xf));
  start := stop + 1

let finish_string src start ob =
  try
    Buffer.add_substring ob src !start (String.length src - !start)
  with exc ->
    Printf.eprintf "src=%S start=%i len=%i\n%!"
      src !start (String.length src - !start);
    raise exc

let write_string_body ob s =
  let start = ref 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        '"' -> write_special s start i ob "\\\""
      | '\\' -> write_special s start i ob "\\\\"
      | '\b' -> write_special s start i ob "\\b"
      | '\012' -> write_special s start i ob "\\f"
      | '\n' -> write_special s start i ob "\\n"
      | '\r' -> write_special s start i ob "\\r"
      | '\t' -> write_special s start i ob "\\t"
      | '\x00'..'\x1F'
      | '\x7F' as c -> write_control_char s start i ob c
      | _ -> ()
  done;
  finish_string s start ob

let write_string ob s =
  Buffer.add_char ob '"';
  write_string_body ob s;
  Buffer.add_char ob '"'

let json_string_of_string s =
  let ob = Buffer.create 10 in
  write_string ob s;
  Buffer.contents ob

let test_string () =
  let s = Bytes.create 256 in
  for i = 0 to 255 do
    Bytes.set s i (Char.chr i)
  done;
  json_string_of_string (Bytes.to_string s)


let write_null ob () =
  Buffer.add_string ob "null"

let write_bool ob x =
  Buffer.add_string ob (if x then "true" else "false")


let max_digits =
  max
    (String.length (string_of_int max_int))
    (String.length (string_of_int min_int))

let dec n =
  Char.chr (n + 48)

let rec write_digits s x =
  if x = 0 then ()
  else
    let d = x mod 10 in
    write_digits s (x / 10);
    Buffer.add_char s (dec (abs d))

let write_int ob x =
  if x > 0 then
    write_digits ob x
  else if x < 0 then (
    Buffer.add_char ob '-';
    write_digits ob x
  )
  else
    Buffer.add_char ob '0'


let json_string_of_int i =
  string_of_int i


(*
  Ensure that the float is not printed as an int.
  This is not required by JSON, but useful in order to guarantee
  reversibility.
*)
let float_needs_period s =
  try
    for i = 0 to String.length s - 1 do
      match s.[i] with
          '0'..'9' | '-' -> ()
        | _ -> raise Exit
    done;
    true
  with Exit ->
    false

(*
  Guarantees that a sufficient number of digits are printed in order to allow
  reversibility.
*)
let write_float ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s =
        if float_of_string s1 = x then s1
        else Printf.sprintf "%.17g" x
      in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

let write_normal_float_prec significant_figures ob x =
  let open Printf in
  let s =
    match significant_figures with
        1 -> sprintf "%.1g" x
      | 2 -> sprintf "%.2g" x
      | 3 -> sprintf "%.3g" x
      | 4 -> sprintf "%.4g" x
      | 5 -> sprintf "%.5g" x
      | 6 -> sprintf "%.6g" x
      | 7 -> sprintf "%.7g" x
      | 8 -> sprintf "%.8g" x
      | 9 -> sprintf "%.9g" x
      | 10 -> sprintf "%.10g" x
      | 11 -> sprintf "%.11g" x
      | 12 -> sprintf "%.12g" x
      | 13 -> sprintf "%.13g" x
      | 14 -> sprintf "%.14g" x
      | 15 -> sprintf "%.15g" x
      | 16 -> sprintf "%.16g" x
      | _ -> sprintf "%.17g" x
  in
  Buffer.add_string ob s;
  if float_needs_period s then
    Buffer.add_string ob ".0"

(* used by atdgen *)
let write_float_prec significant_figures ob x =
  match classify_float x with
    FP_nan ->
      Buffer.add_string ob "NaN"
  | FP_infinite ->
      Buffer.add_string ob (if x > 0. then "Infinity" else "-Infinity")
  | _ ->
      write_normal_float_prec significant_figures ob x

let json_string_of_float x =
  let ob = Buffer.create 20 in
  write_float ob x;
  Buffer.contents ob


let write_std_float ob x =
  match classify_float x with
    FP_nan ->
      json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      let s1 = Printf.sprintf "%.16g" x in
      let s =
        if float_of_string s1 = x then s1
        else Printf.sprintf "%.17g" x
      in
      Buffer.add_string ob s;
      if float_needs_period s then
        Buffer.add_string ob ".0"

(* used by atdgen *)
let write_std_float_prec significant_figures ob x =
  match classify_float x with
    FP_nan ->
      json_error "NaN value not allowed in standard JSON"
  | FP_infinite ->
      json_error
        (if x > 0. then
           "Infinity value not allowed in standard JSON"
         else
           "-Infinity value not allowed in standard JSON")
  | _ ->
      write_normal_float_prec significant_figures ob x

let std_json_string_of_float x =
  let ob = Buffer.create 20 in
  write_std_float ob x;
  Buffer.contents ob


let test_float () =
  let l = [ 0.; 1.; -1. ] in
  let l = l @ List.map (fun x -> 2. *. x +. 1.) l in
  let l = l @ List.map (fun x -> x /. sqrt 2.) l in
  let l = l @ List.map (fun x -> x *. sqrt 3.) l in
  let l = l @ List.map cos l in
  let l = l @ List.map (fun x -> x *. 1.23e50) l in
  let l = l @ [ infinity; neg_infinity ] in
  List.iter (
    fun x ->
      let s = Printf.sprintf "%.17g" x in
      let y = float_of_string s in
      Printf.printf "%g %g %S %B\n" x y s (x = y)
  )
    l

(*
let () = test_float ()
*)

let write_intlit = Buffer.add_string
let write_floatlit = Buffer.add_string
let write_stringlit = Buffer.add_string

let rec iter2_aux f_elt f_sep x = function
    [] -> ()
  | y :: l ->
      f_sep x;
      f_elt x y;
      iter2_aux f_elt f_sep x l

let iter2 f_elt f_sep x = function
    [] -> ()
  | y :: l ->
      f_elt x y;
      iter2_aux f_elt f_sep x l

let f_sep ob =
  Buffer.add_char ob ','

let rec write_json ob (x : t) =
  match x with
      `Null -> write_null ob ()
    | `Bool b -> write_bool ob b
#ifdef INT
    | `Int i -> write_int ob i
#endif
#ifdef INTLIT
    | `Intlit s -> Buffer.add_string ob s
#endif
#ifdef FLOAT
    | `Float f -> write_float ob f
#endif
#ifdef FLOATLIT
    | `Floatlit s -> Buffer.add_string ob s
#endif
#ifdef STRING
    | `String s -> write_string ob s
#endif
#ifdef STRINGLIT
    | `Stringlit s -> Buffer.add_string ob s
#endif
    | `Assoc l -> write_assoc ob l
    | `List l -> write_list ob l
#ifdef TUPLE
    | `Tuple l -> write_tuple ob l
#endif
#ifdef VARIANT
    | `Variant (s, o) -> write_variant ob s o
#endif

and write_assoc ob l =
  let f_elt ob (s, x) =
    write_string ob s;
    Buffer.add_char ob ':';
    write_json ob x
  in
  Buffer.add_char ob '{';
  iter2 f_elt f_sep ob l;
  Buffer.add_char ob '}';

and write_list ob l =
  Buffer.add_char ob '[';
  iter2 write_json f_sep ob l;
  Buffer.add_char ob ']'

#ifdef TUPLE
and write_tuple ob l =
  Buffer.add_char ob '(';
  iter2 write_json f_sep ob l;
  Buffer.add_char ob ')'
#endif

#ifdef VARIANT
and write_variant ob s o =
  Buffer.add_char ob '<';
  write_string ob s;
  (match o with
       None -> ()
     | Some x ->
         Buffer.add_char ob ':';
         write_json ob x
  );
  Buffer.add_char ob '>'
#endif

let write_t = write_json

let rec write_std_json ob (x : t) =
  match x with
      `Null -> write_null ob ()
    | `Bool b -> write_bool ob b
#ifdef INT
    | `Int i -> write_int ob i
#endif
#ifdef INTLIT
    | `Intlit s -> Buffer.add_string ob s
#endif
#ifdef FLOAT
    | `Float f -> write_std_float ob f
#endif
#ifdef FLOATLIT
    | `Floatlit s -> Buffer.add_string ob s
#endif
#ifdef STRING
    | `String s -> write_string ob s
#endif
#ifdef STRINGLIT
    | `Stringlit s -> Buffer.add_string ob s
#endif
    | `Assoc l -> write_std_assoc ob l
    | `List l -> write_std_list ob l
#ifdef TUPLE
    | `Tuple l -> write_std_tuple ob l
#endif
#ifdef VARIANT
    | `Variant (s, o) -> write_std_variant ob s o
#endif

and write_std_assoc ob l =
  let f_elt ob (s, x) =
    write_string ob s;
    Buffer.add_char ob ':';
    write_std_json ob x
  in
  Buffer.add_char ob '{';
  iter2 f_elt f_sep ob l;
  Buffer.add_char ob '}';

and write_std_list ob l =
  Buffer.add_char ob '[';
  iter2 write_std_json f_sep ob l;
  Buffer.add_char ob ']'

and write_std_tuple ob l =
  Buffer.add_char ob '[';
  iter2 write_std_json f_sep ob l;
  Buffer.add_char ob ']'

#ifdef VARIANT
and write_std_variant ob s o =
  match o with
      None -> write_string ob s
    | Some x ->
        Buffer.add_char ob '[';
        write_string ob s;
        Buffer.add_char ob ',';
        write_std_json ob x;
        Buffer.add_char ob ']'
#endif


let to_buffer ?(suf = "") ?(std = false) ob x =
  if std then
    write_std_json ob x
  else
    write_json ob x;
  Buffer.add_string ob suf

let to_string ?buf ?(len = 256) ?(suf = "") ?std x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob ->
          Buffer.clear ob;
          ob
  in
  to_buffer ~suf ?std ob x;
  let s = Buffer.contents ob in
  Buffer.clear ob;
  s

let to_channel ?buf ?(len=4096) ?(suf = "") ?std oc x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> Buffer.clear ob; ob
  in
  to_buffer ~suf ?std ob x;
  Buffer.output_buffer oc ob;
  Buffer.clear ob

let to_output ?buf ?(len=4096) ?(suf = "") ?std out x =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> Buffer.clear ob; ob
  in
  to_buffer ~suf ?std ob x;
  out#output (Buffer.contents ob) 0 (Buffer.length ob);
  Buffer.clear ob

let to_file ?len ?std ?(suf = "\n") file x =
  let oc = open_out file in
  try
    to_channel ?len ~suf ?std oc x;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

let seq_to_buffer ?(suf = "\n") ?std ob st =
  Seq.iter (to_buffer ~suf ?std ob) st

let seq_to_string ?buf ?(len = 256) ?(suf = "\n") ?std st =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob ->
          Buffer.clear ob;
          ob
  in
  seq_to_buffer ~suf ?std ob st;
  let s = Buffer.contents ob in
  Buffer.clear ob;
  s

let seq_to_channel ?buf ?(len=2096) ?(suf = "\n") ?std oc seq =
  let ob =
    match buf with
        None -> Buffer.create len
      | Some ob -> Buffer.clear ob; ob
  in
  Seq.iter (fun json ->
    to_buffer ~suf ?std ob json;
    Buffer.output_buffer oc ob;
    Buffer.clear ob;
  ) seq

let seq_to_file ?len ?(suf = "\n") ?std file st =
  let oc = open_out file in
  try
    seq_to_channel ?len ~suf ?std oc st;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e


let rec sort = function
  | `Assoc l ->
      let l = List.rev (List.rev_map (fun (k, v) -> (k, sort v)) l) in
      `Assoc (List.stable_sort (fun (a, _) (b, _) -> String.compare a b) l)
  | `List l ->
      `List (List.rev (List.rev_map sort l))
#ifdef TUPLE
  | `Tuple l ->
      `Tuple (List.rev (List.rev_map sort l))
#endif
#ifdef VARIANT
  | `Variant (k, Some v) as x ->
      let v' = sort v in
      if v == v' then x
      else
        `Variant (k, Some v')
#endif
  | x -> x
