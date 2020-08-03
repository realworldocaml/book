open! Import
module Int = Int0
module Sys = Sys0

let convert_failure x a b to_string =
  Printf.failwithf
    "conversion from %s to %s failed: %s is out of range"
    a
    b
    (to_string x)
    ()
[@@cold] [@@inline never] [@@local never] [@@specialise never]
;;

let num_bits_int = Sys.int_size_in_bits
let num_bits_int32 = 32
let num_bits_int64 = 64
let num_bits_nativeint = Word_size.num_bits Word_size.word_size
let () = assert (num_bits_int = 63 || num_bits_int = 31 || num_bits_int = 32)
let min_int32 = Caml.Int32.min_int
let max_int32 = Caml.Int32.max_int
let min_int64 = Caml.Int64.min_int
let max_int64 = Caml.Int64.max_int
let min_nativeint = Caml.Nativeint.min_int
let max_nativeint = Caml.Nativeint.max_int
let int_to_string = Caml.string_of_int
let int32_to_string = Caml.Int32.to_string
let int64_to_string = Caml.Int64.to_string
let nativeint_to_string = Caml.Nativeint.to_string

(* int <-> int32 *)

let int_to_int32_failure x = convert_failure x "int" "int32" int_to_string
let int32_to_int_failure x = convert_failure x "int32" "int" int32_to_string
let int32_to_int_trunc = Caml.Int32.to_int
let int_to_int32_trunc = Caml.Int32.of_int

let int_is_representable_as_int32 =
  if num_bits_int <= num_bits_int32
  then fun _ -> true
  else (
    let min = int32_to_int_trunc min_int32 in
    let max = int32_to_int_trunc max_int32 in
    fun x -> compare_int min x <= 0 && compare_int x max <= 0)
;;

let int32_is_representable_as_int =
  if num_bits_int32 <= num_bits_int
  then fun _ -> true
  else (
    let min = int_to_int32_trunc Int.min_value in
    let max = int_to_int32_trunc Int.max_value in
    fun x -> compare_int32 min x <= 0 && compare_int32 x max <= 0)
;;

let int_to_int32 x =
  if int_is_representable_as_int32 x then Some (int_to_int32_trunc x) else None
;;

let int32_to_int x =
  if int32_is_representable_as_int x then Some (int32_to_int_trunc x) else None
;;

let int_to_int32_exn x =
  if int_is_representable_as_int32 x
  then int_to_int32_trunc x
  else int_to_int32_failure x
;;

let int32_to_int_exn x =
  if int32_is_representable_as_int x
  then int32_to_int_trunc x
  else int32_to_int_failure x
;;

(* int <-> int64 *)

let int64_to_int_failure x = convert_failure x "int64" "int" int64_to_string
let () = assert (num_bits_int < num_bits_int64)
let int_to_int64 = Caml.Int64.of_int
let int64_to_int_trunc = Caml.Int64.to_int

let int64_is_representable_as_int =
  let min = int_to_int64 Int.min_value in
  let max = int_to_int64 Int.max_value in
  fun x -> compare_int64 min x <= 0 && compare_int64 x max <= 0
;;

let int64_to_int x =
  if int64_is_representable_as_int x then Some (int64_to_int_trunc x) else None
;;

let int64_to_int_exn x =
  if int64_is_representable_as_int x
  then int64_to_int_trunc x
  else int64_to_int_failure x
;;

(* int <-> nativeint *)

let nativeint_to_int_failure x = convert_failure x "nativeint" "int" nativeint_to_string
let () = assert (num_bits_int <= num_bits_nativeint)
let int_to_nativeint = Caml.Nativeint.of_int
let nativeint_to_int_trunc = Caml.Nativeint.to_int

let nativeint_is_representable_as_int =
  if num_bits_nativeint <= num_bits_int
  then fun _ -> true
  else (
    let min = int_to_nativeint Int.min_value in
    let max = int_to_nativeint Int.max_value in
    fun x -> compare_nativeint min x <= 0 && compare_nativeint x max <= 0)
;;

let nativeint_to_int x =
  if nativeint_is_representable_as_int x then Some (nativeint_to_int_trunc x) else None
;;

let nativeint_to_int_exn x =
  if nativeint_is_representable_as_int x
  then nativeint_to_int_trunc x
  else nativeint_to_int_failure x
;;

(* int32 <-> int64 *)

let int64_to_int32_failure x = convert_failure x "int64" "int32" int64_to_string
let () = assert (num_bits_int32 < num_bits_int64)
let int32_to_int64 = Caml.Int64.of_int32
let int64_to_int32_trunc = Caml.Int64.to_int32

let int64_is_representable_as_int32 =
  let min = int32_to_int64 min_int32 in
  let max = int32_to_int64 max_int32 in
  fun x -> compare_int64 min x <= 0 && compare_int64 x max <= 0
;;

let int64_to_int32 x =
  if int64_is_representable_as_int32 x then Some (int64_to_int32_trunc x) else None
;;

let int64_to_int32_exn x =
  if int64_is_representable_as_int32 x
  then int64_to_int32_trunc x
  else int64_to_int32_failure x
;;

(* int32 <-> nativeint *)

let nativeint_to_int32_failure x =
  convert_failure x "nativeint" "int32" nativeint_to_string
;;

let () = assert (num_bits_int32 <= num_bits_nativeint)
let int32_to_nativeint = Caml.Nativeint.of_int32
let nativeint_to_int32_trunc = Caml.Nativeint.to_int32

let nativeint_is_representable_as_int32 =
  if num_bits_nativeint <= num_bits_int32
  then fun _ -> true
  else (
    let min = int32_to_nativeint min_int32 in
    let max = int32_to_nativeint max_int32 in
    fun x -> compare_nativeint min x <= 0 && compare_nativeint x max <= 0)
;;

let nativeint_to_int32 x =
  if nativeint_is_representable_as_int32 x
  then Some (nativeint_to_int32_trunc x)
  else None
;;

let nativeint_to_int32_exn x =
  if nativeint_is_representable_as_int32 x
  then nativeint_to_int32_trunc x
  else nativeint_to_int32_failure x
;;

(* int64 <-> nativeint *)

let int64_to_nativeint_failure x = convert_failure x "int64" "nativeint" int64_to_string
let () = assert (num_bits_int64 >= num_bits_nativeint)
let int64_to_nativeint_trunc = Caml.Int64.to_nativeint
let nativeint_to_int64 = Caml.Int64.of_nativeint

let int64_is_representable_as_nativeint =
  if num_bits_int64 <= num_bits_nativeint
  then fun _ -> true
  else (
    let min = nativeint_to_int64 min_nativeint in
    let max = nativeint_to_int64 max_nativeint in
    fun x -> compare_int64 min x <= 0 && compare_int64 x max <= 0)
;;

let int64_to_nativeint x =
  if int64_is_representable_as_nativeint x
  then Some (int64_to_nativeint_trunc x)
  else None
;;

let int64_to_nativeint_exn x =
  if int64_is_representable_as_nativeint x
  then int64_to_nativeint_trunc x
  else int64_to_nativeint_failure x
;;

(* int64 <-> int63 *)

let int64_to_int63_failure x = convert_failure x "int64" "int63" int64_to_string

let int64_is_representable_as_int63 =
  let min = Caml.Int64.shift_right min_int64 1 in
  let max = Caml.Int64.shift_right max_int64 1 in
  fun x -> compare_int64 min x <= 0 && compare_int64 x max <= 0
;;

let int64_fit_on_int63_exn x =
  if int64_is_representable_as_int63 x then () else int64_to_int63_failure x
;;

(* string conversions *)

let insert_delimiter_every input ~delimiter ~chars_per_delimiter =
  let input_length = String.length input in
  if input_length <= chars_per_delimiter
  then input
  else (
    let has_sign =
      match input.[0] with
      | '+' | '-' -> true
      | _ -> false
    in
    let num_digits = if has_sign then input_length - 1 else input_length in
    let num_delimiters = (num_digits - 1) / chars_per_delimiter in
    let output_length = input_length + num_delimiters in
    let output = Bytes.create output_length in
    let input_pos = ref (input_length - 1) in
    let output_pos = ref (output_length - 1) in
    let num_chars_until_delimiter = ref chars_per_delimiter in
    let first_digit_pos = if has_sign then 1 else 0 in
    while !input_pos >= first_digit_pos do
      if !num_chars_until_delimiter = 0
      then (
        Bytes.set output !output_pos delimiter;
        decr output_pos;
        num_chars_until_delimiter := chars_per_delimiter);
      Bytes.set output !output_pos input.[!input_pos];
      decr input_pos;
      decr output_pos;
      decr num_chars_until_delimiter
    done;
    if has_sign then Bytes.set output 0 input.[0];
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:output)
;;

let insert_delimiter input ~delimiter =
  insert_delimiter_every input ~delimiter ~chars_per_delimiter:3
;;

let insert_underscores input = insert_delimiter input ~delimiter:'_'
let sexp_of_int_style = Sexp.of_int_style

module Make (I : sig
    type t

    val to_string : t -> string
  end) =
struct
  open I

  let chars_per_delimiter = 3

  let to_string_hum ?(delimiter = '_') t =
    insert_delimiter_every (to_string t) ~delimiter ~chars_per_delimiter
  ;;

  let sexp_of_t t =
    let s = to_string t in
    Sexp.Atom
      (match !sexp_of_int_style with
       | `Underscores -> insert_delimiter_every s ~chars_per_delimiter ~delimiter:'_'
       | `No_underscores -> s)
  ;;
end

module Make_hex (I : sig
    type t [@@deriving_inline compare, hash]

    val compare : t -> t -> int
    val hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
    val hash : t -> Ppx_hash_lib.Std.Hash.hash_value

    [@@@end]

    val to_string : t -> string
    val of_string : string -> t
    val zero : t
    val ( < ) : t -> t -> bool
    val neg : t -> t
    val module_name : string
  end) =
struct
  module T_hex = struct
    type t = I.t [@@deriving_inline compare, hash]

    let compare = (I.compare : t -> t -> int)

    let (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
      I.hash_fold_t

    and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
      let func = I.hash in
      fun x -> func x
    ;;

    [@@@end]

    let chars_per_delimiter = 4

    let to_string' ?delimiter t =
      let make_suffix =
        match delimiter with
        | None -> I.to_string
        | Some delimiter ->
          fun t -> insert_delimiter_every (I.to_string t) ~delimiter ~chars_per_delimiter
      in
      if I.( < ) t I.zero then "-0x" ^ make_suffix (I.neg t) else "0x" ^ make_suffix t
    ;;

    let to_string t = to_string' t ?delimiter:None
    let to_string_hum ?(delimiter = '_') t = to_string' t ~delimiter

    let invalid str =
      Printf.failwithf "%s.of_string: invalid input %S" I.module_name str ()
    ;;

    let of_string_with_delimiter str =
      I.of_string (String.filter str ~f:(fun c -> Char.( <> ) c '_'))
    ;;

    let of_string str =
      let module L = Hex_lexer in
      let lex = Caml.Lexing.from_string str in
      let result = Option.try_with (fun () -> L.parse_hex lex) in
      if lex.lex_curr_pos = lex.lex_buffer_len
      then (
        match result with
        | None -> invalid str
        | Some (Neg body) -> I.neg (of_string_with_delimiter body)
        | Some (Pos body) -> of_string_with_delimiter body)
      else invalid str
    ;;
  end

  module Hex = struct
    include T_hex
    include Sexpable.Of_stringable (T_hex)
  end
end
