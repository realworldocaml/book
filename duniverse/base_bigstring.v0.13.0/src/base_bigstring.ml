open! Base

module Bigstring0 = struct
  type t =
    ( char
    , Stdlib.Bigarray.int8_unsigned_elt
    , Stdlib.Bigarray.c_layout )
      Stdlib.Bigarray.Array1.t
end

module Array1 = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Stdlib.Bigarray.Array1.t

  external get : ('a, 'b, 'c) t -> int -> 'a = "%caml_ba_ref_1"
  external set : ('a, 'b, 'c) t -> int -> 'a -> unit = "%caml_ba_set_1"
  external unsafe_get : ('a, 'b, 'c) t -> int -> 'a = "%caml_ba_unsafe_ref_1"
  external unsafe_set : ('a, 'b, 'c) t -> int -> 'a -> unit = "%caml_ba_unsafe_set_1"
  external dim : ('a, 'b, 'c) t -> int = "%caml_ba_dim_1"
end

include Bigstring0

external aux_create
  :  max_mem_waiting_gc_in_bytes:int
  -> size:int
  -> t
  = "bigstring_alloc"

let sprintf = Printf.sprintf

(* One needs to use [Caml.Sys.word_size] so that its value is known at compile-time. *)
let arch_sixtyfour = Caml.Sys.word_size = 64
let arch_big_endian = Caml.Sys.big_endian
let not_on_32bit = Caml.Sys.word_size > 32

let create ?max_mem_waiting_gc_in_bytes size =
  let max_mem_waiting_gc_in_bytes =
    Option.value max_mem_waiting_gc_in_bytes ~default:(-1)
  in
  (* This check is important because [aux_create ~size:(-1)] raises [Out_of_memory], which
     could be confusing during debugging. *)
  if size < 0 then invalid_arg (sprintf "create: size = %d < 0" size);
  aux_create ~max_mem_waiting_gc_in_bytes ~size
;;

let length = Array1.dim

external is_mmapped : t -> bool = "bigstring_is_mmapped_stub" [@@noalloc]

let init n ~f =
  let t = create n in
  for i = 0 to n - 1 do
    t.{i} <- f i
  done;
  t
;;

let check_args ~loc ~pos ~len (bstr : t) =
  if pos < 0 then invalid_arg (loc ^ ": pos < 0");
  if len < 0 then invalid_arg (loc ^ ": len < 0");
  let bstr_len = length bstr in
  (* Be careful with overflow!  We could have bogons like [pos = Int.max_value] or [len =
     Int.max_value] passed by the user. *)
  if bstr_len - pos < len
  then invalid_arg (sprintf "Bigstring.%s: length(bstr) < pos + len" loc)
;;

let get_opt_len bstr ~pos = function
  | Some len -> len
  | None -> length bstr - pos
;;

(* Blitting *)

external unsafe_blit
  :  src:t
  -> src_pos:int
  -> dst:t
  -> dst_pos:int
  -> len:int
  -> unit
  = "bigstring_blit_stub"
[@@noalloc]

(* Exposing the external version of get/set supports better inlining. *)
external get : t -> int -> char = "%caml_ba_ref_1"
external set : t -> int -> char -> unit = "%caml_ba_set_1"

module Bigstring_sequence = struct
  type nonrec t = t

  let create ~len = create len
  let length = length
end

module Bytes_sequence = struct
  type t = bytes

  let create ~len = Bytes.create len
  let length = Bytes.length
end

include Blit.Make (struct
    include Bigstring_sequence

    let unsafe_blit = unsafe_blit
  end)

module From_bytes =
  Blit.Make_distinct
    (Bytes_sequence)
    (struct
      external unsafe_blit
        :  src:bytes
        -> src_pos:int
        -> dst:t
        -> dst_pos:int
        -> len:int
        -> unit
        = "bigstring_blit_bytes_bigstring_stub"
      [@@noalloc]

      include Bigstring_sequence
    end)

module To_bytes =
  Blit.Make_distinct
    (Bigstring_sequence)
    (struct
      external unsafe_blit
        :  src:t
        -> src_pos:int
        -> dst:bytes
        -> dst_pos:int
        -> len:int
        -> unit
        = "bigstring_blit_bigstring_bytes_stub"
      [@@noalloc]

      include Bytes_sequence
    end)

module From_string =
  Blit.Make_distinct
    (struct
      type t = string

      let length = String.length
    end)
    (struct
      external unsafe_blit
        :  src:string
        -> src_pos:int
        -> dst:t
        -> dst_pos:int
        -> len:int
        -> unit
        = "bigstring_blit_string_bigstring_stub"
      [@@noalloc]

      include Bigstring_sequence
    end)

module To_string = struct
  include To_bytes
  include Blit.Make_to_string (Bigstring0) (To_bytes)
end

let of_string = From_string.subo
let of_bytes = From_bytes.subo
let to_string = To_string.subo
let to_bytes = To_bytes.subo
let sexp_of_t t = Sexp.Atom (to_string t)

let t_of_sexp : Sexp.t -> t = function
  | Atom str -> of_string str
  | List _ as sexp ->
    Sexplib0.Sexp_conv.of_sexp_error "bigstring_of_sexp: atom needed" sexp
;;

let copy t : t = sub t ~pos:0 ~len:(length t)

let concat =
  let append ~src ~dst ~dst_pos_ref =
    let len = length src in
    let src_pos = 0 in
    let dst_pos = !dst_pos_ref in
    blit ~dst ~dst_pos ~src ~src_pos ~len;
    dst_pos_ref := dst_pos + len
  in
  fun ?sep list ->
    match list with
    | [] -> create 0
    | head :: tail ->
      let head_len = length head in
      let sep_len = Option.value_map sep ~f:length ~default:0 in
      let tail_count = List.length tail in
      let len =
        head_len + (sep_len * tail_count) + List.sum (module Int) tail ~f:length
      in
      let dst = create len in
      let dst_pos_ref = ref 0 in
      append ~src:head ~dst ~dst_pos_ref;
      List.iter tail ~f:(fun src ->
        (match sep with
         | None -> ()
         | Some sep -> append ~src:sep ~dst ~dst_pos_ref);
        append ~src ~dst ~dst_pos_ref);
      assert (!dst_pos_ref = len);
      dst
;;

external unsafe_memset
  :  t
  -> pos:int
  -> len:int
  -> char
  -> unit
  = "bigstring_memset_stub"
[@@noalloc]

let memset t ~pos ~len c =
  Ordered_collection_common.check_pos_len_exn ~pos ~len ~total_length:(length t);
  unsafe_memset t ~pos ~len c
;;

(* Comparison *)

external unsafe_memcmp
  :  t
  -> pos1:int
  -> t
  -> pos2:int
  -> len:int
  -> int
  = "bigstring_memcmp_stub"
[@@noalloc]

let memcmp t1 ~pos1 t2 ~pos2 ~len =
  Ordered_collection_common.check_pos_len_exn ~pos:pos1 ~len ~total_length:(length t1);
  Ordered_collection_common.check_pos_len_exn ~pos:pos2 ~len ~total_length:(length t2);
  unsafe_memcmp t1 ~pos1 t2 ~pos2 ~len
;;

let compare t1 t2 =
  if phys_equal t1 t2
  then 0
  else (
    let len1 = length t1 in
    let len2 = length t2 in
    let len = Int.min len1 len2 in
    match unsafe_memcmp t1 ~pos1:0 t2 ~pos2:0 ~len with
    | 0 -> if len1 < len2 then -1 else if len1 > len2 then 1 else 0
    | n -> n)
;;

external internalhash_fold_bigstring
  :  Hash.state
  -> t
  -> Hash.state
  = "internalhash_fold_bigstring"
[@@noalloc]

let _making_sure_the_C_binding_takes_an_int (x : Hash.state) = (x :> int)
let hash_fold_t = internalhash_fold_bigstring
let hash = Ppx_hash_lib.Std.Hash.of_fold hash_fold_t

type t_frozen = t [@@deriving compare, hash, sexp]

let equal t1 t2 =
  if phys_equal t1 t2
  then true
  else (
    let len1 = length t1 in
    let len2 = length t2 in
    Int.equal len1 len2 && Int.equal (unsafe_memcmp t1 ~pos1:0 t2 ~pos2:0 ~len:len1) 0)
;;

(* Search *)

external unsafe_find : t -> char -> pos:int -> len:int -> int = "bigstring_find"
[@@noalloc]

let find ?(pos = 0) ?len chr bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"find" ~pos ~len bstr;
  let res = unsafe_find bstr chr ~pos ~len in
  if res < 0 then None else Some res
;;

(* vim: set filetype=ocaml : *)

(* Binary-packing like accessors *)

external int32_of_int : int -> int32 = "%int32_of_int"
external int32_to_int : int32 -> int = "%int32_to_int"
external int64_of_int : int -> int64 = "%int64_of_int"
external int64_to_int : int64 -> int = "%int64_to_int"
external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external unsafe_get_16 : t -> int -> int = "%caml_bigstring_get16u"
external unsafe_get_32 : t -> int -> int32 = "%caml_bigstring_get32u"
external unsafe_get_64 : t -> int -> int64 = "%caml_bigstring_get64u"
external unsafe_set_16 : t -> int -> int -> unit = "%caml_bigstring_set16u"
external unsafe_set_32 : t -> int -> int32 -> unit = "%caml_bigstring_set32u"
external unsafe_set_64 : t -> int -> int64 -> unit = "%caml_bigstring_set64u"

let get_16 (t : t) (pos : int) : int =
  check_args ~loc:"get_16" ~pos ~len:2 t;
  unsafe_get_16 t pos
;;

let get_32 (t : t) (pos : int) : int32 =
  check_args ~loc:"get_32" ~pos ~len:4 t;
  unsafe_get_32 t pos
;;

let get_64 (t : t) (pos : int) : int64 =
  check_args ~loc:"get_64" ~pos ~len:8 t;
  unsafe_get_64 t pos
;;

let set_16_trunc (t : t) (pos : int) (v : int) : unit =
  check_args ~loc:"set_16" ~pos ~len:2 t;
  unsafe_set_16 t pos v
;;

let set_32 (t : t) (pos : int) (v : int32) : unit =
  check_args ~loc:"set_32" ~pos ~len:4 t;
  unsafe_set_32 t pos v
;;

let set_64 (t : t) (pos : int) (v : int64) : unit =
  check_args ~loc:"set_64" ~pos ~len:8 t;
  unsafe_set_64 t pos v
;;

let sign_extend_16 u = (u lsl (Int.num_bits - 16)) asr (Int.num_bits - 16)

let check_valid_uint16 x ~loc =
  if x < 0 || x > 0xFFFF
  then invalid_arg (sprintf "%s: %d is not a valid unsigned 16-bit integer" loc x)
;;

let check_valid_int16 x ~loc =
  if x < -0x8000 || x > 0x7FFF
  then invalid_arg (sprintf "%s: %d is not a valid (signed) 16-bit integer" loc x)
;;

let check_valid_uint8 x ~loc =
  if x < 0 || x > 0xFF
  then invalid_arg (sprintf "%s: %d is not a valid unsigned 8-bit integer" loc x)
;;

let check_valid_int8 x ~loc =
  if x < -0x80 || x > 0x7F
  then invalid_arg (sprintf "%s: %d is not a valid (signed) 8-bit integer" loc x)
;;

let check_valid_int32 =
  if not arch_sixtyfour
  then fun _ ~loc:_ -> ()
  else
    fun x ~loc ->
      if x >= -1 lsl 31 && x < 1 lsl 31
      then ()
      else invalid_arg (sprintf "%s: %d is not a valid (signed) 32-bit integer" loc x)
;;

let check_valid_uint32 =
  if not arch_sixtyfour
  then
    fun x ~loc ->
      if x >= 0
      then ()
      else invalid_arg (sprintf "%s: %d is not a valid unsigned 32-bit integer" loc x)
  else
    fun x ~loc ->
      if x >= 0 && x < 1 lsl 32
      then ()
      else invalid_arg (sprintf "%s: %d is not a valid unsigned 32-bit integer" loc x)
;;

let check_valid_uint64 x ~loc =
  if x >= 0
  then ()
  else invalid_arg (sprintf "%s: %d is not a valid unsigned 64-bit integer" loc x)
;;

let unsafe_read_int16 t ~pos = sign_extend_16 (unsafe_get_16 t pos)
let unsafe_read_int16_swap t ~pos = sign_extend_16 (swap16 (unsafe_get_16 t pos))
let unsafe_write_int16 t ~pos x = unsafe_set_16 t pos x
let unsafe_write_int16_swap t ~pos x = unsafe_set_16 t pos (swap16 x)
let read_int16 t ~pos = sign_extend_16 (get_16 t pos)
let read_int16_swap t ~pos = sign_extend_16 (swap16 (get_16 t pos))

let write_int16_exn t ~pos x =
  check_valid_int16 x ~loc:"Bigstring.write_int16";
  set_16_trunc t pos x
;;

let write_int16_swap_exn t ~pos x =
  (* Omit "_swap" from the error message it's bi-endian. *)
  check_valid_int16 x ~loc:"Bigstring.write_int16";
  set_16_trunc t pos (swap16 x)
;;

let unsafe_read_uint16 t ~pos = unsafe_get_16 t pos
let unsafe_read_uint16_swap t ~pos = swap16 (unsafe_get_16 t pos)
let unsafe_write_uint16 t ~pos x = unsafe_set_16 t pos x
let unsafe_write_uint16_swap t ~pos x = unsafe_set_16 t pos (swap16 x)
let read_uint16 t ~pos = get_16 t pos
let read_uint16_swap t ~pos = swap16 (get_16 t pos)

let write_uint16_exn t ~pos x =
  check_valid_uint16 x ~loc:"Bigstring.write_uint16";
  set_16_trunc t pos x
;;

let write_uint16_swap_exn t ~pos x =
  (* Omit "_swap" from the error message it's bi-endian. *)
  check_valid_uint16 x ~loc:"Bigstring.write_uint16";
  set_16_trunc t pos (swap16 x)
;;

let unsafe_read_int32_int t ~pos = int32_to_int (unsafe_get_32 t pos)
let unsafe_read_int32_int_swap t ~pos = int32_to_int (swap32 (unsafe_get_32 t pos))
let unsafe_read_int32 t ~pos = unsafe_get_32 t pos
let unsafe_read_int32_swap t ~pos = swap32 (unsafe_get_32 t pos)
let unsafe_write_int32 t ~pos x = unsafe_set_32 t pos x
let unsafe_write_int32_swap t ~pos x = unsafe_set_32 t pos (swap32 x)
let unsafe_write_int32_int t ~pos x = unsafe_set_32 t pos (int32_of_int x)
let unsafe_write_int32_int_swap t ~pos x = unsafe_set_32 t pos (swap32 (int32_of_int x))
let read_int32_int t ~pos = int32_to_int (get_32 t pos)
let read_int32_int_swap t ~pos = int32_to_int (swap32 (get_32 t pos))
let read_int32 t ~pos = get_32 t pos
let read_int32_swap t ~pos = swap32 (get_32 t pos)
let write_int32 t ~pos x = set_32 t pos x
let write_int32_swap t ~pos x = set_32 t pos (swap32 x)

let write_int32_int_exn t ~pos x =
  check_valid_int32 x ~loc:"Bigstring.write_int32_int";
  set_32 t pos (int32_of_int x)
;;

let write_int32_int_swap_exn t ~pos x =
  (* Omit "_swap" from the error message it's bi-endian. *)
  check_valid_int32 x ~loc:"Bigstring.write_int32_int";
  set_32 t pos (swap32 (int32_of_int x))
;;

let unsafe_read_int64_int t ~pos = int64_to_int (unsafe_get_64 t pos)
let unsafe_read_int64_int_swap t ~pos = int64_to_int (swap64 (unsafe_get_64 t pos))
let unsafe_read_int64 t ~pos = unsafe_get_64 t pos
let unsafe_read_int64_swap t ~pos = swap64 (unsafe_get_64 t pos)
let unsafe_write_int64 t ~pos x = unsafe_set_64 t pos x
let unsafe_write_int64_swap t ~pos x = unsafe_set_64 t pos (swap64 x)
let unsafe_write_int64_int t ~pos x = unsafe_set_64 t pos (int64_of_int x)
let unsafe_write_int64_int_swap t ~pos x = unsafe_set_64 t pos (swap64 (int64_of_int x))
let read_int64_int t ~pos = int64_to_int (get_64 t pos)
let read_int64_int_swap t ~pos = int64_to_int (swap64 (get_64 t pos))
let read_int64 t ~pos = get_64 t pos
let read_int64_swap t ~pos = swap64 (get_64 t pos)
let write_int64 t ~pos x = set_64 t pos x
let write_int64_swap t ~pos x = set_64 t pos (swap64 x)
let write_int64_int t ~pos x = set_64 t pos (int64_of_int x)
let write_int64_int_swap t ~pos x = set_64 t pos (swap64 (int64_of_int x))

let unsafe_get_int16_be =
  if arch_big_endian then unsafe_read_int16 else unsafe_read_int16_swap
;;

let unsafe_get_int16_le =
  if arch_big_endian then unsafe_read_int16_swap else unsafe_read_int16
;;

let unsafe_get_uint16_be =
  if arch_big_endian then unsafe_read_uint16 else unsafe_read_uint16_swap
;;

let unsafe_get_uint16_le =
  if arch_big_endian then unsafe_read_uint16_swap else unsafe_read_uint16
;;

let get_int16_be = if arch_big_endian then read_int16 else read_int16_swap
let get_int16_le = if arch_big_endian then read_int16_swap else read_int16
let get_uint16_be = if arch_big_endian then read_uint16 else read_uint16_swap
let get_uint16_le = if arch_big_endian then read_uint16_swap else read_uint16

let unsafe_set_int16_be =
  if arch_big_endian then unsafe_write_int16 else unsafe_write_int16_swap
;;

let unsafe_set_int16_le =
  if arch_big_endian then unsafe_write_int16_swap else unsafe_write_int16
;;

let unsafe_set_uint16_be =
  if arch_big_endian then unsafe_write_uint16 else unsafe_write_uint16_swap
;;

let unsafe_set_uint16_le =
  if arch_big_endian then unsafe_write_uint16_swap else unsafe_write_uint16
;;

let set_int16_be_exn = if arch_big_endian then write_int16_exn else write_int16_swap_exn
let set_int16_le_exn = if arch_big_endian then write_int16_swap_exn else write_int16_exn

let set_uint16_be_exn =
  if arch_big_endian then write_uint16_exn else write_uint16_swap_exn
;;

let set_uint16_le_exn =
  if arch_big_endian then write_uint16_swap_exn else write_uint16_exn
;;

let unsafe_get_int32_t_be =
  if arch_big_endian then unsafe_read_int32 else unsafe_read_int32_swap
;;

let unsafe_get_int32_t_le =
  if arch_big_endian then unsafe_read_int32_swap else unsafe_read_int32
;;

let unsafe_set_int32_t_be =
  if arch_big_endian then unsafe_write_int32 else unsafe_write_int32_swap
;;

let unsafe_set_int32_t_le =
  if arch_big_endian then unsafe_write_int32_swap else unsafe_write_int32
;;

let get_int32_t_be = if arch_big_endian then read_int32 else read_int32_swap
let get_int32_t_le = if arch_big_endian then read_int32_swap else read_int32
let set_int32_t_be = if arch_big_endian then write_int32 else write_int32_swap
let set_int32_t_le = if arch_big_endian then write_int32_swap else write_int32

let unsafe_get_int32_be =
  if arch_big_endian then unsafe_read_int32_int else unsafe_read_int32_int_swap
;;

let unsafe_get_int32_le =
  if arch_big_endian then unsafe_read_int32_int_swap else unsafe_read_int32_int
;;

let unsafe_set_int32_be =
  if arch_big_endian then unsafe_write_int32_int else unsafe_write_int32_int_swap
;;

let unsafe_set_int32_le =
  if arch_big_endian then unsafe_write_int32_int_swap else unsafe_write_int32_int
;;

let get_int32_be = if arch_big_endian then read_int32_int else read_int32_int_swap
let get_int32_le = if arch_big_endian then read_int32_int_swap else read_int32_int

let set_int32_be_exn =
  if arch_big_endian then write_int32_int_exn else write_int32_int_swap_exn
;;

let set_int32_le_exn =
  if arch_big_endian then write_int32_int_swap_exn else write_int32_int_exn
;;

let unsafe_get_int64_be_trunc =
  if arch_big_endian then unsafe_read_int64_int else unsafe_read_int64_int_swap
;;

let unsafe_get_int64_le_trunc =
  if arch_big_endian then unsafe_read_int64_int_swap else unsafe_read_int64_int
;;

let unsafe_set_int64_be =
  if arch_big_endian then unsafe_write_int64_int else unsafe_write_int64_int_swap
;;

let unsafe_set_int64_le =
  if arch_big_endian then unsafe_write_int64_int_swap else unsafe_write_int64_int
;;

let get_int64_be_trunc = if arch_big_endian then read_int64_int else read_int64_int_swap
let get_int64_le_trunc = if arch_big_endian then read_int64_int_swap else read_int64_int
let set_int64_be = if arch_big_endian then write_int64_int else write_int64_int_swap
let set_int64_le = if arch_big_endian then write_int64_int_swap else write_int64_int

let unsafe_get_int64_t_be =
  if arch_big_endian then unsafe_read_int64 else unsafe_read_int64_swap
;;

let unsafe_get_int64_t_le =
  if arch_big_endian then unsafe_read_int64_swap else unsafe_read_int64
;;

let unsafe_set_int64_t_be =
  if arch_big_endian then unsafe_write_int64 else unsafe_write_int64_swap
;;

let unsafe_set_int64_t_le =
  if arch_big_endian then unsafe_write_int64_swap else unsafe_write_int64
;;

let get_int64_t_be = if arch_big_endian then read_int64 else read_int64_swap
let get_int64_t_le = if arch_big_endian then read_int64_swap else read_int64
let set_int64_t_be = if arch_big_endian then write_int64 else write_int64_swap
let set_int64_t_le = if arch_big_endian then write_int64_swap else write_int64

let int64_conv_error () =
  failwith "unsafe_read_int64: value cannot be represented unboxed!"
;;

let uint64_conv_error () =
  failwith "unsafe_read_uint64: value cannot be represented unboxed!"
;;

(* [Poly] is required so that we can compare unboxed [int64]. *)
let int64_to_int_exn n =
  if arch_sixtyfour
  then
    if Poly.(n >= -0x4000_0000_0000_0000L && n < 0x4000_0000_0000_0000L)
    then int64_to_int n
    else int64_conv_error ()
  else if Poly.(n >= -0x0000_0000_4000_0000L && n < 0x0000_0000_4000_0000L)
  then int64_to_int n
  else int64_conv_error ()
;;

let uint64_to_int_exn n =
  if arch_sixtyfour
  then
    if Poly.(n >= 0L && n < 0x4000_0000_0000_0000L)
    then int64_to_int n
    else uint64_conv_error ()
  else if Poly.(n >= 0L && n < 0x0000_0000_4000_0000L)
  then int64_to_int n
  else uint64_conv_error ()
;;

let unsafe_get_int64_be_exn t ~pos = int64_to_int_exn (unsafe_get_int64_t_be t ~pos)
let unsafe_get_int64_le_exn t ~pos = int64_to_int_exn (unsafe_get_int64_t_le t ~pos)
let get_int64_be_exn t ~pos = int64_to_int_exn (get_int64_t_be t ~pos)
let get_int64_le_exn t ~pos = int64_to_int_exn (get_int64_t_le t ~pos)
let unsafe_get_uint64_be_exn t ~pos = uint64_to_int_exn (unsafe_get_int64_t_be t ~pos)
let unsafe_get_uint64_le_exn t ~pos = uint64_to_int_exn (unsafe_get_int64_t_le t ~pos)
let get_uint64_be_exn t ~pos = uint64_to_int_exn (get_int64_t_be t ~pos)
let get_uint64_le_exn t ~pos = uint64_to_int_exn (get_int64_t_le t ~pos)
let unsafe_set_uint64_be = unsafe_set_int64_be
let unsafe_set_uint64_le = unsafe_set_int64_le

let set_uint64_be_exn t ~pos n =
  check_valid_uint64 ~loc:"Bigstring.set_uint64_be_exn" n;
  set_int64_be t ~pos n
;;

let set_uint64_le_exn t ~pos n =
  check_valid_uint64 ~loc:"Bigstring.set_uint64_le_exn" n;
  set_int64_le t ~pos n
;;

(* Type annotations on the [t]s are important here: in order for the compiler to generate
   optimized code, it needs to know the fully instantiated type of the bigarray. This is
   because the type of the bigarray encodes the element kind and the layout of the
   bigarray. Without the annotation the compiler generates a C call to the generic access
   functions. *)
let unsafe_set_uint8 (t : t) ~pos n = Array1.unsafe_set t pos (Char.unsafe_of_int n)

let unsafe_set_int8 (t : t) ~pos n =
  (* In all the set functions where there are these tests, it looks like the test could be
     removed, since they are only changing the values of the bytes that are not
     written. *)
  let n = if n < 0 then n + 256 else n in
  Array1.unsafe_set t pos (Char.unsafe_of_int n)
;;

let unsafe_get_uint8 (t : t) ~pos = Char.to_int (Array1.unsafe_get t pos)

let unsafe_get_int8 (t : t) ~pos =
  let n = Char.to_int (Array1.unsafe_get t pos) in
  if n >= 128 then n - 256 else n
;;

let set_uint8_exn (t : t) ~pos n =
  check_valid_uint8 ~loc:"Bigstring.set_uint8_exn" n;
  Array1.set t pos (Char.unsafe_of_int n)
;;

let set_int8_exn (t : t) ~pos n =
  check_valid_int8 ~loc:"Bigstring.set_int8_exn" n;
  let n = if n < 0 then n + 256 else n in
  Array1.set t pos (Char.unsafe_of_int n)
;;

let get_uint8 (t : t) ~pos = Char.to_int (Array1.get t pos)

let get_int8 (t : t) ~pos =
  let n = Char.to_int (Array1.get t pos) in
  if n >= 128 then n - 256 else n
;;

let unsafe_set_uint32_le t ~pos n =
  let n = if not_on_32bit && n >= 1 lsl 31 then n - (1 lsl 32) else n in
  unsafe_set_int32_le t ~pos n
;;

let unsafe_set_uint32_be t ~pos n =
  let n = if not_on_32bit && n >= 1 lsl 31 then n - (1 lsl 32) else n in
  unsafe_set_int32_be t ~pos n
;;

let unsafe_get_uint32_le t ~pos =
  let n = unsafe_get_int32_le t ~pos in
  if not_on_32bit && n < 0 then n + (1 lsl 32) else n
;;

let unsafe_get_uint32_be t ~pos =
  let n = unsafe_get_int32_be t ~pos in
  if not_on_32bit && n < 0 then n + (1 lsl 32) else n
;;

let set_uint32_le_exn t ~pos n =
  check_valid_uint32 ~loc:"Bigstring.set_uint32_le_exn" n;
  let n = if not_on_32bit && n >= 1 lsl 31 then n - (1 lsl 32) else n in
  set_int32_le_exn t ~pos n
;;

let set_uint32_be_exn t ~pos n =
  check_valid_uint32 ~loc:"Bigstring.set_uint32_be_exn" n;
  let n = if not_on_32bit && n >= 1 lsl 31 then n - (1 lsl 32) else n in
  set_int32_be_exn t ~pos n
;;

let get_uint32_le t ~pos =
  let n = get_int32_le t ~pos in
  if not_on_32bit && n < 0 then n + (1 lsl 32) else n
;;

let get_uint32_be t ~pos =
  let n = get_int32_be t ~pos in
  if not_on_32bit && n < 0 then n + (1 lsl 32) else n
;;

module Private = struct
  let sign_extend_16 = sign_extend_16
end
