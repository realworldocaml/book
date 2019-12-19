(* A substring is a contiguous sequence of characters in a string.  We use a
   functor because we want substrings of [string] and [bigstring].
*)

open! Import
open Std_internal

type bigstring = Bigstring.t

module Blit : sig
  type ('src, 'dst) t = ('src, 'dst) Blit.blito

  val string_string : (string, bytes) t
  [@@deprecated "[since 2017-10] use [string_bytes] instead"]

  val bigstring_string : (bigstring, bytes) t
  [@@deprecated "[since 2017-10] use [bigstring_bytes] instead"]

  val string_bytes : (string, bytes) t
  val bytes_bytes : (bytes, bytes) t
  val bigstring_bytes : (bigstring, bytes) t
  val string_bigstring : (string, bigstring) t
  val bytes_bigstring : (bytes, bigstring) t
  val bigstring_bigstring : (bigstring, bigstring) t
end = struct
  type ('src, 'dst) t = ('src, 'dst) Blit.blito

  let string_bytes ~src ?src_pos ?src_len ~dst ?(dst_pos = 0) () =
    let src_pos, len =
      Ordered_collection_common.get_pos_len_exn
        ()
        ?pos:src_pos
        ?len:src_len
        ~total_length:(String.length src)
    in
    Bytes.From_string.blit ~src ~src_pos ~len ~dst ~dst_pos
  ;;

  let string_string = string_bytes
  let bytes_bytes = Bytes.blito
  let string_bigstring = Bigstring.From_string.blito
  let bytes_bigstring = Bigstring.From_bytes.blito
  let bigstring_bigstring = Bigstring.blito
  let bigstring_string = Bigstring.To_bytes.blito
  let bigstring_bytes = Bigstring.To_bytes.blito
end

module type Base = sig
  type t

  val create : int -> t
  val length : t -> int
  val blit : (t, t) Blit.t
  val blit_to_bytes : (t, bytes) Blit.t
  val blit_to_bigstring : (t, bigstring) Blit.t
  val blit_from_string : (string, t) Blit.t
  val blit_from_bigstring : (bigstring, t) Blit.t

  val blit_to_string : (t, bytes) Blit.t
  [@@deprecated "[since 2017-10] use [blit_to_bytes] instead"]

  val get : t -> int -> char
end

module type S = Substring_intf.S

module F (Base : Base) : S with type base = Base.t = struct
  type base = Base.t

  type t =
    { base : Base.t
    ; pos : int
    ; len : int
    }

  (* {[
       let invariant t =
         assert (0 <= t.pos);
         assert (0 <= t.len);
         assert (t.pos + t.len <= Base.length t.base);
       ;; ]} *)

  let base t = t.base
  let pos t = t.pos
  let length t = t.len
  let is_empty t = Int.equal t.len 0

  let base_of_string s =
    let len = String.length s in
    let buf = Base.create len in
    Base.blit_from_string ~src:s ~dst:buf ();
    buf
  ;;

  let base_of_bigstring s =
    let len = Bigstring.length s in
    let buf = Base.create len in
    Base.blit_from_bigstring ~src:s ~dst:buf ();
    buf
  ;;

  let create ?pos ?len base =
    let pos, len =
      Ordered_collection_common.get_pos_len_exn
        ()
        ?pos
        ?len
        ~total_length:(Base.length base)
    in
    { base; pos; len }
  ;;

  let get t i =
    if i >= 0 && i < length t
    then Base.get (base t) (pos t + i)
    else raise (Invalid_argument "index out of bounds")
  ;;

  let sub ?pos ?len t =
    let pos, len =
      Ordered_collection_common.get_pos_len_exn () ?pos ?len ~total_length:(length t)
    in
    { base = t.base; pos = t.pos + pos; len }
  ;;

  module Make_arg = struct
    type nonrec t = t

    module Elt = Char

    let fold t ~init ~f =
      let rec go acc i = if i >= length t then acc else go (f acc (get t i)) (i + 1) in
      go init 0
    ;;

    let iter =
      `Custom
        (fun t ~f ->
           for i = 0 to length t - 1 do
             f (get t i)
           done)
    ;;

    let length = `Custom length
  end

  module C = Container.Make0 (Make_arg)

  let fold = C.fold
  let iter = C.iter
  let fold_result = C.fold_result
  let fold_until = C.fold_until

  (* [C.to_list] has to construct then reverse the list *)
  let to_list t = List.init (length t) ~f:(get t)
  let to_array = C.to_array
  let find_map = C.find_map
  let find = C.find
  let exists = C.exists
  let for_all = C.for_all
  let mem = C.mem
  let count = C.count
  let sum = C.sum
  let min_elt = C.min_elt
  let max_elt = C.max_elt

  let drop_prefix t n =
    if n > t.len
    then failwith "Substring.drop_prefix"
    else { base = t.base; pos = t.pos + n; len = t.len - n }
  ;;

  let drop_suffix t n =
    if n > t.len
    then failwith "Substring.drop_suffix"
    else { base = t.base; pos = t.pos; len = t.len - n }
  ;;

  let prefix t n =
    if n > t.len
    then failwith "Substring.prefix"
    else { base = t.base; pos = t.pos; len = n }
  ;;

  let suffix t n =
    if n > t.len
    then failwith "Substring.suffix"
    else { base = t.base; pos = t.pos + t.len - n; len = n }
  ;;

  let blit_to (type a) (blit : (Base.t, a) Blit.t) t ~dst ~dst_pos =
    blit ~src:t.base ~src_pos:t.pos ~src_len:t.len ~dst ~dst_pos ()
  ;;

  let blit_to_string = blit_to Base.blit_to_bytes
  let blit_to_bytes = blit_to Base.blit_to_bytes
  let blit_to_bigstring = blit_to Base.blit_to_bigstring
  let blit_base = blit_to Base.blit

  let blit_from ~name (type a) (blit : (a, base) Blit.t) t ~src ~src_pos ~len =
    if len > t.len
    then
      failwithf
        "Substring.blit_from_%s len > substring length : %d > %d"
        name
        len
        t.len
        ();
    blit ~src ~src_pos ~src_len:len ~dst:t.base ~dst_pos:t.pos ()
  ;;

  let blit_from_string = blit_from ~name:"string" Base.blit_from_string
  let blit_from_bigstring = blit_from ~name:"bigstring" Base.blit_from_bigstring
  let of_base base = { base; pos = 0; len = Base.length base }
  let of_string x = of_base (base_of_string x)
  let of_bigstring x = of_base (base_of_bigstring x)

  let make (type a) create (blit : (base, a) Blit.t) t =
    let dst = create t.len in
    blit ~src:t.base ~src_pos:t.pos ~src_len:t.len ~dst ~dst_pos:0 ();
    dst
  ;;

  let to_string x =
    Bytes.unsafe_to_string
      ~no_mutation_while_string_reachable:(make Bytes.create Base.blit_to_bytes x)
  ;;

  let to_bigstring = make Bigstring.create Base.blit_to_bigstring

  let concat_gen create_dst blit_dst ts =
    let len = List.fold ts ~init:0 ~f:(fun len t -> len + length t) in
    let dst = create_dst len in
    ignore
      (List.fold ts ~init:0 ~f:(fun dst_pos t ->
         blit_dst t ~dst ~dst_pos;
         dst_pos + length t)
       : int);
    dst
  ;;

  let concat ts = of_base (concat_gen Base.create blit_base ts)

  let concat_string ts =
    Bytes.unsafe_to_string
      ~no_mutation_while_string_reachable:(concat_gen Bytes.create blit_to_string ts)
  ;;

  let concat_bigstring ts = concat_gen Bigstring.create blit_to_bigstring ts
end
