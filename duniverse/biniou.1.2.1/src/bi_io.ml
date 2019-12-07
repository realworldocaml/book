open Printf

open Bi_outbuf
open Bi_inbuf

type node_tag = int

let bool_tag = 0
let int8_tag = 1
let int16_tag = 2
let int32_tag = 3
let int64_tag = 4
let float32_tag = 11
let float64_tag = 12
let uvint_tag = 16
let svint_tag = 17
let string_tag = 18
let array_tag = 19
let tuple_tag = 20
let record_tag = 21
let num_variant_tag = 22
let variant_tag = 23
let unit_tag = 24
let table_tag = 25
let shared_tag = 26

type hash = int

(*
  Data tree, for testing purposes.
*)
type tree =
    [ `Unit
    | `Bool of bool
    | `Int8 of char
    | `Int16 of int
    | `Int32 of Int32.t
    | `Int64 of Int64.t
    | `Float32 of float
    | `Float64 of float
    | `Uvint of int
    | `Svint of int
    | `String of string
    | `Array of (node_tag * tree array) option
    | `Tuple of tree array
    | `Record of (string option * hash * tree) array
    | `Num_variant of (int * tree option)
    | `Variant of (string option * hash * tree option)
    | `Table of
	((string option * hash * node_tag) array * tree array array) option
    | `Shared of tree ]

(* extend sign bit *)
let make_signed x =
  if x > 0x3FFFFFFF then x - (1 lsl 31) else x

(*
  Same function as the one used for OCaml variants and object methods.
*)
let hash_name s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  make_signed !accu


(*
  Structure of a hashtag: 4 bytes,

  argbit 7bits 8bits 8bits 8bits
         +---------------------+
              31-bit hash

  argbit = 1 iff hashtag is followed by an argument, this is always 1 for
           record fields.

*)

let mask_31bit =
  let n = Bi_util.int_size - 31 in
  assert (n >= 0);
  fun x -> (x lsl n) lsr n

let write_hashtag ob h has_arg =
  let h = mask_31bit h in
  let pos = Bi_outbuf.alloc ob 4 in
  let s = ob.o_s in
  Bytes.unsafe_set s (pos+3) (Char.chr (h land 0xff));
  let h = h lsr 8 in
  Bytes.unsafe_set s (pos+2) (Char.chr (h land 0xff));
  let h = h lsr 8 in
  Bytes.unsafe_set s (pos+1) (Char.chr (h land 0xff));
  let h = h lsr 8 in
  Bytes.unsafe_set s pos (
    Char.chr (
      if has_arg then h lor 0x80
      else h
    )
  )

let string_of_hashtag h has_arg =
  let ob = Bi_outbuf.create 4 in
  write_hashtag ob h has_arg;
  Bi_outbuf.contents ob

let read_hashtag ib cont =
  let i = Bi_inbuf.read ib 4 in
  let s = ib.i_s in
  let x0 = Char.code (Bytes.get s i) in
  let has_arg = x0 >= 0x80 in
  let x1 = (x0 land 0x7f) lsl 24 in
  let x2 = (Char.code (Bytes.get s (i+1))) lsl 16 in
  let x3 = (Char.code (Bytes.get s (i+2))) lsl 8 in
  let x4 = Char.code (Bytes.get s (i+3)) in
  let h = make_signed (x1 lor x2 lor x3 lor x4) in

  cont ib h has_arg


let read_field_hashtag ib =
  let i = Bi_inbuf.read ib 4 in
  let s = ib.i_s in
  let x0 = Char.code (Bytes.unsafe_get s i) in
  if x0 < 0x80 then
    Bi_util.error "Corrupted data (invalid field hashtag)";
  let x1 = (x0 land 0x7f) lsl 24 in
  let x2 = (Char.code (Bytes.unsafe_get s (i+1))) lsl 16 in
  let x3 = (Char.code (Bytes.unsafe_get s (i+2))) lsl 8 in
  let x4 = Char.code (Bytes.unsafe_get s (i+3)) in
  make_signed (x1 lor x2 lor x3 lor x4)


type int7 = int

let write_numtag ob i has_arg =
  if i < 0 || i > 0x7f then
    Bi_util.error "Corrupted data (invalid numtag)";
  let x =
    if has_arg then i lor 0x80
    else i
  in
  Bi_outbuf.add_char ob (Char.chr x)

let read_numtag ib cont =
  let i = Bi_inbuf.read ib 1 in
  let x = Char.code (Bytes.get ib.i_s i) in
  let has_arg = x >= 0x80 in
  cont ib (x land 0x7f) has_arg

let make_unhash l =
  let tbl = Hashtbl.create (4 * List.length l) in
  List.iter (
    fun s ->
      let h = hash_name s in
      try
	match Hashtbl.find tbl h with
	    Some s' ->
	      if s <> s' then
		failwith (
		  sprintf
		    "Bi_io.make_unhash: \
                     %S and %S have the same hash, please pick another name"
		    s s'
		)
	  | None -> assert false

      with Not_found -> Hashtbl.add tbl h (Some s)
  ) l;
  fun h ->
    try Hashtbl.find tbl h
    with Not_found -> None


let write_tag ob x =
  Bi_outbuf.add_char ob (Char.chr x)

let write_untagged_unit ob () =
  Bi_outbuf.add_char ob '\x00'

let write_untagged_bool ob x =
  Bi_outbuf.add_char ob (if x then '\x01' else '\x00')

let write_untagged_char ob x =
  Bi_outbuf.add_char ob x

let write_untagged_int8 ob x =
  Bi_outbuf.add_char ob (Char.chr x)

let write_untagged_int16 ob x =
  Bi_outbuf.add_char ob (Char.chr (x lsr 8));
  Bi_outbuf.add_char ob (Char.chr (x land 0xff))

let write_untagged_int32 ob x =
  let high = Int32.to_int (Int32.shift_right_logical x 16) in
  Bi_outbuf.add_char ob (Char.chr (high lsr 8));
  Bi_outbuf.add_char ob (Char.chr (high land 0xff));
  let low = Int32.to_int x in
  Bi_outbuf.add_char ob (Char.chr ((low lsr 8) land 0xff));
  Bi_outbuf.add_char ob (Char.chr (low land 0xff))

let write_untagged_float32 ob x =
  write_untagged_int32 ob (Int32.bits_of_float x)

let float_endianness = lazy (
  match String.unsafe_get (Obj.magic 1.0) 0 with
      '\x3f' -> `Big
    | '\x00' -> `Little
    | _ -> assert false
)

let read_untagged_float64 ib =
  let i = Bi_inbuf.read ib 8 in
  let s = ib.i_s in
  let x = Obj.new_block Obj.double_tag 8 in
  (match Lazy.force float_endianness with
       `Little ->
	 for j = 0 to 7 do
	   Bytes.unsafe_set (Obj.obj x) (7-j) (Bytes.unsafe_get s (i+j))
	 done
     | `Big ->
	 for j = 0 to 7 do
	   Bytes.unsafe_set (Obj.obj x) j (Bytes.unsafe_get s (i+j))
	 done
  );
  (Obj.obj x : float)

let write_untagged_float64 ob x =
  let i = Bi_outbuf.alloc ob 8 in
  let s = ob.o_s in
  (match Lazy.force float_endianness with
       `Little ->
	 for j = 0 to 7 do
	   Bytes.unsafe_set s (i+j) (String.unsafe_get (Obj.magic x) (7-j))
	 done
     | `Big ->
	 for j = 0 to 7 do
	   Bytes.unsafe_set s (i+j) (String.unsafe_get (Obj.magic x) j)
	 done
  )

(*
let write_untagged_int64 ob x =
  let x4 = Int64.to_int (Int64.shift_right_logical x 48) in
  Bi_outbuf.add_char ob (Char.chr (x4 lsr 8));
  Bi_outbuf.add_char ob (Char.chr (x4 land 0xff));
  let x3 = Int64.to_int (Int64.shift_right_logical x 32) in
  Bi_outbuf.add_char ob (Char.chr ((x3 lsr 8) land 0xff));
  Bi_outbuf.add_char ob (Char.chr (x3 land 0xff));
  let x2 = Int64.to_int (Int64.shift_right_logical x 16) in
  Bi_outbuf.add_char ob (Char.chr ((x2 lsr 8) land 0xff));
  Bi_outbuf.add_char ob (Char.chr (x2 land 0xff));
  let x1 = Int64.to_int x in
  Bi_outbuf.add_char ob (Char.chr ((x1 lsr 8) land 0xff));
  Bi_outbuf.add_char ob (Char.chr (x1 land 0xff))
*)

let write_untagged_int64 ob x =
  write_untagged_float64 ob (Int64.float_of_bits x)


let safety_test () =
  let s = "\x3f\xf0\x06\x05\x04\x03\x02\x01" in
  let x = 1.00146962706651288 in
  let y = read_untagged_float64 (Bi_inbuf.from_string s) in
  if x <> y then
    assert false;
  let ob = Bi_outbuf.create 8 in
  write_untagged_float64 ob x;
  if Bi_outbuf.contents ob <> s then
    assert false



let write_untagged_string ob s =
  Bi_vint.write_uvint ob (String.length s);
  Bi_outbuf.add_string ob s

let write_untagged_uvint = Bi_vint.write_uvint
let write_untagged_svint = Bi_vint.write_svint

let write_unit ob () =
  write_tag ob unit_tag;
  write_untagged_unit ob ()

let write_bool ob x =
  write_tag ob bool_tag;
  write_untagged_bool ob x

let write_char ob x =
  write_tag ob int8_tag;
  write_untagged_char ob x

let write_int8 ob x =
  write_tag ob int8_tag;
  write_untagged_int8 ob x

let write_int16 ob x =
  write_tag ob int16_tag;
  write_untagged_int16 ob x

let write_int32 ob x =
  write_tag ob int32_tag;
  write_untagged_int32 ob x

let write_int64 ob x =
  write_tag ob int64_tag;
  write_untagged_int64 ob x

let write_float32 ob x =
  write_tag ob float32_tag;
  write_untagged_float32 ob x

let write_float64 ob x =
  write_tag ob float64_tag;
  write_untagged_float64 ob x

let write_string ob x =
  write_tag ob string_tag;
  write_untagged_string ob x

let write_uvint ob x =
  write_tag ob uvint_tag;
  write_untagged_uvint ob x

let write_svint ob x =
  write_tag ob svint_tag;
  write_untagged_svint ob x




let rec write_t ob tagged (x : tree) =
  match x with
      `Unit ->
	if tagged then
	  write_tag ob unit_tag;
	write_untagged_unit ob ()

    | `Bool x ->
	if tagged then
	  write_tag ob bool_tag;
	write_untagged_bool ob x

    | `Int8 x ->
	if tagged then
	  write_tag ob int8_tag;
	write_untagged_char ob x

    | `Int16 x ->
	if tagged then
	  write_tag ob int16_tag;
	write_untagged_int16 ob x

    | `Int32 x ->
	if tagged then
	  write_tag ob int32_tag;
	write_untagged_int32 ob x

    | `Int64 x ->
	if tagged then
	  write_tag ob int64_tag;
	write_untagged_int64 ob x

    | `Float32 x ->
        if tagged then
	  write_tag ob float32_tag;
	write_untagged_float32 ob x

    | `Float64 x ->
	if tagged then
	  write_tag ob float64_tag;
	write_untagged_float64 ob x

    | `Uvint x ->
	if tagged then
	  write_tag ob uvint_tag;
	Bi_vint.write_uvint ob x

    | `Svint x ->
	if tagged then
	  write_tag ob svint_tag;
	Bi_vint.write_svint ob x

    | `String s ->
	if tagged then
	  write_tag ob string_tag;
	write_untagged_string ob s

    | `Array o ->
	if tagged then
	  write_tag ob array_tag;
	(match o with
	     None -> Bi_vint.write_uvint ob 0
	   | Some (node_tag, a) ->
	       let len = Array.length a in
	       Bi_vint.write_uvint ob len;
	       if len > 0 then (
		 write_tag ob node_tag;
		 Array.iter (write_t ob false) a
	       )
	)

    | `Tuple a ->
	if tagged then
	  write_tag ob tuple_tag;
	Bi_vint.write_uvint ob (Array.length a);
	Array.iter (write_t ob true) a

    | `Record a ->
	if tagged then
	  write_tag ob record_tag;
	Bi_vint.write_uvint ob (Array.length a);
        Array.iter (write_field ob) a

    | `Num_variant (i, x) ->
	if tagged then
	  write_tag ob num_variant_tag;
	write_numtag ob i (x <> None);
	(match x with
	     None -> ()
	   | Some v -> write_t ob true v)

    | `Variant (o, h, x) ->
	if tagged then
	  write_tag ob variant_tag;
	write_hashtag ob h (x <> None);
	(match x with
	     None -> ()
	   | Some v -> write_t ob true v)

    | `Table o ->
	if tagged then
	  write_tag ob table_tag;
	(match o with
	     None -> Bi_vint.write_uvint ob 0
	   | Some (fields, a) ->
	       let row_num = Array.length a in
	       Bi_vint.write_uvint ob row_num;
	       if row_num > 0 then
		 let col_num = Array.length fields in
		 Bi_vint.write_uvint ob col_num;
		 Array.iter (
		   fun (name, h, tag) ->
		     write_hashtag ob h true;
		     write_tag ob tag
		 ) fields;
		 if row_num > 0 then (
		   for i = 0 to row_num - 1 do
		     let ai = a.(i) in
		     if Array.length ai <> col_num then
		       invalid_arg "Bi_io.write_t: Malformed `Table";
		     for j = 0 to col_num - 1 do
		       write_t ob false ai.(j)
		     done
		   done
		 )
	)

    | `Shared x ->
        if tagged then
          write_tag ob shared_tag;
        let offset =
          Bi_share.Wr.put ob.o_shared
            (x, Bi_share.dummy_type_id) (ob.o_offs + ob.o_len) in
        Bi_vint.write_uvint ob offset;
        if offset = 0 then
          write_t ob true x

and write_field ob (s, h, x) =
  write_hashtag ob h true;
  write_t ob true x

let write_tree ob x =
  write_t ob true x

let string_of_tree x =
  let ob = Bi_outbuf.create 1000 in
  write_tree ob x;
  Bi_outbuf.contents ob

let tag_of_tree (x : tree) =
  match x with
      `Unit -> unit_tag
    | `Bool _ -> bool_tag
    | `Int8 _ -> int8_tag
    | `Int16 _ -> int16_tag
    | `Int32 _ -> int32_tag
    | `Int64 _ -> int64_tag
    | `Float32 _ -> float32_tag
    | `Float64 _ -> float64_tag
    | `Uvint _ -> uvint_tag
    | `Svint _ -> svint_tag
    | `String _ -> string_tag
    | `Array _ -> array_tag
    | `Tuple _ -> tuple_tag
    | `Record _ -> record_tag
    | `Num_variant _ -> num_variant_tag
    | `Variant _ -> variant_tag
    | `Table _ -> table_tag
    | `Shared _ -> shared_tag


let read_tag ib =
  Char.code (Bi_inbuf.read_char ib)

let read_untagged_unit ib =
  match Bi_inbuf.read_char ib with
      '\x00' -> ()
    | _ -> Bi_util.error "Corrupted data (unit value)"

let read_untagged_bool ib =
  match Bi_inbuf.read_char ib with
      '\x00' -> false
    | '\x01' -> true
    | _ -> Bi_util.error "Corrupted data (bool value)"

let read_untagged_char ib = Bi_inbuf.read_char ib

let read_untagged_int8 ib =
  Char.code (Bi_inbuf.read_char ib)

let read_untagged_int16 ib =
  let i = Bi_inbuf.read ib 2 in
  let s = ib.i_s in
  ((Char.code (Bytes.get s i)) lsl 8) lor (Char.code (Bytes.get s (i+1)))


let read_untagged_int32 ib =
  let i = Bi_inbuf.read ib 4 in
  let s = ib.i_s in
  let get_code s i = Char.code (Bytes.get s i) in
  let x1 =
    Int32.of_int (((get_code s (i  )) lsl 8) lor (get_code s (i+1))) in
  let x2 =
    Int32.of_int (((get_code s (i+2)) lsl 8) lor (get_code s (i+3))) in
  Int32.logor (Int32.shift_left x1 16) x2

let read_untagged_float32 ib =
  Int32.float_of_bits (read_untagged_int32 ib)

(*
let read_untagged_int64 ib =
  let i = Bi_inbuf.read ib 8 in
  let s = ib.i_s in
  let x1 =
    Int64.of_int (((Char.code s.[i  ]) lsl 8) lor (Char.code s.[i+1])) in
  let x2 =
    Int64.of_int (((Char.code s.[i+2]) lsl 8) lor (Char.code s.[i+3])) in
  let x3 =
    Int64.of_int (((Char.code s.[i+4]) lsl 8) lor (Char.code s.[i+5])) in
  let x4 =
    Int64.of_int (((Char.code s.[i+6]) lsl 8) lor (Char.code s.[i+7])) in
  Int64.logor (Int64.shift_left x1 48)
    (Int64.logor (Int64.shift_left x2 32)
       (Int64.logor (Int64.shift_left x3 16) x4))
*)

let read_untagged_int64 ib =
  Int64.bits_of_float (read_untagged_float64 ib)



let read_untagged_string ib =
  let len = Bi_vint.read_uvint ib in
  let str = Bytes.create len in
  let pos = ref 0 in
  let rem = ref len in
  while !rem > 0 do
    let bytes_read = Bi_inbuf.try_preread ib !rem in
    if bytes_read = 0 then
      Bi_util.error "Corrupted data (string)"
    else (
      Bytes.blit ib.i_s ib.i_pos str !pos bytes_read;
      ib.i_pos <- ib.i_pos + bytes_read;
      pos := !pos + bytes_read;
      rem := !rem - bytes_read
    )
  done;
  Bytes.to_string str

let read_untagged_uvint = Bi_vint.read_uvint
let read_untagged_svint = Bi_vint.read_svint

let read_unit ib = read_untagged_unit ib; `Unit

let read_bool ib = `Bool (read_untagged_bool ib)

let read_int8 ib = `Int8 (read_untagged_char ib)

let read_int16 ib = `Int16 (read_untagged_int16 ib)

let read_int32 ib = `Int32 (read_untagged_int32 ib)

let read_int64 ib = `Int64 (read_untagged_int64 ib)

let read_float32 ib =
  `Float32 (read_untagged_float32 ib)

let read_float64 ib =
  `Float64 (read_untagged_float64 ib)

let read_uvint ib = `Uvint (read_untagged_uvint ib)
let read_svint ib = `Svint (read_untagged_svint ib)

let read_string ib = `String (read_untagged_string ib)

let print s = print_string s; print_newline ()

let read_tree ?(unhash = make_unhash []) ib : tree =

  let rec read_array ib =
    let len = Bi_vint.read_uvint ib in
    if len = 0 then `Array None
    else
      let tag = read_tag ib in
      let read = reader_of_tag tag in
      `Array (Some (tag, Array.init len (fun _ -> read ib)))

  and read_tuple ib =
    let len = Bi_vint.read_uvint ib in
    `Tuple (Array.init len (fun _ -> read_tree ib))

  and read_field ib =
    let h = read_field_hashtag ib in
    let name = unhash h in
    let x = read_tree ib in
    (name, h, x)

  and read_record ib =
    let len = Bi_vint.read_uvint ib in
    `Record (Array.init len (fun _ -> read_field ib))

  and read_num_variant_cont ib i has_arg =
    let x =
      if has_arg then
	Some (read_tree ib)
      else
	None
    in
    `Num_variant (i, x)

  and read_num_variant ib =
    read_numtag ib read_num_variant_cont

  and read_variant_cont ib h has_arg =
    let name = unhash h in
    let x =
      if has_arg then
	Some (read_tree ib)
      else
	None
    in
    `Variant (name, h, x)

  and read_variant ib =
    read_hashtag ib read_variant_cont

  and read_table ib =
    let row_num = Bi_vint.read_uvint ib in
    if row_num = 0 then
      `Table None
    else
      let col_num = Bi_vint.read_uvint ib in
      let fields =
	Array.init col_num (
	  fun _ ->
	    let h = read_field_hashtag ib in
	    let name = unhash h in
	    let tag = read_tag ib in
	    (name, h, tag)
	)
      in
      let readers =
	Array.map (fun (name, h, tag) -> reader_of_tag tag) fields in
      let a =
	Array.init row_num
	  (fun _ ->
	     Array.init col_num (fun j -> readers.(j) ib))
      in
      `Table (Some (fields, a))

  and read_shared ib =
    let pos = ib.i_offs + ib.i_pos in
    let offset = Bi_vint.read_uvint ib in
    if offset = 0 then
      let rec r = `Shared r in
      Bi_share.Rd.put ib.i_shared
        (pos, Bi_share.dummy_type_id) (Obj.repr r);
      let x = read_tree ib in
      Obj.set_field (Obj.repr r) 1 (Obj.repr x);
      r
    else
      Obj.obj (Bi_share.Rd.get ib.i_shared
                 (pos - offset, Bi_share.dummy_type_id))

  and reader_of_tag = function
      0 (* bool *) -> read_bool
    | 1 (* int8 *) -> read_int8
    | 2 (* int16 *) -> read_int16
    | 3 (* int32 *) -> read_int32
    | 4 (* int64 *) -> read_int64
    | 11 (* float32 *) -> read_float32
    | 12 (* float64 *) -> read_float64
    | 16 (* uvint *) -> read_uvint
    | 17 (* svint *) -> read_svint
    | 18 (* string *) -> read_string
    | 19 (* array *) -> read_array
    | 20 (* tuple *) -> read_tuple
    | 21 (* record *) -> read_record
    | 22 (* num_variant *) -> read_num_variant
    | 23 (* variant *) -> read_variant
    | 24 (* unit *) -> read_unit
    | 25 (* table *) -> read_table
    | 26 (* shared *) -> read_shared
    | _ -> Bi_util.error "Corrupted data (invalid tag)"

  and read_tree ib : tree =
    reader_of_tag (read_tag ib) ib

  in
  read_tree ib

let tree_of_string ?unhash s = read_tree ?unhash (Bi_inbuf.from_string s)


let skip_bytes ib n = ignore (Bi_inbuf.read ib n)

let skip_unit ib = skip_bytes ib 1
let skip_bool ib = skip_bytes ib 1
let skip_int8 ib = skip_bytes ib 1
let skip_int16 ib = skip_bytes ib 2
let skip_int32 ib = skip_bytes ib 4
let skip_int64 ib = skip_bytes ib 8
let skip_float32 ib = skip_bytes ib 4
let skip_float64 ib = skip_bytes ib 8
let skip_uvint ib = ignore (read_untagged_uvint ib)
let skip_svint ib = ignore (read_untagged_svint ib)

let skip_string ib =
  let len = Bi_vint.read_uvint ib in
  skip_bytes ib len

let rec skip_array ib =
  let len = Bi_vint.read_uvint ib in
  if len = 0 then ()
  else
    let tag = read_tag ib in
    let read = skipper_of_tag tag in
    for i = 1 to len do
      read ib
    done

and skip_tuple ib =
  let len = Bi_vint.read_uvint ib in
  for i = 1 to len do
    skip ib
  done

and skip_field ib =
  ignore (read_field_hashtag ib);
  skip ib

and skip_record ib =
  let len = Bi_vint.read_uvint ib in
  for i = 1 to len do
    skip_field ib
  done

and skip_num_variant_cont ib i has_arg =
  if has_arg then
    skip ib

and skip_num_variant ib =
  read_numtag ib skip_num_variant_cont

and skip_variant_cont ib h has_arg =
  if has_arg then
    skip ib

and skip_variant ib =
  read_hashtag ib skip_variant_cont

and skip_table ib =
  let row_num = Bi_vint.read_uvint ib in
  if row_num = 0 then
    ()
  else
    let col_num = Bi_vint.read_uvint ib in
    let readers =
      Array.init col_num (
	fun _ ->
	  ignore (read_field_hashtag ib);
	  skipper_of_tag (read_tag ib)
      )
    in
    for i = 1 to row_num do
      for j = 1 to col_num do
	readers.(j) ib
      done
    done

and skipper_of_tag = function
    0 (* bool *) -> skip_bool
  | 1 (* int8 *) -> skip_int8
  | 2 (* int16 *) -> skip_int16
  | 3 (* int32 *) -> skip_int32
  | 4 (* int64 *) -> skip_int64
  | 11 (* float32 *) -> skip_float32
  | 12 (* float64 *) -> skip_float64
  | 16 (* uvint *) -> skip_uvint
  | 17 (* svint *) -> skip_svint
  | 18 (* string *) -> skip_string
  | 19 (* array *) -> skip_array
  | 20 (* tuple *) -> skip_tuple
  | 21 (* record *) -> skip_record
  | 22 (* num_variant *) -> skip_num_variant
  | 23 (* variant *) -> skip_variant
  | 24 (* unit *) -> skip_unit
  | 25 (* table *) -> skip_table
  | _ -> Bi_util.error "Corrupted data (invalid tag)"

and skip ib : unit =
  skipper_of_tag (read_tag ib) ib


(* Equivalent of Array.map that guarantees a left-to-right order *)
let array_map f a =
  let len = Array.length a in
  if len = 0 then [||]
  else (
    let r = Array.make len (f (Array.unsafe_get a 0)) in
    for i = 1 to len - 1 do
      Array.unsafe_set r i (f (Array.unsafe_get a i))
    done;
    r
  )


module Pp =
struct
  open Easy_format

  let array = list
  let record = list
  let tuple = { list with
		  space_after_opening = false;
		  space_before_closing = false;
		  align_closing = false }
  let variant = { list with
		    separators_stick_left = true }

  let map f a = Array.to_list (array_map f a)

  let rec format shared (x : tree) =
    match x with
        `Unit -> Atom ("unit", atom)
      | `Bool x -> Atom ((if x then "true" else "false"), atom)
      | `Int8 x -> Atom (sprintf "0x%02x" (Char.code x), atom)
      | `Int16 x -> Atom (sprintf "0x%04x" x, atom)
      | `Int32 x -> Atom (sprintf "0x%08lx" x, atom)
      | `Int64 x -> Atom (sprintf "0x%016Lx" x, atom)
      | `Float32 x -> Atom (string_of_float x, atom)
      | `Float64 x -> Atom (string_of_float x, atom)
      | `Uvint x -> Atom (string_of_int x, atom)
      | `Svint x -> Atom (string_of_int x, atom)
      | `String s -> Atom (sprintf "%S" s, atom)
      | `Array None -> Atom ("[]", atom)
      | `Array (Some (_, a)) ->
          List (("[", ",", "]", array), map (format shared) a)
      | `Tuple a -> List (("(", ",", ")", tuple), map (format shared) a)
      | `Record a -> List (("{", ",", "}", record), map (format_field shared) a)
      | `Num_variant (i, o) ->
	  let suffix =
	    if i = 0 then ""
	    else string_of_int i
	  in
	  (match o with
	       None -> Atom ("None" ^ suffix, atom)
	     | Some x ->
		 let cons = Atom ("Some" ^ suffix, atom) in
		 Label ((cons, label), format shared x))
      | `Variant (opt_name, h, o) ->
	  let name =
	    match opt_name with
		None -> sprintf "#%08lx" (Int32.of_int h)
	      | Some s -> sprintf "%S" s
	  in
	  (match o with
	       None -> Atom ("<" ^ name ^ ">", atom)
	     | Some x ->
		 List (("<", "", ">", tuple),
                       [ Label ((Atom (name ^ ":", atom), label),
                                format shared x) ])
	  )
      | `Table None -> Atom ("[]", atom)
      | `Table (Some (header, aa)) ->
	  let record_array =
	    `Array (
	      Some (
		record_tag,
		Array.map (
		  fun a ->
		    `Record (
		      Array.mapi (
			fun i x ->
			  let s, h, _ = header.(i) in
			  (s, h, x)
		      ) a
		    )
		) aa
	      )
	    ) in
	  format shared record_array

      | `Shared x ->
          let tbl, p = shared in
          incr p;
          let pos = !p in
          let offset = Bi_share.Wr.put tbl (x, Bi_share.dummy_type_id) pos in
          if offset = 0 then
            Label ((Atom (sprintf "shared%i ->" pos, atom), label),
                   format shared x)
          else
            Atom (sprintf "shared%i" (pos - offset), atom)

  and format_field shared (o, h, x) =
    let s =
      match o with
	  None -> sprintf "#%08lx" (Int32.of_int h)
	| Some s -> sprintf "%S" s
    in
    Label ((Atom (sprintf "%s:" s, atom), label), format shared x)
end

let init () = (Bi_share.Wr.create 512, ref 0)

let view_of_tree t =
  Easy_format.Pretty.to_string (Pp.format (init ()) t)

let print_view_of_tree t =
  Easy_format.Pretty.to_stdout (Pp.format (init ()) t)

let output_view_of_tree oc t =
  Easy_format.Pretty.to_channel oc (Pp.format (init ()) t)

let view ?unhash s =
  view_of_tree (tree_of_string ?unhash s)

let print_view ?unhash s =
  print_view_of_tree (tree_of_string ?unhash s)

let output_view ?unhash oc s =
  output_view_of_tree oc (tree_of_string ?unhash s)
