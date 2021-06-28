type bigstring = (char, Bigarray_compat.int8_unsigned_elt, Bigarray_compat.c_layout) Bigarray_compat.Array1.t

let length x = Bigarray_compat.Array1.dim x [@@inline]
let get x i = Bigarray_compat.Array1.unsafe_get x i |> Char.code [@@inline]
external unsafe_get_int16 : bigstring -> int -> int = "%caml_bigstring_get16u" [@@noalloc]
let get16 x i = unsafe_get_int16 x i [@@inline]

let equal ~ln a b =
  let l1 = ln asr 1 in
  let r = ref 0 in
  for i = 0 to pred l1 do r := !r lor (get16 a (i * 2) lxor get16 b (i * 2)) done ;
  for _ = 1 to ln land 1 do r := !r lor (get a (ln - 1) lxor get b (ln - 1)) done ;
  !r = 0

let equal a b =
  let al = length a in
  let bl = length b in
  if al <> bl
  then false
  else equal ~ln:al a b

let[@inline always] compare (a:int) b = a - b
let[@inline always] sixteen_if_minus_one_or_less n = (n asr Sys.int_size) land 16
let[@inline always] eight_if_one_or_more n = ((-n) asr Sys.int_size) land 8

let compare_le ~ln a b =
  let r = ref 0 in
  let i = ref (pred ln) in

  while !i >= 0 do
    let xa = get a !i and xb = get b !i in
    let c = compare xa xb in
    r := !r lor ((sixteen_if_minus_one_or_less c + eight_if_one_or_more c) lsr !r) ;
    decr i ;
  done ;

  (!r land 8) - (!r land 16)

let compare_le_with_len ~len:ln a b =
  let al = length a in
  let bl = length b in
  if ln = 0 then 0
  else if (al lxor ln) lor (bl lxor ln) <> 0
  then invalid_arg "compare_le_with_len"
  else compare_le ~ln a b

let compare_le a b =
  let al = length a in
  let bl = length b in
  if al < bl
  then 1
  else if al > bl
  then (-1)
  else compare_le ~ln:al (* = bl *) a b

let compare_be ~ln a b =
  let r = ref 0 in
  let i = ref 0 in

  while !i < ln do
    let xa = get a !i and xb = get b !i in
    let c = compare xa xb in
    r := !r lor ((sixteen_if_minus_one_or_less c + eight_if_one_or_more c) lsr !r) ;
    incr i ;
  done ;

  (!r land 8) - (!r land 16)

let compare_be_with_len ~len:ln a b =
  let al = length a in
  let bl = length b in
  if ln = 0 then 0
  else if (al lxor ln) lor (bl lxor ln) <> 0
  then invalid_arg "compare_be_with_len"
  else compare_be ~ln a b

let compare_be a b =
  let al = length a in
  let bl = length b in
  if al < bl then 1
  else if al > bl then (-1)
  else compare_be ~ln:al (* = bl *) a b

(* XXX(dinosaure): see [eqaf.ml] for this part. *)

external int_of_bool : bool -> int = "%identity"
external unsafe_bool_of_int : int -> bool = "%identity"

let[@inline always] find_uint8 ~off ~len ~f str =
  let i = ref (len - 1) in
  let a = ref (lnot 0) in
  while !i >= off do
    let byte = get str !i in
    let pred = int_of_bool (f byte) in
    a := Eqaf.select_int (((!i - off) land min_int) lor pred) !a !i ;
    decr i ;
  done ; !a

let find_uint8 ?(off= 0) ~f str =
  let len = length str in
  find_uint8 ~off ~len ~f str

let exists_uint8 ?off ~f str =
  let v = find_uint8 ?off ~f str in
  let r = Eqaf.select_int (v + 1) 0 1 in
  unsafe_bool_of_int r
