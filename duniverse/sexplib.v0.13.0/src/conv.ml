open Printf
open Bigarray

include Sexplib0.Sexp_conv
open Sexp

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t
type float32_vec = (float, float32_elt, fortran_layout) Array1.t
type float64_vec = (float, float64_elt, fortran_layout) Array1.t
type vec = float64_vec
type float32_mat = (float, float32_elt, fortran_layout) Array2.t
type float64_mat = (float, float64_elt, fortran_layout) Array2.t
type mat = float64_mat

let sexp_of_float_vec vec =
  let lst_ref = ref [] in
  for i = Array1.dim vec downto 1 do
    lst_ref := sexp_of_float vec.{i} :: !lst_ref
  done;
  List !lst_ref

let sexp_of_bigstring (bstr : bigstring) =
  let n = Array1.dim bstr in
  let str = Bytes.create n in
  for i = 0 to n - 1 do Bytes.set str i bstr.{i} done;
  Atom (Bytes.unsafe_to_string str)

let sexp_of_float32_vec (vec : float32_vec) = sexp_of_float_vec vec
let sexp_of_float64_vec (vec : float64_vec) = sexp_of_float_vec vec
let sexp_of_vec (vec : vec) = sexp_of_float_vec vec

let sexp_of_float_mat mat =
  let m = Array2.dim1 mat in
  let n = Array2.dim2 mat in
  let lst_ref = ref [] in
  for col = n downto 1 do
    let vec = Array2.slice_right mat col in
    for row = m downto 1 do
      lst_ref := sexp_of_float vec.{row} :: !lst_ref
    done
  done;
  List (sexp_of_int m :: sexp_of_int n :: !lst_ref)

let sexp_of_float32_mat (mat : float32_mat) = sexp_of_float_mat mat
let sexp_of_float64_mat (mat : float64_mat) = sexp_of_float_mat mat
let sexp_of_mat (mat : mat) = sexp_of_float_mat mat

let bigstring_of_sexp sexp = match sexp with
  | Atom str ->
    let len = String.length str in
    let bstr = Array1.create char c_layout len in
    for i = 0 to len - 1 do bstr.{i} <- str.[i] done;
    bstr
  | List _ -> of_sexp_error "bigstring_of_sexp: atom needed" sexp

let float_vec_of_sexp empty_float_vec create_float_vec sexp = match sexp with
  | List [] -> empty_float_vec
  | List lst ->
    let len = List.length lst in
    let res = create_float_vec len in
    let rec loop i = function
      | [] -> res
      | h :: t -> res.{i} <- float_of_sexp h; loop (i + 1) t in
    loop 1 lst
  | Atom _ -> of_sexp_error "float_vec_of_sexp: list needed" sexp

let create_float32_vec = Array1.create float32 fortran_layout
let create_float64_vec = Array1.create float64 fortran_layout
let empty_float32_vec = create_float32_vec 0
let empty_float64_vec = create_float64_vec 0
let float32_vec_of_sexp = float_vec_of_sexp empty_float32_vec create_float32_vec
let float64_vec_of_sexp = float_vec_of_sexp empty_float64_vec create_float64_vec
let vec_of_sexp = float_vec_of_sexp empty_float64_vec create_float64_vec

let check_too_much_data sexp data res =
  if data = [] then res
  else of_sexp_error "float_mat_of_sexp: too much data" sexp

let float_mat_of_sexp create_float_mat sexp = match sexp with
  | List (sm :: sn :: data) ->
    let m = int_of_sexp sm in
    let n = int_of_sexp sn in
    let res = create_float_mat m n in
    if m = 0 || n = 0 then check_too_much_data sexp data res
    else
      let rec loop_cols col data =
        let vec = Array2.slice_right res col in
        let rec loop_rows row = function
          | [] -> of_sexp_error "float_mat_of_sexp: not enough data" sexp
          | h :: t ->
            vec.{row} <- float_of_sexp h;
            if row = m then
              if col = n then check_too_much_data sexp t res
              else loop_cols (col + 1) t
            else loop_rows (row + 1) t in
        loop_rows 1 data in
      loop_cols 1 data
  | List _ -> of_sexp_error "float_mat_of_sexp: list too short" sexp
  | Atom _ -> of_sexp_error "float_mat_of_sexp: list needed" sexp

let create_float32_mat = Array2.create float32 fortran_layout
let create_float64_mat = Array2.create float64 fortran_layout

let float32_mat_of_sexp = float_mat_of_sexp create_float32_mat
let float64_mat_of_sexp = float_mat_of_sexp create_float64_mat
let mat_of_sexp = float_mat_of_sexp create_float64_mat

let string_of__of__sexp_of to_sexp x = Sexp.to_string (to_sexp x)

let of_string__of__of_sexp of_sexp s =
  try
    let sexp = Sexp.of_string s in
    of_sexp sexp
  with e ->
    failwith (sprintf "of_string failed on %s with %s" s
                (Sexp.to_string_hum (sexp_of_exn e)))
