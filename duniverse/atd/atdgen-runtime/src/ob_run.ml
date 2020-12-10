[@@@ocaml.warning "-32"]

(*
  Runtime library
*)

open Printf

exception Error of string

(*
  Error messages
*)
let error s = raise (Error s)

let read_error () =
  error "Read error"

let read_error_at ib =
  error (sprintf "Read error (%i)" ib.Bi_inbuf.i_pos)

let tag_error tag s =
  error (sprintf "Found wrong tag %i for %s" tag s)

let unsupported_variant h has_arg =
  error (sprintf "Unsupported variant (hash=%i, arg=%B)" h has_arg)


let missing_tuple_fields len req_fields =
  let missing =
    List.fold_right (
      fun i acc -> if i >= len then i :: acc else acc
    ) req_fields []
  in
  error (sprintf "Missing tuple field%s %s"
           (if List.length missing > 1 then "s" else "")
           (String.concat ", " (List.map string_of_int missing)))


let missing_fields bit_fields field_names =
  let acc = ref [] in
  for z = Array.length field_names - 1 downto 0 do
    let i = z / 31 in
    let j = z mod 31 in
    if bit_fields.(i) land (1 lsl j) = 0 then
      acc := field_names.(z) :: !acc
  done;
  error (sprintf "Missing record field%s %s"
           (if List.length !acc > 1 then "s" else "")
           (String.concat ", " !acc))


(*
  Readers
*)

let get_unit_reader tag =
  if tag = Bi_io.unit_tag then
    Bi_io.read_untagged_unit
  else
    tag_error tag "unit"

let read_unit ib =
  if Bi_io.read_tag ib = Bi_io.unit_tag then
    Bi_io.read_untagged_unit ib
  else
    read_error_at ib

let get_bool_reader tag =
  if tag = Bi_io.bool_tag then
    Bi_io.read_untagged_bool
  else
    tag_error tag "bool"

let read_bool ib =
  if Bi_io.read_tag ib = Bi_io.bool_tag then
    Bi_io.read_untagged_bool ib
  else
    read_error_at ib

let get_int_reader tag =
  match tag with
      1 -> Bi_io.read_untagged_int8
    | 2 -> Bi_io.read_untagged_int16
    | 16 -> Bi_io.read_untagged_uvint
    | 17 -> Bi_io.read_untagged_svint
    | _ -> tag_error tag "int"

let read_int ib =
  match Bi_io.read_tag ib with
      1 -> Bi_io.read_untagged_int8 ib
    | 2 -> Bi_io.read_untagged_int16 ib
    | 16 -> Bi_io.read_untagged_uvint ib
    | 17 -> Bi_io.read_untagged_svint ib
    | _ -> read_error_at ib

let get_char_reader tag =
  if tag = Bi_io.int8_tag then
    Bi_io.read_untagged_char
  else
    tag_error tag "char"

let read_char ib =
  if Bi_io.read_tag ib = Bi_io.int8_tag then
    Bi_io.read_untagged_char ib
  else
    read_error_at ib

let get_int16_reader tag =
  if tag = Bi_io.int16_tag then
    Bi_io.read_untagged_int16
  else
    tag_error tag "int16"

let read_int16 ib =
  if Bi_io.read_tag ib = Bi_io.int16_tag then
    Bi_io.read_untagged_int16 ib
  else
    read_error_at ib

let get_int32_reader tag =
  if tag = Bi_io.int32_tag then
    Bi_io.read_untagged_int32
  else
    tag_error tag "int32"

let read_int32 ib =
  if Bi_io.read_tag ib = Bi_io.int32_tag then
    Bi_io.read_untagged_int32 ib
  else
    read_error_at ib

let get_int64_reader tag =
  if tag = Bi_io.int64_tag then
    Bi_io.read_untagged_int64
  else
    tag_error tag "int64"

let read_int64 ib =
  if Bi_io.read_tag ib = Bi_io.int64_tag then
    Bi_io.read_untagged_int64 ib
  else
    read_error_at ib

let get_float32_reader tag =
  if tag = Bi_io.float32_tag then
    Bi_io.read_untagged_float32
  else
    tag_error tag "float32"

let get_float64_reader tag =
  if tag = Bi_io.float64_tag then
    Bi_io.read_untagged_float64
  else
    tag_error tag "float64"

let get_float_reader = get_float64_reader

let read_float32 ib =
  if Bi_io.read_tag ib = Bi_io.float32_tag then
    Bi_io.read_untagged_float32 ib
  else
    read_error_at ib

let read_float64 ib =
  if Bi_io.read_tag ib = Bi_io.float64_tag then
    Bi_io.read_untagged_float64 ib
  else
    read_error_at ib

let read_float = read_float64

let get_string_reader tag =
  if tag = Bi_io.string_tag then
    Bi_io.read_untagged_string
  else
    tag_error tag "string"

let read_string ib =
  if Bi_io.read_tag ib = Bi_io.string_tag then
    Bi_io.read_untagged_string ib
  else
    read_error_at ib

let read_array_value get_reader ib =
  let len = Bi_vint.read_uvint ib in
  if len = 0 then [| |]
  else
    let reader = get_reader (Bi_io.read_tag ib) in
    let a = Array.make len (reader ib) in
    for i = 1 to len - 1 do
      Array.unsafe_set a i (reader ib)
    done;
    a

let read_list_value get_reader ib =
  Array.to_list (read_array_value get_reader ib)

let get_array_reader get_reader tag =
  if tag = Bi_io.array_tag then
    read_array_value get_reader
  else
    tag_error tag "array"

let get_list_reader get_reader tag =
  if tag = Bi_io.array_tag then
    fun ib -> Array.to_list (read_array_value get_reader ib)
  else
    tag_error tag "list"

let read_array get_reader ib =
  if Bi_io.read_tag ib = Bi_io.array_tag then
    read_array_value get_reader ib
  else
    read_error_at ib

let read_list read ib =
  Array.to_list (read_array read ib)


(*
  Writers
*)

let write_tagged tag write buf x =
  Bi_io.write_tag buf tag;
  write buf x

let write_untagged_option write buf x =
  match x with
      None -> Bi_io.write_numtag buf 0 false
    | Some x ->
        Bi_io.write_numtag buf 0 true;
        write buf x

let write_option write buf x =
  Bi_io.write_tag buf Bi_io.num_variant_tag;
  write_untagged_option write buf x

let array_init2 len x f =
  if len = 0 then [| |]
  else
    let a = Array.make len (f 0 x) in
    for i = 1 to len - 1 do
      Array.unsafe_set a i (f i x)
    done;
    a

let array_init3 len x y f =
  if len = 0 then [| |]
  else
    let a = Array.make len (f 0 x y) in
    for i = 1 to len - 1 do
      Array.unsafe_set a i (f i x y)
    done;
    a

let array_iter2 f x a =
  for i = 0 to Array.length a - 1 do
    f x (Array.unsafe_get a i)
  done

let array_iter3 f x y a =
  for i = 0 to Array.length a - 1 do
    f x y (Array.unsafe_get a i)
  done


let rec list_iter2 f x = function
    [] -> ()
  | y :: l ->
      f x y;
      list_iter2 f x l

let rec list_iter3 f x y = function
    [] -> ()
  | z :: l ->
      f x y z;
      list_iter3 f x y l


let write_untagged_array cell_tag write buf a =
  let len = Array.length a in
  Bi_vint.write_uvint buf len;
  if len > 0 then (
    Bi_io.write_tag buf cell_tag;
    array_iter2 write buf a
  )

let write_array cell_tag write buf a =
  Bi_io.write_tag buf Bi_io.array_tag;
  write_untagged_array cell_tag write buf a

let write_untagged_list cell_tag write buf l =
  let len = List.length l in
  Bi_vint.write_uvint buf len;
  if len > 0 then (
    Bi_io.write_tag buf cell_tag;
    list_iter2 write buf l
  )

let write_list cell_tag write buf l =
  Bi_io.write_tag buf Bi_io.array_tag;
  write_untagged_list cell_tag write buf l

(*
  shortcut for getting the tag of a polymorphic variant since
  biniou uses the same representation
  (usefulness?)
*)
let get_poly_tag (x : [> ]) =
  let r = Obj.repr x in
  if Obj.is_block r then
    (Obj.obj (Obj.field r 0) : int)
  else
    (Obj.obj r : int)

(* We want an identity function that is not inlined *)
type identity_t = { mutable _identity : 'a. 'a -> 'a }
let identity_ref = { _identity = (fun x -> x) }
let identity x = identity_ref._identity x

(*
  Checking at runtime that our assumptions on unspecified compiler behavior
  still hold.
*)

type t = {
  _a : int option;
  _b : int;
}

let create () =
  { { _a = None; _b = Array.length Sys.argv } with _a = None }

let test () =
  let r = create () in
  let v = Some 17 in
  Obj.set_field (Obj.repr r) 0 (Obj.repr v);
  let safe_r = identity r in
  (* r._a is inlined by ocamlopt and equals None
     because the field is supposed to be immutable. *)
  assert (safe_r._a = v)

let () = test ()

(************************************)
