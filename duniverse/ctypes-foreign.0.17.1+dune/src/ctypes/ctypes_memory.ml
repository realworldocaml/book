(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

[@@@warning "-9-27"]

open Ctypes_static

module Stubs = Ctypes_memory_stubs
module Raw = Ctypes_ptr.Raw
module Fat = Ctypes_ptr.Fat

let castp reftype (CPointer p) = CPointer (Fat.coerce p reftype)

let make_unmanaged ~reftyp p = Fat.make ~managed:None ~reftyp p

(* Describes how to read a value, e.g. from a return buffer *)
let rec build : type a b. a typ -> (_, b typ) Fat.t -> a
 = function
    | Void ->
      fun _ -> ()
    | Primitive p -> Stubs.read p
    | Struct { spec = Incomplete _ } ->
      raise IncompleteType
    | Struct { spec = Complete { size } } as reftyp ->
      (fun buf ->
        let p = Stubs.allocate 1 size in
        let dst = Fat.make ~managed:(Some (Obj.repr p)) ~reftyp (Stubs.block_address p) in
        let () = Stubs.memcpy ~size ~dst ~src:buf in
        { structured = CPointer dst})
    | Pointer reftyp ->
      (fun buf -> CPointer (make_unmanaged ~reftyp (Stubs.Pointer.read buf)))
    | Funptr fn ->
      (fun buf -> Static_funptr (make_unmanaged ~reftyp:fn (Stubs.Pointer.read buf)))
    | View { read; ty } ->
      let buildty = build ty in
      (fun buf -> read (buildty buf))
    | OCaml _ -> (fun buf -> assert false)
    (* The following cases should never happen; non-struct aggregate
       types are excluded during type construction. *)
    | Union _ -> assert false
    | Array _ -> assert false
    | Bigarray _ -> assert false
    | Abstract _ -> assert false

let rec write : type a b. a typ -> a -> (_, b) Fat.t -> unit
  = let write_aggregate size { structured = CPointer src } dst =
      Stubs.memcpy ~size ~dst ~src
    in
    function
    | Void -> (fun _ _ -> ())
    | Primitive p -> Stubs.write p
    | Pointer _ ->
      (fun (CPointer p) dst -> Stubs.Pointer.write p dst)
    | Funptr _ ->
      (fun (Static_funptr p) dst -> Stubs.Pointer.write p dst)
    | Struct { spec = Incomplete _ } -> raise IncompleteType
    | Struct { spec = Complete _ } as s -> write_aggregate (sizeof s)
    | Union { uspec = None } -> raise IncompleteType
    | Union { uspec = Some { size } } -> write_aggregate size
    | Abstract { asize } -> write_aggregate asize
    | Array _ as a ->
      let size = sizeof a in
      (fun { astart = CPointer src } dst ->
        Stubs.memcpy ~size ~dst ~src)
    | Bigarray b as t ->
      let size = sizeof t in
      (fun ba dst ->
        let src = Fat.make ~managed:ba ~reftyp:Void
          (Ctypes_bigarray.unsafe_address ba)
        in
        Stubs.memcpy ~size ~dst ~src)
    | View { write = w; ty } ->
      let writety = write ty in
      (fun v -> writety (w v))
    | OCaml _ -> raise IncompleteType

let null : unit ptr = CPointer (Fat.make ~managed:None ~reftyp:Void Raw.null)

let rec (!@) : type a. a ptr -> a
  = fun (CPointer cptr as ptr) ->
    match Fat.reftype cptr with
      | Void -> raise IncompleteType
      | Union { uspec = None } -> raise IncompleteType
      | Struct { spec = Incomplete _ } -> raise IncompleteType
      | View { read; ty } -> read (!@ (CPointer (Fat.coerce cptr ty)))
      (* If it's a reference type then we take a reference *)
      | Union _ -> { structured = ptr }
      | Struct _ -> { structured = ptr }
      | Array (elemtype, alength) ->
        { astart = CPointer (Fat.coerce cptr elemtype); alength }
      | Bigarray b -> Ctypes_bigarray.view b cptr
      | Abstract _ -> { structured = ptr }
      | OCaml _ -> raise IncompleteType
      (* If it's a value type then we cons a new value. *)
      | _ -> build (Fat.reftype cptr) cptr

let ptr_diff : type a b. (a, b) pointer -> (a, b) pointer -> int
  = fun l r ->
    match l, r with
    | CPointer lp, CPointer rp ->
      (* We assume the pointers are properly aligned, or at least that
         the difference is a multiple of sizeof reftype. *)
      Fat.diff_bytes lp rp / sizeof (Fat.reftype lp)
    | OCamlRef (lo, l, _), OCamlRef (ro, r, _) ->
      if l != r then invalid_arg "Ctypes.ptr_diff";
      ro - lo

let (+@) : type a b. (a, b) pointer -> int -> (a, b) pointer
  = fun p x ->
    match p with
    | CPointer p ->
      CPointer (Fat.add_bytes p (x * sizeof (Fat.reftype p)))
    | OCamlRef (offset, obj, ty) ->
      OCamlRef (offset + x, obj, ty)

let (-@) p x = p +@ (-x)

let (<-@) : type a. a ptr -> a -> unit
  = fun (CPointer p) ->
    fun v -> write (Fat.reftype p) v p

let from_voidp = castp
let to_voidp p = castp Void p

let allocate_n
  : type a. ?finalise:(a ptr -> unit) -> a typ -> count:int -> a ptr
  = fun ?finalise reftyp ~count ->
    let package p =
      CPointer (Fat.make ~managed:(Some (Obj.repr p)) ~reftyp (Stubs.block_address p))
    in
    let finalise = match finalise with
      | Some f -> Gc.finalise (fun p -> f (package p))
      | None -> ignore
    in
    let p = Stubs.allocate count (sizeof reftyp) in begin
      finalise p;
      package p
    end

let allocate : type a. ?finalise:(a ptr -> unit) -> a typ -> a -> a ptr
  = fun ?finalise reftype v ->
    let p = allocate_n ?finalise ~count:1 reftype in begin
      p <-@ v;
      p
    end

let ptr_compare (CPointer l) (CPointer r) = Fat.(compare l r)

let reference_type (CPointer p) = Fat.reftype p

let ptr_of_raw_address addr =
  CPointer (make_unmanaged ~reftyp:Void (Raw.of_nativeint addr))

let funptr_of_raw_address addr =
  Static_funptr (make_unmanaged ~reftyp:(void @-> returning void) (Raw.of_nativeint addr))

let raw_address_of_ptr (CPointer p) =
  (* This is unsafe by definition: if the object to which [p] refers
     is collected at this point then the returned address is invalid.
     If there is an OCaml object associated with [p] then it is vital
     that the caller retains a reference to it. *)
  Raw.to_nativeint (Fat.unsafe_raw_addr p)

module CArray =
struct
  type 'a t = 'a carray

  let check_bound { alength } i =
    if i < 0 || i >= alength then
      invalid_arg "index out of bounds"

  let unsafe_get { astart } n = !@(astart +@ n)
  let unsafe_set { astart } n v = (astart +@ n) <-@ v

  let get arr n =
    check_bound arr n;
    unsafe_get arr n

  let set arr n v =
    check_bound arr n;
    unsafe_set arr n v

  let start { astart } = astart
  let length { alength } = alength
  let from_ptr astart alength = { astart; alength }

  let fill ({ alength; astart = (CPointer p as _astart) } as _arr) v =
    let size = sizeof (Fat.reftype p) in
    let w = write (Fat.reftype p) v in
    for i = 0 to alength - 1 do
      w (Fat.add_bytes p (i * size))
    done

  let make : type a. ?finalise:(a t -> unit) -> a typ -> ?initial:a -> int -> a t
    = fun ?finalise reftype ?initial count ->
      let finalise = match finalise with
        | Some f -> Some (fun astart -> f { astart; alength = count } )
        | None -> None
      in
      let arr = { astart = allocate_n ?finalise ~count reftype;
                  alength = count } in
      match initial with
        | None -> arr
        | Some v -> fill arr v; arr

  let copy {astart = CPointer src; alength} =
    begin
      let reftyp = Fat.reftype src in
      let CPointer dst as r = allocate_n reftyp ~count:alength in
      let () = Stubs.memcpy ~dst ~src ~size:(alength * sizeof reftyp) in
      from_ptr r alength
    end

  let sub arr ~pos ~length:len =
  if pos < 0 || len < 0 || pos > length arr - len
  then invalid_arg "CArray.sub"
  else copy { astart = arr.astart +@ pos; alength = len }

  let element_type { astart } = reference_type astart

  let of_string string =
    let len = String.length string in
    let arr = make char (len + 1) in
    String.iteri (set arr) string;
    set arr len '\x00';
    arr

  let of_list typ list =
    let arr = make typ (List.length list) in
    List.iteri (set arr) list;
    arr

  let to_list a =
    let l = ref [] in
    for i = length a - 1 downto 0 do
      l := get a i :: !l
    done;
    !l

  let iter f a =
    for i = 0 to length a - 1 do
      f (unsafe_get a i)
    done

  let map typ f a =
    let l = length a in
    let r = make typ l in
    for i = 0 to l - 1 do
      unsafe_set r i (f (unsafe_get a i))
    done;
    r

  let mapi typ f a =
    let l = length a in
    let r = make typ l in
    for i = 0 to l - 1 do
      unsafe_set r i (f i (unsafe_get a i))
    done;
    r

  let fold_left f x a =
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f !r (unsafe_get a i)
    done;
    !r

  let fold_right f a x =
    let r = ref x in
    for i = length a - 1 downto 0 do
      r := f (unsafe_get a i) !r
    done;
    !r
end

let make ?finalise s =
  let finalise = match finalise with
    | Some f -> Some (fun structured -> f { structured })
    | None -> None in
  { structured = allocate_n ?finalise s ~count:1 }
let (|->) (CPointer p) { ftype; foffset } =
  CPointer (Fat.(add_bytes (Fat.coerce p ftype) foffset))

let (@.) { structured = p } f = p |-> f
let setf s field v = (s @. field) <-@ v
let getf s field = !@(s @. field)

let addr { structured } = structured

open Bigarray

let _bigarray_start kind ba =
  let raw_address = Ctypes_bigarray.unsafe_address ba in
  let reftyp = Primitive (Ctypes_bigarray.prim_of_kind kind) in
  CPointer (Fat.make ~managed:(Some (Obj.repr ba)) ~reftyp raw_address)

let bigarray_kind : type a b c d f l.
  < element: a;
    layout: l;
    ba_repr: f;
    bigarray: b;
    carray: c;
    dims: d > bigarray_class -> b -> (a, f) Bigarray.kind =
  function
  | Genarray -> Genarray.kind
  | Array1 -> Array1.kind
  | Array2 -> Array2.kind
  | Array3 -> Array3.kind

let bigarray_start spec ba = _bigarray_start (bigarray_kind spec ba) ba

let array_of_bigarray : type a b c d e.
  < element: a;
    layout: Bigarray.c_layout;
    ba_repr: e;
    bigarray: b;
    carray: c;
    dims: d > bigarray_class -> b -> c
  = fun spec ba ->
    let CPointer p as element_ptr =
      bigarray_start spec ba in
    match spec with
  | Genarray ->
    let ds = Genarray.dims ba in
    CArray.from_ptr element_ptr (Array.fold_left ( * ) 1 ds)
  | Array1 ->
    let d = Array1.dim ba in
    CArray.from_ptr element_ptr d
  | Array2 ->
    let d1 = Array2.dim1 ba and d2 = Array2.dim2 ba in
    CArray.from_ptr (castp (array d2 (Fat.reftype p)) element_ptr) d1
  | Array3 ->
    let d1 = Array3.dim1 ba and d2 = Array3.dim2 ba and d3 = Array3.dim3 ba in
    CArray.from_ptr (castp (array d2 (array d3 (Fat.reftype p))) element_ptr) d1

let bigarray_elements : type a b c d f l.
   < element: a;
     layout: l;
     ba_repr: f;
     bigarray: b;
     carray: c;
     dims: d > bigarray_class -> d -> int
  = fun spec dims -> match spec, dims with
   | Genarray, ds -> Array.fold_left ( * ) 1 ds
   | Array1, d -> d
   | Array2, (d1, d2) -> d1 * d2
   | Array3, (d1, d2, d3) -> d1 * d2 * d3

let bigarray_of_ptr spec dims kind ptr =
  !@ (castp (bigarray spec dims kind) ptr)

let fortran_bigarray_of_ptr spec dims kind ptr =
  !@ (castp (fortran_bigarray spec dims kind) ptr)

let array_dims : type a b c d f l.
   < element: a;
     layout: l;
     ba_repr: f;
     bigarray: b;
     carray: c carray;
     dims: d > bigarray_class -> c carray -> d =
   let unsupported () = raise (Unsupported "taking dimensions of non-array type") in
   fun spec a -> match spec with
   | Genarray -> [| a.alength |]
   | Array1 -> a.alength
   | Array2 ->
     begin match a.astart with
     | CPointer p ->
       begin match Fat.reftype p with
       | Array (_, n) -> (a.alength, n)
       | _ -> unsupported ()
       end
    end
   | Array3 ->
     begin match a.astart with
     | CPointer p ->
       begin match Fat.reftype p with
       |  Array (Array (_, m), n) -> (a.alength, n, m)
       | _ -> unsupported ()
       end
     end

let bigarray_of_array spec kind a =
  let dims = array_dims spec a in
  !@ (castp (bigarray spec dims kind) (CArray.start a))

let genarray = Genarray
let array1 = Array1
let array2 = Array2
let array3 = Array3
let typ_of_bigarray_kind k = Primitive (Ctypes_bigarray.prim_of_kind k)

let string_from_ptr (CPointer p) ~length:len =
  if len < 0 then invalid_arg "Ctypes.string_from_ptr"
  else Stubs.string_of_array p ~len

let ocaml_string_start str =
  OCamlRef (0, str, String)

let ocaml_bytes_start str =
  OCamlRef (0, str, Bytes)

let ocaml_float_array_start arr =
  OCamlRef (0, arr, FloatArray)

module Root =
struct
  module Stubs = Ctypes_roots_stubs

  (* Roots are not managed values so it's safe to call unsafe_raw_addr. *)
  let raw_addr : unit ptr -> Raw.t =
    fun (CPointer p) -> Fat.unsafe_raw_addr p

  let create : 'a. 'a -> unit ptr =
    fun v -> CPointer (make_unmanaged ~reftyp:void (Stubs.root v))

  let get : 'a. unit ptr -> 'a =
    fun p -> Stubs.get (raw_addr p)

  let set : 'a. unit ptr -> 'a -> unit =
    fun p v -> Stubs.set (raw_addr p) v
  
  let release : 'a. unit ptr -> unit =
    fun p -> Stubs.release (raw_addr p)
end

let is_null (CPointer p) = Fat.is_null p
