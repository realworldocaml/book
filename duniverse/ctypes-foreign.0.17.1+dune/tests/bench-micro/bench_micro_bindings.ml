module type FOREIGN = sig
  type 'a fn
  val foreign : string -> ('a -> 'b) Ctypes.fn -> ('a -> 'b) fn
end

type int5  = int -> int -> int -> int -> int -> int
type int10 = int -> int -> int -> int -> int -> int5

module type API = sig
  type 'a fn

  val f_i0 : (unit -> int) fn
  val f_i1 : (int  -> int) fn
  val f_i2 : (int  -> int -> int) fn
  val f_i3 : (int  -> int -> int -> int) fn
  val f_i4 : (int  -> int -> int -> int -> int) fn
  val f_i5 : int5 fn
  val f_i6 : (int  -> int5) fn
  val f_i7 : (int  -> int -> int5) fn
  val f_i8 : (int  -> int -> int -> int5) fn
  val f_i9 : (int  -> int -> int -> int -> int5) fn
  val f_i10: int10 fn
  val f_i11: (int  -> int10) fn
  val f_i12: (int  -> int -> int10) fn
  val f_i13: (int  -> int -> int -> int10) fn
  val f_i14: (int  -> int -> int -> int -> int10) fn
  val f_i15: (int  -> int -> int -> int -> int -> int10) fn
end

module Interpreter_local : FOREIGN with type 'a fn = 'a = struct
  type 'a fn = 'a

  external f_i0_ptr : unit -> nativeint = "f_i0_ptr"
  external f_i1_ptr : unit -> nativeint = "f_i1_ptr"
  external f_i2_ptr : unit -> nativeint = "f_i2_ptr"
  external f_i3_ptr : unit -> nativeint = "f_i3_ptr"
  external f_i4_ptr : unit -> nativeint = "f_i4_ptr"
  external f_i5_ptr : unit -> nativeint = "f_i5_ptr"
  external f_i6_ptr : unit -> nativeint = "f_i6_ptr"
  external f_i7_ptr : unit -> nativeint = "f_i7_ptr"
  external f_i8_ptr : unit -> nativeint = "f_i8_ptr"
  external f_i9_ptr : unit -> nativeint = "f_i9_ptr"
  external f_i10_ptr: unit -> nativeint = "f_i10_ptr"
  external f_i11_ptr: unit -> nativeint = "f_i11_ptr"
  external f_i12_ptr: unit -> nativeint = "f_i12_ptr"
  external f_i13_ptr: unit -> nativeint = "f_i13_ptr"
  external f_i14_ptr: unit -> nativeint = "f_i14_ptr"
  external f_i15_ptr: unit -> nativeint = "f_i15_ptr"

  let foreign name fn =
    let f_addr = match name with
      | "f_i0" -> f_i0_ptr ()
      | "f_i1" -> f_i1_ptr ()
      | "f_i2" -> f_i2_ptr ()
      | "f_i3" -> f_i3_ptr ()
      | "f_i4" -> f_i4_ptr ()
      | "f_i5" -> f_i5_ptr ()
      | "f_i6" -> f_i6_ptr ()
      | "f_i7" -> f_i7_ptr ()
      | "f_i8" -> f_i8_ptr ()
      | "f_i9" -> f_i9_ptr ()
      | "f_i10"-> f_i10_ptr ()
      | "f_i11"-> f_i11_ptr ()
      | "f_i12"-> f_i12_ptr ()
      | "f_i13"-> f_i13_ptr ()
      | "f_i14"-> f_i14_ptr ()
      | "f_i15"-> f_i15_ptr ()
      | _      -> assert false
    in
    Ctypes.(coerce (ptr void) (Foreign.funptr fn) (ptr_of_raw_address f_addr))
end

module Interpreter_shared : FOREIGN with type 'a fn = 'a = struct
  type 'a fn = 'a

  let foreign name fn = Foreign.foreign name fn
end

module Make (F : FOREIGN) : API with type 'a fn = 'a F.fn = struct
  open Ctypes

  type 'a fn = 'a F.fn

  let plus_int5 r = int @-> int @-> int @-> int @-> int @-> r
  let int5 = plus_int5 (returning int)
  let int10= plus_int5 int5

  let f_i0 = F.foreign "f_i0" @@ void @-> returning int
  let f_i1 = F.foreign "f_i1" @@ int  @-> returning int
  let f_i2 = F.foreign "f_i2" @@ int  @-> int @-> returning int
  let f_i3 = F.foreign "f_i3" @@ int  @-> int @-> int @-> returning int
  let f_i4 = F.foreign "f_i4" @@ int  @-> int @-> int @-> int @-> returning int
  let f_i5 = F.foreign "f_i5" @@ int5
  let f_i6 = F.foreign "f_i6" @@ int  @-> int5
  let f_i7 = F.foreign "f_i7" @@ int  @-> int @-> int5
  let f_i8 = F.foreign "f_i8" @@ int  @-> int @-> int @-> int5
  let f_i9 = F.foreign "f_i9" @@ int  @-> int @-> int @-> int @-> int5
  let f_i10= F.foreign "f_i10"@@ int10
  let f_i11= F.foreign "f_i11"@@ int  @-> int10
  let f_i12= F.foreign "f_i12"@@ int  @-> int @-> int10
  let f_i13= F.foreign "f_i13"@@ int  @-> int @-> int @-> int10
  let f_i14= F.foreign "f_i14"@@ int  @-> int @-> int @-> int @-> int10
  let f_i15= F.foreign "f_i15"@@ int  @-> int @-> int @-> int @-> int @-> int10

end

module Traditional : API with type 'a fn = 'a = struct
  type 'a fn = 'a

  external f_i0 : unit -> int = "f_i0_caml"
  external f_i1 : int  -> int = "f_i1_caml"
  external f_i2 : int  -> int -> int = "f_i2_caml"
  external f_i3 : int  -> int -> int -> int = "f_i3_caml"
  external f_i4 : int  -> int -> int -> int -> int = "f_i4_caml"
  external f_i5 : int  -> int -> int -> int -> int -> int = "f_i5_caml"
  external f_i6 : int  -> int -> int -> int -> int -> int -> int = "f_i6_caml_byte" "f_i6_caml"
  external f_i7 : int  -> int -> int -> int -> int -> int -> int -> int = "f_i7_caml_byte" "f_i7_caml"
  external f_i8 : int  -> int -> int -> int -> int -> int -> int -> int -> int = "f_i8_caml_byte" "f_i8_caml"
  external f_i9 : int  -> int -> int -> int -> int -> int -> int -> int -> int -> int = "f_i9_caml_byte" "f_i9_caml"
  external f_i10: int  -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = "f_i10_caml_byte" "f_i10_caml"
  external f_i11: int  -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = "f_i11_caml_byte" "f_i11_caml"
  external f_i12: int  -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = "f_i12_caml_byte" "f_i12_caml"
  external f_i13: int  -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = "f_i13_caml_byte" "f_i13_caml"
  external f_i14: int  -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = "f_i14_caml_byte" "f_i14_caml"
  external f_i15: int  -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = "f_i15_caml_byte" "f_i15_caml"
end

module Cowboy : API with type 'a fn = 'a = struct
  type 'a fn = 'a

  external f_i0 : unit -> int = "f_i0_cowboy" "noalloc"
  external f_i1 : int  -> int = "f_i1_cowboy" "noalloc"
  external f_i2 : int  -> int -> int = "f_i2_cowboy" "noalloc"
  external f_i3 : int  -> int -> int -> int = "f_i3_cowboy" "noalloc"
  external f_i4 : int  -> int -> int -> int -> int = "f_i4_cowboy" "noalloc"
  external f_i5 : int  -> int -> int -> int -> int -> int = "f_i5_cowboy" "noalloc"
  external f_i6 : int  -> int -> int -> int -> int -> int -> int = "f_i6_cowboy_byte" "f_i6_cowboy" "noalloc"
  external f_i7 : int  -> int -> int -> int -> int -> int -> int -> int = "f_i7_cowboy_byte" "f_i7_cowboy" "noalloc"
  external f_i8 : int  -> int -> int -> int -> int -> int -> int -> int -> int = "f_i8_cowboy_byte" "f_i8_cowboy" "noalloc"
  external f_i9 : int  -> int -> int -> int -> int -> int -> int -> int -> int -> int = "f_i9_cowboy_byte" "f_i9_cowboy" "noalloc"
  external f_i10: int  -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = "f_i10_cowboy_byte" "f_i10_cowboy" "noalloc"
  external f_i11: int  -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = "f_i11_cowboy_byte" "f_i11_cowboy" "noalloc"
  external f_i12: int  -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = "f_i12_cowboy_byte" "f_i12_cowboy" "noalloc"
  external f_i13: int  -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = "f_i13_cowboy_byte" "f_i13_cowboy" "noalloc"
  external f_i14: int  -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = "f_i14_cowboy_byte" "f_i14_cowboy" "noalloc"
  external f_i15: int  -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = "f_i15_cowboy_byte" "f_i15_cowboy" "noalloc"
end
