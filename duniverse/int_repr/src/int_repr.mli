module type T = sig
  type t [@@deriving compare, equal, hash, quickcheck, sexp, typerep]

  val signed : bool
  val num_bits : int
  val num_bytes : int
  val zero : t
  val min_value : t
  val max_value : t

  include Ppx_hash_lib.Hashable.S with type t := t
  include Base.Comparisons.S with type t := t

  module O : sig
    include Base.Comparisons.Infix with type t := t

    module Wrap : sig
      val ( + ) : t -> t -> t
      val ( - ) : t -> t -> t
      val ( * ) : t -> t -> t
      val ( / ) : t -> t -> t
    end
  end

end

type int8 = private Base.Int.t
type uint8 = private Base.Int.t
type int16 = private Base.Int.t
type uint16 = private Base.Int.t
type int32 [@@immediate64]
type uint32 [@@immediate64]
type int63 = Base.Int63.t
type uint63 = private Base.Int63.t
type int64 = Base.Int64.t
type uint64 = private Base.Int64.t

module Int8 : sig
  type t = int8 [@@immediate]

  include T with type t := t

  (* "Base" conversions. *)
  val of_base_int_trunc : Base.Int.t -> t
  val of_base_int_exn : Base.Int.t -> t
  val to_base_int : t -> Base.Int.t

  (* Same-signedness conversions. *)
  val of_int16_trunc : int16 -> t
  val of_int16_exn : int16 -> t
  val of_int32_trunc : int32 -> t
  val of_int32_exn : int32 -> t
  val of_int63_trunc : int63 -> t
  val of_int63_exn : int63 -> t
  val of_int64_trunc : int64 -> t
  val of_int64_exn : int64 -> t

  (* Same-width conversions. *)
  val of_uint8_wrap : uint8 -> t
  val of_uint8_exn : uint8 -> t
end

module Uint8 : sig
  type t = uint8 [@@immediate]

  include T with type t := t

  (* "Base" conversions. *)
  val of_base_int_trunc : Base.Int.t -> t
  val of_base_int_exn : Base.Int.t -> t
  val to_base_int : t -> Base.Int.t

  (* Same-signedness conversions. *)
  val of_uint16_trunc : uint16 -> t
  val of_uint16_exn : uint16 -> t
  val of_uint32_trunc : uint32 -> t
  val of_uint32_exn : uint32 -> t
  val of_uint63_trunc : uint63 -> t
  val of_uint63_exn : uint63 -> t
  val of_uint64_trunc : uint64 -> t
  val of_uint64_exn : uint64 -> t

  (* Same-width conversions. *)
  val of_int8_wrap : int8 -> t
  val of_int8_exn : int8 -> t
end

module Int16 : sig
  type t = int16 [@@immediate]

  include T with type t := t

  (* "Base" conversions. *)
  val of_base_int_trunc : Base.Int.t -> t
  val of_base_int_exn : Base.Int.t -> t
  val to_base_int : t -> Base.Int.t

  (* Same-signedness conversions. *)
  val of_int8 : int8 -> t
  val of_int32_trunc : int32 -> t
  val of_int32_exn : int32 -> t
  val of_int63_trunc : int63 -> t
  val of_int63_exn : int63 -> t
  val of_int64_trunc : int64 -> t
  val of_int64_exn : int64 -> t

  (* Same-width conversions. *)
  val of_uint16_wrap : uint16 -> t
  val of_uint16_exn : uint16 -> t
end

module Uint16 : sig
  type t = uint16 [@@immediate]

  include T with type t := t

  (* "Base" conversions. *)
  val of_base_int_trunc : Base.Int.t -> t
  val of_base_int_exn : Base.Int.t -> t
  val to_base_int : t -> Base.Int.t

  (* Same-signedness conversions. *)
  val of_uint8 : uint8 -> t
  val of_uint32_trunc : uint32 -> t
  val of_uint32_exn : uint32 -> t
  val of_uint63_trunc : uint63 -> t
  val of_uint63_exn : uint63 -> t
  val of_uint64_trunc : uint64 -> t
  val of_uint64_exn : uint64 -> t

  (* Same-width conversions. *)
  val of_int16_wrap : int16 -> t
  val of_int16_exn : int16 -> t
end

module Int32 : sig
  type t = int32

  include T with type t := t

  (* "Base" conversions. *)
  val of_base_int32 : Base.Int32.t -> t
  val to_base_int32 : t -> Base.Int32.t

  (* Same-signedness conversions. *)
  val of_int8 : int8 -> t
  val of_int16 : int16 -> t
  val of_int63_trunc : int63 -> t
  val of_int63_exn : int63 -> t
  val of_int64_trunc : int64 -> t
  val of_int64_exn : int64 -> t

  (* Same-width conversions. *)
  val of_uint32_wrap : uint32 -> t
  val of_uint32_exn : uint32 -> t
end

module Uint32 : sig
  type t = uint32

  include T with type t := t

  (* "Base" conversions. *)
  val of_base_int32_trunc : Base.Int32.t -> t
  val of_base_int32_exn : Base.Int32.t -> t
  val to_base_int32_trunc : t -> Base.Int32.t
  val to_base_int32_exn : t -> Base.Int32.t
  val of_base_int64_trunc : Base.Int64.t -> t
  val of_base_int64_exn : Base.Int64.t -> t
  val to_base_int64 : t -> Base.Int64.t
  val to_base_int_exn : t -> Base.Int.t

  (* Same-signedness conversions. *)
  val of_uint8 : uint8 -> t
  val of_uint16 : uint16 -> t
  val of_uint63_trunc : uint63 -> t
  val of_uint63_exn : uint63 -> t
  val of_uint64_trunc : uint64 -> t
  val of_uint64_exn : uint64 -> t

  (* Same-width conversions. *)
  val of_int32_wrap : int32 -> t
  val of_int32_exn : int32 -> t
end

module Int63 : sig
  type t = int63 [@@immediate64]

  include T with type t := t

  (* Same-signedness conversions. *)
  val of_int8 : int8 -> t
  val of_int16 : int16 -> t
  val of_int32 : int32 -> t
  val of_int64_trunc : int64 -> t
  val of_int64_exn : int64 -> t

  (* Same-width conversions. *)
  val of_uint63_wrap : uint63 -> t
  val of_uint63_exn : uint63 -> t
end

module Uint63 : sig
  type t = uint63

  include T with type t := t

  (* "Base" conversions. *)
  val of_base_int64_trunc : Base.Int64.t -> t
  val of_base_int64_exn : Base.Int64.t -> t
  val to_base_int64 : t -> Base.Int64.t

  (* Same-signedness conversions. *)
  val of_uint8 : uint8 -> t
  val of_uint16 : uint16 -> t
  val of_uint32 : uint32 -> t
  val of_uint64_trunc : uint64 -> t
  val of_uint64_exn : uint64 -> t

  (* Same-width conversions. *)
  val of_int63_wrap : int63 -> t
  val of_int63_exn : int63 -> t
end

module Int64 : sig
  type t = int64

  include T with type t := t

  (* Same-signedness conversions. *)
  val of_int8 : int8 -> t
  val of_int16 : int16 -> t
  val of_int32 : int32 -> t
  val of_int63 : int63 -> t

  (* Same-width conversions. *)
  val of_uint64_wrap : uint64 -> t
  val of_uint64_exn : uint64 -> t
end

module Uint64 : sig
  type t = uint64

  include T with type t := t

  (* "Base" conversions. *)
  val of_base_int64_trunc : Base.Int64.t -> t
  val of_base_int64_exn : Base.Int64.t -> t
  val to_base_int64_trunc : t -> Base.Int64.t
  val to_base_int64_exn : t -> Base.Int64.t

  (* Same-signedness conversions. *)
  val of_uint8 : uint8 -> t
  val of_uint16 : uint16 -> t
  val of_uint32 : uint32 -> t
  val of_uint63 : uint63 -> t

  (* Same-width conversions. *)
  val of_int64_wrap : int64 -> t
  val of_int64_exn : int64 -> t
end

module type Get = sig
  type t

  (** {2 8-bit signed values} *)

  val get_int8 : t -> pos:int -> int8

  (** {2 8-bit unsigned values} *)

  val get_uint8 : t -> pos:int -> uint8

  (** {2 16-bit signed values} *)

  val get_int16_le : t -> pos:int -> int16
  val get_int16_be : t -> pos:int -> int16

  (** {2 16-bit unsigned values} *)

  val get_uint16_le : t -> pos:int -> uint16
  val get_uint16_be : t -> pos:int -> uint16

  (** {2 32-bit signed values} *)

  val get_int32_le : t -> pos:int -> int32
  val get_int32_be : t -> pos:int -> int32

  (** {2 32-bit unsigned values} *)

  val get_uint32_le : t -> pos:int -> uint32
  val get_uint32_be : t -> pos:int -> uint32

  (** {2 64-bit signed values} *)

  val get_int64_le : t -> pos:int -> int64
  val get_int64_be : t -> pos:int -> int64

  (** {2 64-bit unsigned values} *)

  val get_uint64_le : t -> pos:int -> uint64
  val get_uint64_be : t -> pos:int -> uint64
end

module type Set = sig
  type t

  (** {2 8-bit signed values} *)

  val set_int8 : t -> pos:int -> int8 -> unit

  (** {2 8-bit unsigned values} *)

  val set_uint8 : t -> pos:int -> uint8 -> unit

  (** {2 16-bit signed values} *)

  val set_int16_le : t -> pos:int -> int16 -> unit
  val set_int16_be : t -> pos:int -> int16 -> unit

  (** {2 16-bit unsigned values} *)

  val set_uint16_le : t -> pos:int -> uint16 -> unit
  val set_uint16_be : t -> pos:int -> uint16 -> unit

  (** {2 32-bit signed values} *)

  val set_int32_le : t -> pos:int -> int32 -> unit
  val set_int32_be : t -> pos:int -> int32 -> unit

  (** {2 32-bit unsigned values} *)

  val set_uint32_le : t -> pos:int -> uint32 -> unit
  val set_uint32_be : t -> pos:int -> uint32 -> unit

  (** {2 64-bit signed values} *)

  val set_int64_le : t -> pos:int -> int64 -> unit
  val set_int64_be : t -> pos:int -> int64 -> unit

  (** {2 64-bit unsigned values} *)

  val set_uint64_le : t -> pos:int -> uint64 -> unit
  val set_uint64_be : t -> pos:int -> uint64 -> unit
end

module type Get_functions = sig
  type t

  (* The following functions must use native endianness (hence the `_ne` suffix). *)
  val get_uint8 : t -> int -> Base.Int.t
  val get_uint16_ne : t -> int -> Base.Int.t
  val get_int32_ne : t -> int -> Base.Int32.t
  val get_int64_ne : t -> int -> Base.Int64.t
end

module type Set_functions = sig
  type t

  (* The following functions must use native endianness (hence the `_ne` suffix). *)
  val set_uint8 : t -> int -> Base.Int.t -> unit
  val set_uint16_ne : t -> int -> Base.Int.t -> unit
  val set_int32_ne : t -> int -> Base.Int32.t -> unit
  val set_int64_ne : t -> int -> Base.Int64.t -> unit
end

module Make_get (F : Get_functions) : Get with type t := F.t
module Make_set (F : Set_functions) : Set with type t := F.t

module Bytes : sig
  include Get with type t := Bytes.t
  include Set with type t := Bytes.t

  module Unsafe : sig
    include Get with type t := Bytes.t
    include Set with type t := Bytes.t
  end
end

module String : sig
  include Get with type t := String.t

  module Unsafe : sig
    include Get with type t := String.t
  end
end
