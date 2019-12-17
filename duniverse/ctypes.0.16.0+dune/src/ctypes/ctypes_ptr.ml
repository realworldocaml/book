(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Boxed pointers to C memory locations . *)

[@@@warning "-9"]

module Raw = struct
  include Nativeint

  let of_nativeint x = x
  let to_nativeint x = x

  let null = zero
end

type voidp = Raw.t

module Fat :
sig
  (** A fat pointer, which holds a reference to the reference type, the C memory
      location, and an OCaml object. *)
  type (_,_) t

  (** [make ?managed ~reftyp raw] builds a fat pointer from the reference
      type [reftyp], the C memory location [raw], and (optionally) an OCaml
      value, [managed].  The [managed] argument may be used to manage the
      lifetime of the C object; a typical use it to attach a finaliser to
      [managed] which releases the memory associated with the C object whose
      address is stored in [raw_ptr]. *)
  val make : managed:'m -> reftyp:'typ -> voidp -> ('m,'typ) t

  val is_null : (_,_) t -> bool

  val reftype : (_,'typ) t -> 'typ

  val managed : ('m,_) t -> 'm

  val set_managed : ('m,_) t -> 'm -> unit

  val coerce : ('m,_) t -> 'typ -> ('m,'typ) t

  (** Return the raw pointer address.  The function is unsafe in the sense
      that it dissociates the address from the value which manages the memory,
      which may trigger associated finalisers, invalidating the address. *)
  val unsafe_raw_addr : (_,_) t -> voidp

  val add_bytes : ('m,'typ) t -> int -> ('m,'typ) t

  val compare : (_,'typ) t -> (_,'typ) t -> int

  val diff_bytes : (_,'typ) t -> (_,'typ) t -> int
end =
struct
  type ('m, 'typ) t =
    { reftyp  : 'typ;
      raw     : voidp;
      mutable managed : 'm; }

  let make ~managed ~reftyp raw = { reftyp; raw; managed }

  let is_null { raw } = Raw.(compare zero) raw = 0

  let reftype { reftyp } = reftyp

  let managed { managed } = managed

  let set_managed p m = p.managed <- m

  let coerce p reftyp = { p with reftyp }

  let unsafe_raw_addr { raw } = raw

  let add_bytes p bytes = { p with raw = Raw.(add p.raw (of_int bytes)) }

  let compare l r = Raw.compare l.raw r.raw

  let diff_bytes l r = Raw.(to_int (sub r.raw l.raw))
end
