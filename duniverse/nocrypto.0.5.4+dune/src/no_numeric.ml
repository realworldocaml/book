open No_uncommon

type bits = int

module Z0 = Z

module type S_core = sig
  type t
  val zero : t
  val one  : t
  val (lsr)  : t -> int -> t
  val (lsl)  : t -> int -> t
  val (land) : t -> t -> t
  val (lor)  : t -> t -> t
  val (lxor) : t -> t -> t
  val (+)  : t -> t -> t
  val (-)  : t -> t -> t
  val succ : t -> t
  val pred : t -> t
  val of_int   : int -> t
  val of_int32 : int32 -> t
  val of_int64 : int64 -> t
  val to_int   : t -> int
  val to_int32 : t -> int32
  val to_int64 : t -> int64
  val bit_bound : t -> int (* XXX get rid of this from the interface *)
  val pp_print : Format.formatter -> t -> unit
end

module type S = sig
  include S_core
  val bits            : t -> int
  val of_cstruct_be   : ?bits:int -> Cstruct.t -> t
  val to_cstruct_be   : ?size:int -> t -> Cstruct.t
  val into_cstruct_be : t -> Cstruct.t -> unit
end

module Int_core = struct
  type t = int
  let bit_bound _ = Sys.word_size
  let zero = 0 and one  = 1
  let (lsr)  = (lsr)
  let (lsl)  = (lsl)
  let (land) = (land)
  let (lor)  = (lor)
  let (lxor) = (lxor)
  let (+)  = (+)
  let (-)  = (-)
  let succ = succ
  let pred = pred
  let of_int    = id
  let of_int32  = Int32.to_int
  let of_int64  = Int64.to_int
  let to_int    = id
  let to_int32  = Int32.of_int
  let to_int64  = Int64.of_int
  let pp_print = Format.pp_print_int
end

module Int32_core = struct
  include Int32
  let bit_bound _ = 32
  let (lsr)  = shift_right_logical
  let (lsl)  = shift_left
  let (land) = logand
  let (lor)  = logor
  let (lxor) = logxor
  let (+)    = add
  let (-)    = sub
  let of_int32 = id
  let of_int64 = Int64.to_int32
  let to_int32 = id
  let to_int64 = Int64.of_int32
  let pp_print f x = Format.pp_print_string f (to_string x)
end

module Int64_core = struct
  include Int64
  let bit_bound _ = 64
  let (lsr)  = shift_right_logical
  let (lsl)  = shift_left
  let (land) = logand
  let (lor)  = logor
  let (lxor) = logxor
  let (+)    = add
  let (-)    = sub
  let of_int64 = id
  let to_int64 = id
  let pp_print f x = Format.pp_print_string f (to_string x)
end

module Z_core = struct
  let bit_bound z = Z.size z * 64
  include Z
  let (lsr) = shift_right
  let (lsl) = shift_left
end


module Repr (N : S_core) = struct

  (* If there was only, like, an instruction doing `ceil (log2 n)`... *)
  let bits i =
    if i < N.zero then invalid_arg "Numeric.Repr.bits: %a" N.pp_print i;
    let rec scan acc bound = function
      | i when i = N.zero -> acc
      | i when i = N.one  -> acc + 1
      | i ->
          let mid   = bound / 2 in
          let upper = N.(i lsr mid) in
          if upper = N.zero then
            scan acc (bound - mid) i
          else scan (acc + mid) (bound - mid) upper in
    scan 0 N.(bit_bound i) i

  let of_cstruct_be ?bits cs =
    let open Cstruct in
    let open BE in
    let rec loop acc i = function
      | b when b >= 64 ->
          let x = get_uint64 cs i in
          let x = N.of_int64 Int64.(shift_right_logical x 8) in
          loop N.(x + acc lsl 56) (i + 7) (b - 56)
      | b when b >= 32 ->
          let x = get_uint32 cs i in
          let x = N.of_int32 Int32.(shift_right_logical x 8) in
          loop N.(x + acc lsl 24) (i + 3) (b - 24)
      | b when b >= 16 ->
          let x = N.of_int (get_uint16 cs i) in
          loop N.(x + acc lsl 16) (i + 2) (b - 16)
      | b when b >= 8  ->
          let x = N.of_int (get_uint8 cs i) in
          loop N.(x + acc lsl 8 ) (i + 1) (b - 8 )
      | b when b > 0   ->
          let x = get_uint8 cs i and b' = 8 - b in
          N.(of_int x lsr b' + acc lsl b)
      | _              -> acc in
    loop N.zero 0 @@ match bits with
      | None   -> Cstruct.len cs * 8
      | Some b -> imin b (Cstruct.len cs * 8)

  let byte1 = N.of_int64 0xffL
  and byte2 = N.of_int64 0xffffL
  and byte3 = N.of_int64 0xffffffL
  and byte7 = N.of_int64 0xffffffffffffffL

  let into_cstruct_be n cs =
    let open Cstruct in
    let open BE in

    let rec write n = function
      | i when i >= 7 ->
          set_uint64 cs (i - 7) N.(to_int64 (n land byte7)) ;
          write N.(n lsr 56) (i - 7)
      | i when i >= 3 ->
          set_uint32 cs (i - 3) N.(to_int32 (n land byte3)) ;
          write N.(n lsr 24) (i - 3)
      | i when i >= 1 ->
          set_uint16 cs (i - 1) N.(to_int (n land byte2)) ;
          write N.(n lsr 16) (i - 2)
      | 0 -> set_uint8 cs 0 N.(to_int (n land byte1)) ;
      | _ -> ()
    in
    write n (len cs - 1)

  let to_cstruct_be ?size n =
    let cs = Cstruct.create_unsafe @@ match size with
      | Some s -> imax 0 s
      | None   -> bits n // 8 in
    ( into_cstruct_be n cs ; cs )

end

module S (N : S_core) : S with type t = N.t = struct
  include N
  include Repr (N)
end

module Int   = S (Int_core  )
module Int32 = S (Int32_core)
module Int64 = S (Int64_core)
module Z     = S (Z_core    )


(* Handbook of Applied Cryptography, Table 4.4:
 * Miller-Rabin rounds for composite probability <= 1/2^80. *)
let pseudoprime z =
  let i = match Z.bits z with
    | i when i >= 1300 ->  2
    | i when i >=  850 ->  3
    | i when i >=  650 ->  4
    | i when i >=  350 ->  8
    | i when i >=  250 -> 12
    | i when i >=  150 -> 18
    | _                -> 27 in
  Z0.probab_prime z i <> 0

(* strip_factor ~f x = (s, t), where x = f^s t *)
let strip_factor ~f x =
  let rec go n x =
    let (x1, r) = Z0.div_rem x f in
    if r = Z0.zero then go (succ n) x1 else (n, x) in
  if Z0.two <= f then go 0 x else invalid_arg "factor_count: f: %a" Z0.pp f
