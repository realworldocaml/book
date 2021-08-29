module type S = sig
  type mac = Cstruct.t
  type 'a iter = 'a Uncommon.iter

  type t
  val mac_size : int

  val empty : key:Cstruct.t -> t
  val feed : t -> Cstruct.t -> t
  val feedi : t -> Cstruct.t iter -> t
  val get : t -> Cstruct.t

  val mac : key:Cstruct.t -> Cstruct.t -> mac
  val maci : key:Cstruct.t -> Cstruct.t iter -> mac
end

module It : S = struct
  type mac = Cstruct.t
  type 'a iter = 'a Uncommon.iter

  module P = Native.Poly1305
  let mac_size = P.mac_size ()

  type t = Native.ctx

  let dup = Bytes.copy

  let empty ~key:{ Cstruct.buffer ; off ; len } =
    let ctx = Bytes.create (P.ctx_size ()) in
    if len <> 32 then invalid_arg "Poly1305 key must be 32 bytes" ;
    P.init ctx buffer off ;
    ctx

  let update ctx { Cstruct.buffer ; off ; len } =
    P.update ctx buffer off len

  let feed ctx cs =
    let t = dup ctx in
    update t cs ;
    t

  let feedi ctx iter =
    let t = dup ctx in
    iter (update t) ;
    t

  let final ctx =
    let res = Cstruct.create mac_size in
    P.finalize ctx res.buffer res.off;
    res

  let get ctx = final (dup ctx)

  let mac ~key data = feed (empty ~key) data |> final

  let maci ~key iter = feedi (empty ~key) iter |> final
end
