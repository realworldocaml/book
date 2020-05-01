(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

type dyn = ..
type 'a var = ('a -> dyn) * (dyn -> 'a option)

let variant (type a) () =
  let module M = struct type dyn += K of a end in
  (fun x -> M.K x), (function M.K x -> Some x | _ -> None)
let inj = fst and prj = snd

module Make (KV: sig
  type 'a k and 'a v
  val mapv : ('a -> 'b) -> 'a v -> 'b v
end) =
struct
  type k = K : 'a KV.k -> k
  type t = (k, dyn KV.v) Hashtbl.t
  let create () = Hashtbl.create 7
  let prj_ var d = match prj var d with Some x -> x | _ -> assert false
  let intern t var k v =
    let k = K k in
    try Hashtbl.find t k |> KV.mapv (prj_ var) with Not_found ->
      KV.mapv (inj var) v |> Hashtbl.add t k ; v
end
