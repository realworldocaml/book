type type_id = int

let dummy_type_id = 0

let create_type_id =
  let n = ref dummy_type_id in
  fun () ->
    incr n;
    if !n < 0 then
      failwith "Bi_share.Rd_poly.create_type_id: \
                exhausted available type_id's"
    else
      !n

module Wr =
struct
  module H = Hashtbl.Make (
    struct
      type t = Obj.t * type_id
      let equal (x1, t1) (x2, t2) = x1 == x2 && t1 == t2
      let hash = Hashtbl.hash
    end
  )

  type tbl = int H.t

  let create = H.create
  let clear tbl =
    if H.length tbl > 0 then
      H.clear tbl

  let put tbl k pos =
    try
      let pos0 = H.find tbl (Obj.magic k) in
      pos - pos0
    with Not_found ->
      H.add tbl (Obj.magic k) pos;
      0
end

module Rd =
struct
  type tbl = ((int * type_id), Obj.t) Hashtbl.t

  let create n = Hashtbl.create n
  let clear = Hashtbl.clear

  let put tbl pos x =
    Hashtbl.add tbl pos x

  let get tbl pos =
    try Hashtbl.find tbl pos
    with Not_found ->
      Bi_util.error "Corrupted data (invalid reference)"
end
