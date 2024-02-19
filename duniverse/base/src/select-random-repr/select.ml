let () =
  let ver, output =
    try
      match Sys.argv with
      | [|_; "-ocaml-version"; v; "-o"; fn|] ->
        (Scanf.sscanf v "%d.%d" (fun major minor -> (major, minor)),
         fn)
      | _ -> raise Exit
    with _ ->
      failwith "bad command line arguments"
  in
  let oc = open_out output in
  if ver >= (5, 0) then
    Printf.fprintf oc {|
module Repr = struct
  open Caml.Bigarray

  type t = (int64, int64_elt, c_layout) Array1.t

  let of_state : Caml.Random.State.t -> t = Caml.Obj.magic
end

let assign dst src =
  let dst = Repr.of_state (Lazy.force dst) in
  let src = Repr.of_state (Lazy.force src) in
  Caml.Bigarray.Array1.blit src dst

let make_default default =
  let split_from_parent v =
    Caml.Lazy.map_val Caml.Random.State.split v
  in
  Caml.Domain.DLS.new_key ~split_from_parent (fun () -> default)

let get_state random_key = Caml.Domain.DLS.get random_key
|}
  else
    Printf.fprintf oc {|
module Array = Array0

module Repr = struct
  type t =
    { st : int array
    ; mutable idx : int
    }

  let of_state : Caml.Random.State.t -> t = Caml.Obj.magic
end

let assign t1 t2 =
  let t1 = Repr.of_state (Lazy.force t1) in
  let t2 = Repr.of_state (Lazy.force t2) in
  Array.blit ~src:t2.st ~src_pos:0 ~dst:t1.st ~dst_pos:0 ~len:(Array.length t1.st);
  t1.idx <- t2.idx

let make_default default = default

let[@inline always] get_state state = state
|};
  close_out oc
