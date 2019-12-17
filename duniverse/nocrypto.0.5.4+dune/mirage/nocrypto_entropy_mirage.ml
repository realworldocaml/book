open Lwt

open Nocrypto
open Uncommon

module E = Entropy

type t = { e : E.t ; token : E.token ; g : Rng.g }

let attach e g =
  let `Acc acc = Rng.accumulate (Some g) in
  E.add_handler e acc

let active = ref None
and mx     = Lwt_mutex.create ()

let initialize () =
  Lwt_mutex.with_lock mx @@ fun () ->
    let g     = !Rng.generator in
    let reg e = attach e g >|= fun token -> active := Some { e ; token ; g } in
    match !active with
    | Some t when t.g == g -> return_unit
    | Some t               -> E.remove_handler t.e t.token ; reg t.e
    | None                 -> E.connect () >>= reg

let sources () =
  Option.map ~f:(fun { e; _ } -> E.sources e) !active
