open Lwt.Infix
open Nocrypto

let chunk  = 32
and period = 30
and device = Nocrypto_entropy_unix.sys_rng

let mvar_map v f =
  Lwt_mvar.take v >>= fun x ->
    Lwt.catch (fun () -> f x >>= Lwt_mvar.put v)
              (fun exn -> Lwt_mvar.put v x >>= fun () -> Lwt.fail exn)

let some x = Some x

[@@@ocaml.warning "-3"]

type t = {
  fd     : Lwt_unix.file_descr ;
  remove : (unit -> unit) Lwt_sequence.node ;
  g      : Rng.g
}

let background ~period f =
  let last   = ref Unix.(gettimeofday ())
  and live   = ref false
  and period = float period in
  fun () ->
    let t = Unix.gettimeofday () in
    if (not !live) && (t -. !last >= period) then begin
      last := t ;
      live := true ;
      Lwt.async @@ fun () -> f () >|= fun () -> live := false
    end

let rec read_cs fd cs =
  Lwt_bytes.read fd cs.Cstruct.buffer cs.Cstruct.off cs.Cstruct.len >>=
    function 0 -> Lwt.return_unit | n -> read_cs fd (Cstruct.shift cs n)

let attach ~period ?(device = device) g =
  Lwt_unix.(openfile device [O_RDONLY] 0) >|= fun fd ->
  let buf = Cstruct.create chunk in
  let seed () = read_cs fd buf >|= fun () -> Rng.reseed ~g buf in
  let remove =
      Lwt_sequence.add_r (background ~period seed) Lwt_main.enter_iter_hooks in
  { g ; fd ; remove }

let stop t =
  Lwt_sequence.remove t.remove ;
  Lwt.(catch (fun () -> Lwt_unix.close t.fd)
    Unix.(function Unix_error (EBADF, _, _) -> return_unit | exn -> fail exn))

let active = Lwt_mvar.create None

let initialize () =
  Nocrypto_entropy_unix.initialize () ;
  let g = !Rng.generator in
  mvar_map active @@ function
    | Some t when t.g == g -> Lwt.return_some t
    | Some t               -> stop t >>= fun () -> attach ~period g >|= some
    | None                 -> attach ~period g >|= some
