open Lwt.Infix

module C = Domainslib.Chan
module T = Domainslib.Task

type pool = Domainslib.Task.pool

let setup_pool ?name num_additional_domains =
    T.setup_pool ?name ~num_additional_domains ()

let teardown_pool = T.teardown_pool

let lookup_pool = T.lookup_pool

let get_num_domains = T.get_num_domains

let init_result = Error (Failure "Lwt_domain.detach")

let detach pool f args =
  if (get_num_domains pool = 1) then
    Lwt.wrap1 f args
  else begin
    let result = ref init_result in
    let task () =
      result := try Ok (f args) with exn -> Error exn
    in
    let waiter, wakener = Lwt.wait () in
    let id =
      Lwt_unix.make_notification ~once:true
        (fun () -> Lwt.wakeup_result wakener !result)
    in
    let _ = T.async pool (fun _ -> task ();
    Lwt_unix.send_notification id) in
    waiter
  end

(* +-----------------------------------------------------------------+
   | Running Lwt threads in the main domain                          |
   +-----------------------------------------------------------------+ *)

(* Jobs to be run in the main domain *)
let jobs = C.make_unbounded ()
let job_done = C.make_bounded 0
let job_notification =
  Lwt_unix.make_notification
    (fun () ->
      let thunk = C.recv jobs in
      ignore (thunk ()))

let run_in_main f =
  let res = ref init_result in
  let job () =
    Lwt.try_bind f
      (fun ret -> Lwt.return (Result.Ok ret))
      (fun exn -> Lwt.return (Result.Error exn)) >>= fun result ->
    res := result;
    C.send job_done ();
    Lwt.return_unit
  in
  C.send jobs job;
  Lwt_unix.send_notification job_notification;
  (* blocks calling domain until the job is executed *)
  C.recv job_done;
  match !res with
  | Result.Ok ret -> ret
  | Result.Error exn -> raise exn
