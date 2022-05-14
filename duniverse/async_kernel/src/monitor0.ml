open Core
open Import

let debug = Debug.monitor

module Forwarding = Types.Forwarding

type t = Types.Monitor.t =
  { name : Info.t
  ; here : Source_code_position.t option
  ; id : int
  ; mutable next_error : exn Types.Ivar.t
  ; (* [Monitor.send_exn] schedules a job for each element of [handlers_for_all_errors]. *)
    mutable handlers_for_all_errors : (Types.Execution_context.t * (exn -> unit)) Bag.t
  ; (* [Monitor.send_exn] extends each tail in [tails_for_all_errors]. *)
    mutable tails_for_all_errors : exn Types.Tail.t list
  ; mutable has_seen_error : bool
  ; mutable forwarding : Forwarding.t
  }
[@@deriving fields]

let description t =
  match t.here with
  | None -> [%sexp (t.name : Info.t)]
  | Some here -> [%sexp (t.name : Info.t), (here : Source_code_position.t)]
;;

let descriptions =
  let rec loop t ac =
    let ac = description t :: ac in
    match t.forwarding with
    | Detached | Report_uncaught_exn -> List.rev ac
    | Parent t -> loop t ac
  in
  fun t -> loop t []
;;

let sexp_of_t t = [%sexp (descriptions t : Sexp.t list)]

let next_id =
  let r = ref 0 in
  fun () ->
    incr r;
    !r
;;

let create_with_parent ?here ?info ?name parent =
  let id = next_id () in
  let name =
    match info, name with
    | Some i, None -> i
    | Some i, Some s -> Info.tag i ~tag:s
    | None, Some s -> Info.of_string s
    | None, None -> Info.create "id" id [%sexp_of: int Sexp_hidden_in_test.t]
  in
  let t =
    { name
    ; here
    ; forwarding =
        (match parent with
         | None -> Report_uncaught_exn
         | Some parent -> Parent parent)
    ; id
    ; next_error = { cell = Empty }
    ; handlers_for_all_errors = Bag.create ()
    ; tails_for_all_errors = []
    ; has_seen_error = false
    }
  in
  if debug then Debug.log "created monitor" t [%sexp_of: t];
  t
;;

let main = create_with_parent ~name:"main" None
