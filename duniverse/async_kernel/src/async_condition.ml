open Core_kernel

type 'a t = { waits : 'a Ivar.t Queue.t } [@@deriving sexp_of]

let create () = { waits = Queue.create () }
let wait t = Deferred.create (fun ivar -> Queue.enqueue t.waits ivar)
let signal t a = Option.iter (Queue.dequeue t.waits) ~f:(fun ivar -> Ivar.fill ivar a)

let broadcast t a =
  Queue.iter t.waits ~f:(fun ivar -> Ivar.fill ivar a);
  Queue.clear t.waits
;;
