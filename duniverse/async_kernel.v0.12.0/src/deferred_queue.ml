open Core_kernel
open Deferred_std
module List = Deferred_list

(* We implement all of the [Queue] operations by converting the queue to a list and then
   using the corresponding [List] operation.  We use lists rather than arrays because
   arrays longer than a certain length are allocated in the major heap, which can cause
   unnecessary promotion of the elements in the queue.  Also, when one is folding or
   iterating over an array, the entire array must be kept alive.  When folding or
   iterating over a list, only the remaining tail of the list is kept alive.  So, using
   arrays rather than lists would increase the live-space needed by the program. *)

let foldi t ~init ~f = List.foldi (Queue.to_list t) ~init ~f
let fold t ~init ~f = List.fold (Queue.to_list t) ~init ~f
let all t = List.all (Queue.to_list t) >>| Queue.of_list
let all_unit t = List.all_unit (Queue.to_list t)
let iter ?how t ~f = List.iter ?how (Queue.to_list t) ~f
let iteri ?how t ~f = List.iteri ?how (Queue.to_list t) ~f
let map ?how t ~f = List.map ?how (Queue.to_list t) ~f >>| Queue.of_list
let mapi ?how t ~f = List.mapi ?how (Queue.to_list t) ~f >>| Queue.of_list
let init ?how n ~f = List.init ?how n ~f >>| Queue.of_list
let filter ?how t ~f = List.filter ?how (Queue.to_list t) ~f >>| Queue.of_list
let filteri ?how t ~f = List.filteri ?how (Queue.to_list t) ~f >>| Queue.of_list
let filter_map ?how t ~f = List.filter_map ?how (Queue.to_list t) ~f >>| Queue.of_list
let filter_mapi ?how t ~f = List.filter_mapi ?how (Queue.to_list t) ~f >>| Queue.of_list

let concat_map ?how t ~f =
  List.concat_map ?how (Queue.to_list t) ~f:(fun x -> f x >>| Queue.to_list)
  >>| Queue.of_list
;;

let concat_mapi ?how t ~f =
  List.concat_mapi ?how (Queue.to_list t) ~f:(fun i x -> f i x >>| Queue.to_list)
  >>| Queue.of_list
;;

let find_map t ~f = List.find_map (Queue.to_list t) ~f
let find_mapi t ~f = List.find_mapi (Queue.to_list t) ~f
let find t ~f = List.find (Queue.to_list t) ~f
let findi t ~f = List.findi (Queue.to_list t) ~f
let for_all t ~f = List.for_all (Queue.to_list t) ~f
let for_alli t ~f = List.for_alli (Queue.to_list t) ~f
let exists t ~f = List.exists (Queue.to_list t) ~f
let existsi t ~f = List.existsi (Queue.to_list t) ~f
