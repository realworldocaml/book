open Core
open Async
open Print

(* ToDo: Come up with some instrumentation for heap size
 * as the program evolves.  Less important here since IVar
 * properties are well understood and not pathological. *)
(* Uses deferred to structure backtracking search.  The
 * runtime queue represents the nodes that are waiting
 * to be visited.  Changing this behavior doesn't particularly
 * change behavior though because we demand all results. *)
(* Should generate IVars equal to the branching factor plus
 * the depth of the search space [nq], and there should
 * only ever be one callback waiting on the result of a leaf
 * (since this is a search tree.) *)
(* Hack: force IVar to be empty for some period of time. Can't
 * use higher level clock because if the time to wait is zero
 * it will immediately run without giving it to the scheduler. *)
let ticky_return x = Scheduler.schedule' (fun () -> return x)

let nqueens nq =
  let rec safe x d = function
    | [] -> true
    | q :: l -> x <> q && x <> q + d && x <> q - d && safe x (d + 1) l
  in
  let nql =
    (* not tail recursive *)
    let rec f = function
      | n when n = nq -> [ n ]
      | n -> n :: f (n + 1)
    in
    f 1
  in
  let gen bs =
    List.concat
      (List.map bs ~f:(fun b ->
         List.concat (List.map nql ~f:(fun q -> if safe q 1 b then [ q :: b ] else []))))
      (* use option instead *)
  in
  let rec step n b =
    if n < nq
    then
      Deferred.all (List.map ~f:(step (n + 1)) (gen [ b ]))
      >>= fun rs ->
      (* Should be Empty
         let r = ticky_return (List.concat rs)
         in print_endline (Sexp.to_string (Deferred.sexp_of_t (fun _ -> Sexp.Atom "...") r)); r *)
      ticky_return (List.concat rs)
    else ticky_return [ b ]
  in
  step 0 []
;;

let nq = 8

let () =
  nqueens nq
  >>> fun solns ->
  print_int (List.length solns);
  print_newline ();
  Shutdown.shutdown 0
;;

let () = never_returns (Scheduler.go ())
