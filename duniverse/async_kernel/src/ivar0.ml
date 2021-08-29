open! Core_kernel
open! Import
module Scheduler = Scheduler1
module Cell = Types.Cell

type any =
  [ `Empty
  | `Empty_one_handler
  | `Empty_one_or_more_handlers
  | `Full
  | `Indir
  ]

type 'a t = 'a Types.Ivar.t = { mutable cell : ('a, any) cell }

(* The ['b] is used to encode the constructor.  This allows us to write functions that
   take only one of the constructors, with no runtime test.

   We maintain the invariant that the directed graph with ivars as nodes and [Indir]s as
   edges is acyclic.  The only functions that create an [Indir] are [squash] and
   [connect], and for those, the target of the [Indir] is always a non-[Indir].  Thus, the
   newly added edges are never part of a cycle. *)
and ('a, 'b) cell = ('a, 'b) Types.Cell.t =
  | Empty_one_or_more_handlers :
      { (* [run] is mutable so we can set it to [ignore] when the handler is removed.
           This is used when we install a handler on a full ivar since it is immediately
           added to the scheduler. *)
        mutable run : 'a -> unit
      ; execution_context : Execution_context.t
      ; (* [prev] and [next] circularly doubly link all handlers of the same ivar. *)
        mutable prev : ('a, [ `Empty_one_or_more_handlers ]) cell
      ; mutable next : ('a, [ `Empty_one_or_more_handlers ]) cell
      }
      -> ('a, [> `Empty_one_or_more_handlers ]) cell
  | Empty_one_handler :
      ('a -> unit) * Execution_context.t
      -> ('a, [> `Empty_one_handler ]) cell
  | Empty : ('a, [> `Empty ]) cell
  | Full : 'a -> ('a, [> `Full ]) cell
  | Indir : 'a t -> ('a, [> `Indir ]) cell

module Handler = struct
  type 'a t = ('a, [ `Empty_one_or_more_handlers ]) cell

  let run (Empty_one_or_more_handlers t : _ t) = t.run
  let execution_context (Empty_one_or_more_handlers t : _ t) = t.execution_context
  let prev (Empty_one_or_more_handlers t : _ t) = t.prev
  let next (Empty_one_or_more_handlers t : _ t) = t.next
  let set_run (Empty_one_or_more_handlers t : _ t) x = t.run <- x
  let set_prev (Empty_one_or_more_handlers t : _ t) x = t.prev <- x
  let set_next (Empty_one_or_more_handlers t : _ t) x = t.next <- x

  let create run execution_context =
    (* An optimized implementation of:

       {[
         let rec t =
           Empty_one_or_more_handlers
             { run
             ; execution_context
             ; prev              = t
             ; next              = t }
         in
         h1 ]}

       However the compilation of recursive value in OCaml is not optimal: the value is
       allocated twice and copied once (with a loop calling caml_modify).  This is not
       necessary for simple recursive definitions like this one.

       Instead we allocate the value with dummy fields and update them after. *)
    let t =
      Empty_one_or_more_handlers
        { run; execution_context; prev = Obj.magic None; next = Obj.magic None }
    in
    set_prev t t;
    set_next t t;
    t
  ;;

  let create2 run1 execution_context1 run2 execution_context2 =
    (* An optimized implementation of:

       {[
         let rec t1 =
           { run               = run1
           ; execution_context = execution_context1
           ; prev              = t2
           ; next              = t2 }
         and t2 =
           { run               = run2
           ; execution_context = execution_context2
           ; prev              = t1
           ; next              = t1 }
         in
         t1 ]} *)
    let t1 =
      Empty_one_or_more_handlers
        { run = run1
        ; execution_context = execution_context1
        ; prev = Obj.magic None
        ; next = Obj.magic None
        }
    in
    let t2 =
      Empty_one_or_more_handlers
        { run = run2; execution_context = execution_context2; prev = t1; next = t1 }
    in
    set_prev t1 t2;
    set_next t1 t2;
    t1
  ;;

  let invariant t =
    Execution_context.invariant (execution_context t);
    let r = ref (next t) in
    while not (phys_equal !r t) do
      let t1 = !r in
      assert (phys_equal (prev (next t1)) t1);
      Execution_context.invariant (execution_context t1);
      r := next !r
    done
  ;;

  let is_singleton t = phys_equal t (next t)

  let length t =
    let n = ref 1 in
    let r = ref (next t) in
    while not (phys_equal !r t) do
      incr n;
      r := next !r
    done;
    !n
  ;;

  let enqueue t scheduler v = Scheduler.enqueue scheduler (execution_context t) (run t) v

  let schedule_jobs t v =
    let scheduler = Scheduler.t () in
    enqueue t scheduler v;
    let r = ref (next t) in
    while not (phys_equal !r t) do
      enqueue !r scheduler v;
      r := next !r
    done
  ;;

  let unlink t =
    set_prev (next t) (prev t);
    set_next (prev t) (next t);
    set_prev t t;
    set_next t t
  ;;

  let add t run execution_context =
    let result =
      Empty_one_or_more_handlers { run; execution_context; prev = prev t; next = t }
    in
    set_next (prev t) result;
    set_prev t result;
    result
  ;;

  (* [splice t1 t2] creates:

     {v
       --> t1 <--> ... <--> last1 <--> t2 <--> ... <--> last2 <--
       |                                                        |
       ----------------------------------------------------------
     v} *)
  let splice t1 t2 =
    let last1 = prev t1 in
    let last2 = prev t2 in
    set_next last1 t2;
    set_next last2 t1;
    set_prev t1 last2;
    set_prev t2 last1
  ;;

  let of_list l =
    match l with
    | [] -> None
    | (run, execution_context) :: l ->
      let first = create run execution_context in
      let rec loop prev l =
        match l with
        | [] -> set_prev first prev
        | (run, execution_context) :: l ->
          let t =
            Empty_one_or_more_handlers { run; execution_context; prev; next = first }
          in
          set_next prev t;
          loop t l
      in
      loop first l;
      Some first
  ;;

  let to_list first =
    let rec loop t acc =
      let acc = (run t, execution_context t) :: acc in
      if phys_equal t first then acc else loop (prev t) acc
    in
    loop (prev first) []
  ;;

  let sexp_of_t _ (t : _ t) =
    let (Empty_one_or_more_handlers { run = _; execution_context; next = _; prev = _ }) =
      t
    in
    [%message (execution_context : Execution_context.t)]
  ;;
end

type 'a ivar = 'a t

(* Compiled as the identity. *)
let cell_of_handler : _ Handler.t -> _ = function
  | Empty_one_or_more_handlers _ as x -> (x :> (_, any) cell)
;;

let equal (t : _ t) t' = phys_equal t t'
let indir t = { cell = Indir t }

include Scheduler.Ivar

(* [squash t] returns the non-[Indir] ivar at the end of the (possibly empty) chain of
   [Indir]s starting with [t] and ensures that all [Indir]s along that chain are replaced
   with an [Indir] pointing to the end of the chain. *)
let squash =
  let rec follow indir t =
    (* [indir = Indir t] *)
    match t.cell with
    | Indir t' as indir' -> follow indir' t'
    | _ -> indir
  in
  let rec update t indir =
    match t.cell with
    | Indir t' ->
      t.cell <- indir;
      update t' indir
    | _ -> t
  in
  fun t ->
    match t.cell with
    | Indir t' ->
      (match t'.cell with
       | Indir t'' as indir -> update t (follow indir t'')
       | _ -> t' (* nothing to do, since [t] is a chain with a single [Indir] *))
    | _ -> t
;;

(* nothing to do, since [t] isn't an [Indir]. *)

let invariant a_invariant t =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full a -> a_invariant a
  | Empty -> ()
  | Empty_one_handler (_, execution_context) ->
    Execution_context.invariant execution_context
  | Empty_one_or_more_handlers _ as handler -> Handler.invariant handler
;;

let sexp_of_t sexp_of_a t : Sexp.t =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full a -> List [ Atom "Full"; sexp_of_a a ]
  | Empty | Empty_one_handler _ | Empty_one_or_more_handlers _ -> Atom "Empty"
;;

let peek t =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full a -> Some a
  | Empty | Empty_one_handler _ | Empty_one_or_more_handlers _ -> None
;;

let value t ~if_empty_then_failwith =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full a -> a
  | Empty | Empty_one_handler _ | Empty_one_or_more_handlers _ ->
    failwith if_empty_then_failwith
;;

let value_exn t = value t ~if_empty_then_failwith:"Ivar.value_exn called on empty ivar"

let is_empty t =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full _ -> false
  | Empty | Empty_one_handler _ | Empty_one_or_more_handlers _ -> true
;;

let is_full t = not (is_empty t)

let fill t v =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full _ -> raise_s [%message "Ivar.fill of full ivar" (t : _ t)]
  | Empty -> t.cell <- Full v
  | Empty_one_handler (run, execution_context) ->
    t.cell <- Full v;
    Scheduler.(enqueue (t ())) execution_context run v
  | Empty_one_or_more_handlers _ as handler ->
    t.cell <- Full v;
    Handler.schedule_jobs handler v
;;

let remove_handler t (handler : _ Handler.t) =
  Handler.set_run handler ignore;
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Empty | Empty_one_handler _ ->
    (* These are only possible if [handler] was already removed.  *)
    ()
  | Full _ ->
    (* This is possible if [t] was filled before we try to remove the handler.  E.g.
       [Deferred.choose] will do this. *)
    ()
  | Empty_one_or_more_handlers _ as cell ->
    if Handler.is_singleton handler
    then t.cell <- Empty
    else (
      if phys_equal handler cell then t.cell <- cell_of_handler (Handler.next handler);
      Handler.unlink handler)
;;

let add_handler t run execution_context =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Empty ->
    let handler = Handler.create run execution_context in
    t.cell <- cell_of_handler handler;
    handler
  | Empty_one_handler (run', execution_context') ->
    let handler = Handler.create2 run execution_context run' execution_context' in
    t.cell <- cell_of_handler handler;
    handler
  | Empty_one_or_more_handlers _ as handler -> Handler.add handler run execution_context
  | Full v ->
    let handler = Handler.create run execution_context in
    (* [run] calls [handler.run], which, if [handler] has been removed, has been changed
       to [ignore]. *)
    let run v = Handler.run handler v in
    Scheduler.(enqueue (t ())) execution_context run v;
    handler
;;

let has_handlers t =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Empty_one_handler _ | Empty_one_or_more_handlers _ -> true
  | Empty | Full _ -> false
;;

let upon' t run = add_handler t run Scheduler.(current_execution_context (t ()))

(* [upon] is conceptually the same as:

   {[
     let upon t f = ignore (upon' t run) ]}

   However, below is a more efficient implementation, which is worth doing because [upon]
   is very widely used and is so much more common than [upon'].  The below implementation
   avoids the use of the bag of handlers in the extremely common case of one handler for
   the deferred. *)
let upon t run =
  let scheduler = Scheduler.t () in
  let execution_context = Scheduler.current_execution_context scheduler in
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full v -> Scheduler.enqueue scheduler execution_context run v
  | Empty -> t.cell <- Empty_one_handler (run, execution_context)
  | Empty_one_handler (run', execution_context') ->
    t.cell
    <- cell_of_handler (Handler.create2 run execution_context run' execution_context')
  | Empty_one_or_more_handlers _ as handler ->
    ignore (Handler.add handler run execution_context : _ Handler.t)
;;

(* [connect] takes ivars [bind_result] and [bind_rhs], and makes [bind_rhs]
   be an [Indir] pointing to the non-indir cell reachable from [bind_result].  On entry
   to [connect], [bind_result] and [bind_rhs] may be chains, since [bind_rhs] is an
   arbitrary user-supplied deferred, and [bind_result] is returned to the user prior to
   being [connect]ed, and may have been converted to an indirection in the case of
   right-nested binds.

   The purpose of [connect] is to make tail-recursive bind loops use constant space.
   E.g.:

   {[
     let rec loop i =
       if i = 0
       then return ()
       else (
         let%bind () = after (sec 1.) in
         loop (i - 1)) ]}

   [connect] makes intermediate bind results all be [Indir]s pointing at the outermost
   bind, rather than being a linear-length chain, with each pointing to the previous one.
   Then, since the program is only holding on to the innermost and outermost binds all the
   intermediate ones can be garbage collected.

   [connect] works by squashing its arguments so that the [bind_rhs] always points at the
   ultimate result. *)
let connect =
  (* [repoint_indirs ~ivar ~indir ~bind_result] repoints to [indir] all the ivars in the
     chain reachable from [ivar], and returns the non-[Indir] cell at the end of the
     chain.  After repointing, we will merge the handlers in that cell with the handlers
     in [bind_result], and put the merged set of handlers in [bind_result]. *)
  let rec repoint_indirs ~ivar ~indir ~bind_result =
    let cell = ivar.cell in
    match cell with
    | Indir ivar' ->
      ivar.cell <- indir;
      repoint_indirs ~ivar:ivar' ~indir ~bind_result
    | Full _ -> cell
    | Empty | Empty_one_handler _ | Empty_one_or_more_handlers _ ->
      (* It is possible that [bind_result] and [bind_rhs] are not equal, but their chains
         of indirs lead to the same non-[Indir] cell, in which case we cannot set that
         cell to point to itself, because that would introduce a cycle. *)
      if not (phys_equal ivar bind_result) then ivar.cell <- indir;
      cell
  in
  fun ~bind_result ~bind_rhs ->
    if not (phys_equal bind_result bind_rhs)
    then (
      let bind_result = squash bind_result in
      let indir = Indir bind_result in
      let bind_rhs_contents = repoint_indirs ~ivar:bind_rhs ~indir ~bind_result in
      (* update [bind_result] with the union of handlers in [bind_result] and
         [bind_rhs] *)
      match bind_result.cell, bind_rhs_contents with
      | Indir _, _ | _, Indir _ -> assert false
      (* fulfilled by [squash] and [repoint_indirs] *)
      (* [connect] is only used in bind, whose ivar is only ever exported as a read-only
         deferred.  Thus, [bind_result] must be empty. *)
      | Full _, _ -> assert false
      | _, Empty -> ()
      | Empty, _ -> bind_result.cell <- bind_rhs_contents
      | Empty_one_handler (run, execution_context), Full v ->
        bind_result.cell <- bind_rhs_contents;
        Scheduler.(enqueue (t ())) execution_context run v
      | (Empty_one_or_more_handlers _ as handler), Full v ->
        bind_result.cell <- bind_rhs_contents;
        Handler.schedule_jobs handler v
      | ( Empty_one_handler (run1, execution_context1)
        , Empty_one_handler (run2, execution_context2) ) ->
        let handler1 = Handler.create2 run1 execution_context1 run2 execution_context2 in
        bind_result.cell <- cell_of_handler handler1
      | ( (Empty_one_or_more_handlers _ as handler1)
        , Empty_one_handler (run2, execution_context2) ) ->
        ignore (Handler.add handler1 run2 execution_context2 : _ Handler.t)
      | ( Empty_one_handler (run1, execution_context1)
        , (Empty_one_or_more_handlers _ as handler2) ) ->
        let handler1 = Handler.add handler2 run1 execution_context1 in
        bind_result.cell <- cell_of_handler handler1
      | ( (Empty_one_or_more_handlers _ as handler1)
        , (Empty_one_or_more_handlers _ as handler2) ) ->
        Handler.splice handler1 handler2)
;;
