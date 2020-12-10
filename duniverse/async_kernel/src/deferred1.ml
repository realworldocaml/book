open Core_kernel
module Scheduler = Scheduler1
include Deferred0

(* To avoid a space leak, it is necessary that [never] allocates a new ivar whenever it is
   called.  Code can bind on [never ()], so if we re-used the ivar, we could endlessly
   accumulate handlers. *)
let never () = Ivar.read (Ivar.create ())

module M = Monad.Make (struct
    include Deferred0

    let map t ~f =
      (* We manually inline [Deferred.create] here, because the non-flambda compiler isn't
         able to optimize away the closure that would be be created. *)
      let result = Ivar.create () in
      upon t (fun a -> Ivar.fill result (f a));
      of_ivar result
    ;;

    let map = `Custom map
  end)

include M

(* We rebind all the various [return]s because the use of the [Monad.Make] functor
   causes the compiler to not inline [return], and hence makes it impossible to
   statically allocate constants like [return ()].  By rebinding [return] as
   [Deferred0.return], the compiler can see that:

   {[
     return a = { Ivar.Immutable. cell = Full a } ]}

   And hence, if [a] is constant, then the return is constant and can be statically
   allocated.  When compiling with flambda, the compiler inlines [return] and this manual
   rebinding would not help; we've decided to do it anyway so that non-flambda builds
   get the optimization. *)
let return = Deferred0.return

module Let_syntax = struct
  include M.Let_syntax

  let return = Deferred0.return

  module Let_syntax = struct
    include M.Let_syntax.Let_syntax

    let return = Deferred0.return
  end
end

open Let_syntax

(* We shadow [all] on-purpose here, since the default definition introduces a chain of
   binds as long as the list. *)
let all = `Make_sure_to_define_all_elsewhere
let _ = all
let unit = return ()
let ignore = ignore_m

let both t1 t2 =
  create (fun result ->
    upon t1 (fun a1 -> upon t2 (fun a2 -> Ivar.fill result (a1, a2))))
;;

module Infix = struct
  include Monad_infix

  let ( >>> ) = upon
  let ppx_both = both
end

open Infix

let don't_wait_for (_ : unit t) = ()

module Choice = struct
  type +'a t = T : 'b Deferred0.t * ('b -> 'a) -> 'a t

  let map (T (t, f1)) ~f:f2 = T (t, fun x -> f2 (f1 x))
end

type 'a choice = 'a Choice.t

module Unregister = struct
  (* This representation saves 2n words for a list of n choices. *)
  type t =
    | Nil : t
    | Cons : 'a Deferred0.t * 'a Deferred0.Handler.t * t -> t

  let rec process = function
    | Nil -> ()
    | Cons (t, handler, rest) ->
      remove_handler t handler;
      process rest
  ;;
end

let choice t f = Choice.T (t, f)

let enabled choices =
  let result = Ivar.create () in
  let unregisters = ref Unregister.Nil in
  let ready _ =
    if Ivar.is_empty result
    then (
      Unregister.process !unregisters;
      Ivar.fill result (fun () ->
        List.rev
          (List.fold choices ~init:[] ~f:(fun ac (Choice.T (t, f)) ->
             match peek t with
             | None -> ac
             | Some v -> f v :: ac))))
  in
  let execution_context = Scheduler.(current_execution_context (t ())) in
  unregisters
  := List.fold choices ~init:Unregister.Nil ~f:(fun acc (Choice.T (t, _)) ->
    Cons (t, Deferred0.add_handler t ready execution_context, acc));
  Ivar.read result
;;

let rec choose_result choices =
  match choices with
  | [] -> assert false
  | Choice.T (t, f) :: choices ->
    (match peek t with
     | None -> choose_result choices
     | Some v -> f v)
;;

let choose choices =
  let result = Ivar.create () in
  let unregisters = ref Unregister.Nil in
  let ready _ =
    if Ivar.is_empty result
    then (
      Unregister.process !unregisters;
      Ivar.fill result (choose_result choices))
  in
  let execution_context = Scheduler.(current_execution_context (t ())) in
  unregisters
  := List.fold choices ~init:Unregister.Nil ~f:(fun acc (Choice.T (t, _)) ->
    Cons (t, Deferred0.add_handler t ready execution_context, acc));
  Ivar.read result
;;

let any_f ts f = choose (List.map ts ~f:(fun t -> choice t f))
let any ts = any_f ts Fn.id
let any_unit ts = any_f ts (Fn.ignore : unit -> unit)

let for_ start ~to_ ~do_ =
  let rec loop i =
    if i > to_
    then return ()
    else (
      let%bind () = do_ i in
      loop (i + 1))
  in
  loop start
;;

let repeat_until_finished state f =
  create (fun finished ->
    let rec loop state =
      f state
      >>> function
      | `Repeat state -> loop state
      | `Finished result -> Ivar.fill finished result
    in
    loop state)
;;

let forever state f =
  repeat_until_finished state (fun state ->
    let%map state = f state in
    `Repeat state)
  >>> never_returns
;;

type how = Monad_sequence.how [@@deriving sexp_of]

module type Monad_sequence = Monad_sequence.S with type 'a monad := 'a t


let fold t ~init ~f =
  create (fun result ->
    let rec loop t b =
      match t with
      | [] -> Ivar.fill result b
      | x :: xs -> f b x >>> fun b -> loop xs b
    in
    loop t init)
;;

let seqmap t ~f = fold t ~init:[] ~f:(fun bs a -> f a >>| fun b -> b :: bs) >>| List.rev
let all ds = seqmap ds ~f:Fn.id
let all_unit ds = fold ds ~init:() ~f:(fun () d -> d)
let ok x = x >>| fun x -> Ok x
