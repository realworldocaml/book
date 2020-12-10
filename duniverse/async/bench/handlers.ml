(*open Core*)
open Async
open Core_bench

(* Reset the Ivar to the empty state *)
let reset_ivar ivar = Obj.set_field (Obj.repr ivar) 0 (Obj.repr 0)
let bench_create () = ignore (Ivar.create () : unit Ivar.t)

let bench_1handler =
  let ivar = Ivar.create () in
  fun () ->
    reset_ivar ivar;
    upon (Ivar.read ivar) ignore
;;

let bench_5handlers =
  let ivar = Ivar.create () in
  fun () ->
    reset_ivar ivar;
    upon (Ivar.read ivar) ignore;
    upon (Ivar.read ivar) ignore;
    upon (Ivar.read ivar) ignore;
    upon (Ivar.read ivar) ignore;
    upon (Ivar.read ivar) ignore
;;

let bench_choose_over2 =
  let ivar1 = Ivar.create () in
  let ivar2 = Ivar.create () in
  fun () ->
    reset_ivar ivar1;
    reset_ivar ivar2;
    let d =
      choose [ choice (Ivar.read ivar1) ignore; choice (Ivar.read ivar2) ignore ]
    in
    ignore (d : unit Deferred.t)
;;

let bench_install_remove_handler =
  let module Ivar = Async_kernel_private.Ivar0 in
  let ivar = Ivar.create () in
  fun () ->
    reset_ivar ivar;
    Ivar.remove_handler ivar (Ivar.upon' ivar ignore)
;;

let bench_install5_remove1_handler =
  let module Ivar = Async_kernel_private.Ivar0 in
  let ivar = Ivar.create () in
  fun () ->
    reset_ivar ivar;
    let h = Ivar.upon' ivar ignore in
    ignore (Ivar.upon' ivar ignore : unit Ivar.Handler.t);
    ignore (Ivar.upon' ivar ignore : unit Ivar.Handler.t);
    ignore (Ivar.upon' ivar ignore : unit Ivar.Handler.t);
    ignore (Ivar.upon' ivar ignore : unit Ivar.Handler.t);
    Ivar.remove_handler ivar h
;;

let bench_connect_a =
  let module Ivar = Async_kernel_private.Ivar0 in
  let ivar1 = Ivar.create () in
  let ivar2 = Ivar.create () in
  fun () ->
    reset_ivar ivar1;
    reset_ivar ivar2;
    Ivar.upon ivar1 ignore;
    Ivar.upon ivar2 ignore;
    Ivar.connect ~bind_result:ivar1 ~bind_rhs:ivar2
;;

let bench_connect_b =
  let module Ivar = Async_kernel_private.Ivar0 in
  let ivar1 = Ivar.create () in
  let ivar2 = Ivar.create () in
  fun () ->
    reset_ivar ivar1;
    reset_ivar ivar2;
    Ivar.upon ivar1 ignore;
    ignore (Ivar.upon' ivar2 ignore : _ Ivar.Handler.t);
    Ivar.connect ~bind_result:ivar1 ~bind_rhs:ivar2
;;

let bench_connect_c =
  let module Ivar = Async_kernel_private.Ivar0 in
  let ivar1 = Ivar.create () in
  let ivar2 = Ivar.create () in
  fun () ->
    reset_ivar ivar1;
    reset_ivar ivar2;
    ignore (Ivar.upon' ivar1 ignore : _ Ivar.Handler.t);
    Ivar.upon ivar2 ignore;
    Ivar.connect ~bind_result:ivar1 ~bind_rhs:ivar2
;;

let bench_connect_d =
  let module Ivar = Async_kernel_private.Ivar0 in
  let ivar1 = Ivar.create () in
  let ivar2 = Ivar.create () in
  fun () ->
    reset_ivar ivar1;
    reset_ivar ivar2;
    ignore (Ivar.upon' ivar1 ignore : _ Ivar.Handler.t);
    ignore (Ivar.upon' ivar2 ignore : _ Ivar.Handler.t);
    Ivar.connect ~bind_result:ivar1 ~bind_rhs:ivar2
;;

let () =
  Command.run
    (Bench.make_command
       [ Bench.Test.create ~name:"create" bench_create
       ; Bench.Test.create ~name:"1handler" bench_1handler
       ; Bench.Test.create ~name:"5handlers" bench_5handlers
       ; Bench.Test.create ~name:"choose_over2" bench_choose_over2
       ; Bench.Test.create ~name:"install_remove_handler" bench_install_remove_handler
       ; Bench.Test.create
           ~name:"install5_remove1_handler"
           bench_install5_remove1_handler
       ; Bench.Test.create ~name:"bench_connect_a" bench_connect_a
       ; Bench.Test.create ~name:"bench_connect_b" bench_connect_b
       ; Bench.Test.create ~name:"bench_connect_c" bench_connect_c
       ; Bench.Test.create ~name:"bench_connect_d" bench_connect_d
       ])
;;
