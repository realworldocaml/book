(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Test
open Lwt

let suite = suite "lwt_signal" [
  test "limit"
    (fun () ->
       let s, push = React.S.create 0 in
       let cond    = Lwt_condition.create () in
       let s'      = Lwt_react.S.limit (fun () -> Lwt_condition.wait cond) s in
       let l       = ref [] in
       let e       = React.E.map (fun x -> l := x :: !l) (React.S.changes s') in
         ignore e;
         Lwt_condition.signal cond ();
         push 1;
         push 0;
         push 2; (* overwrites previous 0 *)
         Lwt_condition.signal cond ();
         push 3;
         Lwt_condition.signal cond ();
         push 4;
         Lwt_condition.signal cond ();
         return (!l = [4; 3; 2; 1]));

  test "with_finaliser lifetime" begin fun () ->
    let s, set = React.S.create 0 in
    let finalizer_ran = ref false in
    let s' = Lwt_react.S.with_finaliser (fun () -> finalizer_ran := true) s in

    Gc.full_major ();
    let check1 = !finalizer_ran = false in

    let p = Lwt_react.E.next (React.S.changes s') in
    set 1;
    p >>= fun _ ->

    Gc.full_major ();
    let check2 = !finalizer_ran = true in

    Lwt.return (check1 && check2)
  end;
]
