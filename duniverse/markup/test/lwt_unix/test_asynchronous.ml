(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2

(* Lwt.Infix not available for Lwt 2.4.6 (Ocaml 4.00). *)
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

open Markup_lwt

let tests = [
  ("asynchronous.stream,next" >:: fun _ ->
    let returned = ref false in
    let s =
      (fun () ->
        if not !returned then begin
          returned := true;
          Lwt.return (Some 1337)
        end
        else Lwt.return None)
      |> stream
    in
    (next s >|= assert_equal ~msg:"first" (Some 1337) >>= fun () ->
    next s >|= assert_equal ~msg:"second" None)
    |> Lwt_main.run);

  ("asynchronous.peek,drain" >:: fun _ ->
    let s = Markup.of_list [1; 2; 3] in
    (peek s >|= assert_equal ~msg:"1" (Some 1) >>= fun () ->
    peek s >|= assert_equal ~msg:"1b" (Some 1) >>= fun () ->
    next s >|= ignore >>= fun () ->
    peek s >|= assert_equal ~msg:"2" (Some 2) >>= fun () ->
    drain s >>= fun () ->
    peek s >|= assert_equal ~msg:"empty" None)
    |> Lwt_main.run)
]
