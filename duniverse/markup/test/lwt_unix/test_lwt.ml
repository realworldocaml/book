(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Test_support

(* Lwt.Infix not available for Lwt 2.4.6 (Ocaml 4.0.0). *)
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

open Markup_lwt
open Markup_lwt_unix

let self = "./test_lwt.ml"

let suite =
  "markup_lwt" >::: Test_asynchronous.tests @ [
    ("lwt.stream" >:: fun _ ->
      let s =
        (fun () -> Lwt_unix.sleep 0.1 >|= fun () -> Some 1337)
        |> stream
      in
      next s >|= assert_equal (Some 1337)
      |> Lwt_main.run);

    ("lwt.stream.tail_call.to_cps" >:: fun _ ->
      let s = (fun () -> Lwt.return (Some 1337)) |> stream in
      let limit = 10000 in
      Lwt.catch
        (fun () ->
          fold (fun count _ ->
            if count >= limit then Lwt.fail Exit
            else Lwt.return (count + 1))
            0 s
          >|= ignore)
        (function
          | Exit -> Lwt.return_unit
          | exn -> Lwt.fail exn)
      |> Lwt_main.run);

    ("lwt.stream.tail_call.of_cps" >:: fun _ ->
      let t = ref (Lwt.wait ()) in
      let s = (fun () -> fst !t) |> stream in
      let rec repeat n =
        if n = 0 then
          Lwt.return_unit
        else begin
          let proceed =
            next s >>= (function
            | Some () -> repeat (n - 1)
            | None -> Lwt.fail_with "unexpected result")
          in
          let push = snd !t in
          t := Lwt.wait ();
          Lwt.wakeup push (Some ());
          proceed
        end
      in
      Lwt_main.run (repeat 10000));

    ("lwt.lwt_stream" >:: fun _ ->
      [1; 2; 3]
      |> Lwt_stream.of_list
      |> lwt_stream
      |> to_list
      >|= assert_equal [1; 2; 3]
      |> Lwt_main.run);

    ("lwt.to_lwt_stream" >:: fun _ ->
      [1; 2; 3]
      |> Markup.of_list
      |> to_lwt_stream
      |> Lwt_stream.to_list
      >|= assert_equal [1; 2; 3]
      |> Lwt_main.run);

    ("lwt.channel" >:: fun _ ->
      Lwt_io.with_file ~mode:Lwt_io.input self (fun c ->
        let s = channel c in
        next s >|= assert_equal (Some '(') >>= fun () ->
        next s >|= assert_equal (Some '*') >>= fun () ->
        next s >|= assert_equal (Some ' ') >>= fun () ->
        next s >|= assert_equal (Some 'T') >>= fun () ->
        drain s >>= fun () ->
        next s >|= assert_equal None >>= fun () ->
        next s >|= assert_equal None >>= fun () ->
        Lwt_io.close c >>= fun () ->
        next s >|= assert_equal None)
      |> Lwt_main.run);

    ("lwt.channel.closed" >:: fun _ ->
      Lwt_io.with_file ~mode:Lwt_io.input self (fun c ->
        let s = channel c in
        Lwt_io.close c >>= fun () ->
        Lwt.catch
          (fun () -> next s >|= wrong_k "did not fail")
          (function
            | Lwt_io.Channel_closed "input" -> Lwt.return_unit
            | _ -> wrong_k "wrong exception" () |> Lwt.return))
      |> Lwt_main.run);

    ("lwt.to_channel" >:: fun context ->
      let name, c = bracket_tmpfile context in
      close_out_noerr c;
      (Lwt_io.with_file ~mode:Lwt_io.output name (fun c ->
        Markup.of_list ['f'; 'o'; 'o'] |> to_channel c) >>= fun () ->
      Markup.file name |> fst |> to_list >|= assert_equal ['f'; 'o'; 'o'])
      |> Lwt_main.run);

    ("lwt.file" >:: fun _ ->
      let s, close = file self in
      (next s >|= assert_equal (Some '(') >>= fun () ->
      next s >|= assert_equal (Some '*') >>= fun () ->
      next s >|= assert_equal (Some ' ') >>= fun () ->
      next s >|= assert_equal (Some 'T') >>= fun () ->
      drain s >>= fun () ->
      next s >|= assert_equal None >>= fun () ->
      next s >|= assert_equal None >>= fun () ->
      close () >>= fun () ->
      next s >|= assert_equal None)
      |> Lwt_main.run);

    ("lwt.file.closed" >:: fun _ ->
      let s, close = file self in
      (next s >|= assert_equal (Some '(') >>= fun () ->
      close () >>= fun () ->
      Lwt.catch
        (fun () -> next s >|= wrong_k "did not fail")
        (function
          | Lwt_io.Channel_closed "input" -> Lwt.return_unit
          | _ -> wrong_k "wrong exception" () |> Lwt.return))
      |> Lwt_main.run);

    ("lwt.file.closed_early" >:: fun _ ->
      let s, close = file self in
      (close () >>= fun () ->
      Lwt.catch
        (fun () -> next s >|= wrong_k "did not fail")
        (function
          | Lwt_io.Channel_closed "input" -> Lwt.return_unit
          | _ -> wrong_k "wrong exception" () |> Lwt.return))
      |> Lwt_main.run);

    ("lwt.to_file" >:: fun context ->
      let name, c = bracket_tmpfile context in
      close_out_noerr c;
      (Markup.of_list ['f'; 'o'; 'o'] |> to_file name >>= fun () ->
      Markup.file name |> fst |> to_list >|= assert_equal ['f'; 'o'; 'o'])
      |> Lwt_main.run);

    ("lwt.load" >:: fun _ ->
      (Markup.of_list ['f'; 'o'; 'o'] |> Markup_lwt.load
      >|= Markup.to_list
      >|= assert_equal ['f'; 'o'; 'o'])
      |> Lwt_main.run);
  ]

let () =
  Printf.printf "\nRunning tests in %s\n" (Filename.basename Sys.argv.(0));
  run_test_tt_main suite
