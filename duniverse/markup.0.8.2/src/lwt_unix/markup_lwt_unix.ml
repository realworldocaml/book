(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

module Kstream = Markup__Kstream

(* Lwt.Infix not available for Lwt 2.4.6 (Ocaml 4.00). *)
let (>>=) = Lwt.(>>=)

let channel c =
  let ended = ref false in

  (fun () ->
    if !ended then Lwt.return_none
    else
      Lwt_io.read_char_opt c >>= function
      | Some _ as v -> Lwt.return v
      | None -> ended := true; Lwt.return_none)
  |> Markup_lwt.stream

let file =
  let open_file name =
    (fun () -> Lwt_io.open_file ~mode:Lwt_io.input name) |> Markup_lwt.to_cps in

  let close c k =
    ((fun () -> Lwt_io.close c) |> Markup_lwt.to_cps)
      (fun _ -> k ()) (fun _ -> k ())
  in

  fun name ->
    let closed = ref false in
    let close_fn = ref (fun () -> closed := true; Lwt.return_unit) in

    let constructor throw k =
      open_file name throw (fun c ->
        if !closed then throw (Lwt_io.Channel_closed "input")
        else begin
          close_fn := (fun () -> Lwt_io.close c);
          let s = channel c |> Markup.kstream in
          (fun throw e k ->
            Kstream.next s
              (fun exn -> close c (fun () -> throw exn))
              (fun () -> close c e)
              k)
          |> Kstream.make
          |> k
        end)
    in

    let s = Kstream.construct constructor |> Markup.of_kstream in

    let close () = !close_fn () in

    s, close

let to_channel c s = s |> Markup_lwt.iter (Lwt_io.write_char c)

let to_file name s =
  Lwt_io.with_file ~mode:Lwt_io.output name (fun c -> to_channel c s)
