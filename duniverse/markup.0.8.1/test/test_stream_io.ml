(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2

open Markup__Common
open Markup

let self = "./test_stream_io.ml"
let no_file = "./no_such_file"
let directory = "."

let exn = Failure ("failure")
let fails = fun () -> raise exn

let with_file_reading name f =
  let c = open_in name in
  try
    f c;
    close_in_noerr c
  with exn ->
    close_in_noerr c;
    raise exn

let with_file_writing name f =
  let c = open_out name in
  try
    f c;
    close_out_noerr c
  with exn ->
    close_out_noerr c;
    raise exn

let tests = [
  ("stream_io.sync.string" >:: fun _ ->
    let s = string "foo" in
    to_list s |> assert_equal ['f'; 'o'; 'o'];
    next s |> assert_equal None;
    next s |> assert_equal None);

  ("stream_io.sync.buffer" >:: fun _ ->
    let b = Buffer.create 4 in
    let s = buffer b in
    Buffer.add_string b "foo";
    to_list s |> assert_equal ['f'; 'o'; 'o'];
    next s |> assert_equal None;
    next s |> assert_equal None);

  ("stream_io.sync.channel" >:: fun _ ->
    with_file_reading self (fun c ->
      let s = channel c in
      next s |> assert_equal (Some '(');
      next s |> assert_equal (Some '*');
      next s |> assert_equal (Some ' ');
      next s |> assert_equal (Some 'T');
      drain s;
      next s |> assert_equal None;
      next s |> assert_equal None;
      close_in_noerr c;
      next s |> assert_equal None));

  ("stream_io.sync.channel.closed" >:: fun _ ->
    with_file_reading self (fun c ->
      let s = channel c in
      close_in_noerr c;
      assert_raises (Sys_error "Bad file descriptor")
        (fun () -> next s |> ignore)));

  ("stream_io.sync.file" >:: fun _ ->
    let s, close = file self in
    next s |> assert_equal (Some '(');
    next s |> assert_equal (Some '*');
    next s |> assert_equal (Some ' ');
    next s |> assert_equal (Some 'T');
    drain s;
    next s |> assert_equal None;
    next s |> assert_equal None;
    close ();
    next s |> assert_equal None);

  ("stream_io.sync.file.closed" >:: fun _ ->
    let s, close = file self in
    close ();
    assert_raises (Sys_error "Bad file descriptor")
      (fun () -> next s |> ignore));

  ("stream_io.sync.file.not_found" >:: fun _ ->
    assert_raises (Sys_error (no_file ^ ": No such file or directory"))
      (fun () -> file no_file |> ignore));

  ("stream_io.sync.to_buffer" >:: fun _ ->
    of_list ['f'; 'o'; 'o'] |> to_string |> assert_equal "foo");

  ("stream_io.sync.to_string.exn" >:: fun _ ->
    assert_raises exn (fun () -> fails |> stream |> to_string |> ignore));

  ("stream_io.sync.to_buffer" >:: fun _ ->
    of_list ['f'; 'o'; 'o'] |> to_buffer |> Buffer.contents
    |> assert_equal "foo");

  ("stream_io.sync.to_buffer.exn" >:: fun _ ->
    assert_raises exn (fun () -> fails |> stream |> to_buffer |> ignore));

  ("stream_io.sync.to_channel" >:: fun context ->
    let name, c = bracket_tmpfile context in
    of_list ['f'; 'o'; 'o'] |> to_channel c;
    close_out_noerr c;
    with_file_reading name (fun c ->
      input_line c |> assert_equal "foo";
      assert_raises End_of_file (fun () -> input_line c |> ignore)));

  ("stream_io.sync.to_channel.exn" >:: fun _ ->
    assert_raises exn (fun () -> fails |> stream |> to_channel stdout));

  ("stream_io.sync.to_channel.closed" >:: fun context ->
    let _, c = bracket_tmpfile context in
    close_out_noerr c;
    assert_raises (Sys_error "Bad file descriptor")
      (fun () -> of_list ['f'; 'o'; 'o'] |> to_channel c));

  ("stream_io.sync.to_file" >:: fun context ->
    let name, c = bracket_tmpfile context in
    close_out_noerr c;
    of_list ['f'; 'o'; 'o'] |> to_file name;
    with_file_reading name (fun c ->
      input_line c |> assert_equal "foo";
      assert_raises End_of_file (fun () -> input_line c |> ignore)));

  ("stream_io.sync.to_file.exn" >:: fun context ->
    let name, c = bracket_tmpfile context in
    close_out_noerr c;
    assert_raises exn (fun () -> fails |> stream |> to_file name));

  ("stream_io.sync.to_file.not_a_file" >:: fun _ ->
    assert_raises (Sys_error (directory ^ ": Is a directory"))
      (fun () -> of_list ['f'; 'o'; 'o'] |> to_file directory))
]
