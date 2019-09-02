(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Test
open Lwt.Infix

let test_file = "Lwt_io_test"
let file_contents = "test file content"

let open_and_read_filename () =
  Lwt_io.open_file ~mode:Lwt_io.input test_file >>= fun in_chan ->
  Lwt_io.read in_chan >>= fun s ->
  Lwt_io.close in_chan >>= fun () ->
  assert (s = file_contents);
  Lwt.return ()

let suite = suite "lwt_io non blocking io" [
  test "file does not exist"
    (fun () -> Lwt_unix.file_exists test_file >|= fun r -> not r);

  test "file does not exist (invalid path)"
    (fun () -> Lwt_unix.file_exists (test_file ^ "/foo") >|= fun r -> not r);

  test "file does not exist (LargeFile)"
    (fun () -> Lwt_unix.LargeFile.file_exists test_file >|= fun r -> not r);

  test "file does not exist (LargeFile, invalid path)"
    (fun () -> Lwt_unix.LargeFile.file_exists (test_file ^ "/foo") >|= fun r -> not r);

  test "create file"
    (fun () ->
      Lwt_io.open_file ~mode:Lwt_io.output test_file >>= fun out_chan ->
      Lwt_io.write out_chan file_contents >>= fun () ->
      Lwt_io.close out_chan >>= fun () ->
      Lwt.return_true);

  test "file exists"
    (fun () -> Lwt_unix.file_exists test_file);

  test "file exists (LargeFile)"
    (fun () -> Lwt_unix.LargeFile.file_exists test_file);


  test "read file"
    (fun () ->
      Lwt_io.open_file ~mode:Lwt_io.input test_file >>= fun in_chan ->
      Lwt_io.read in_chan >>= fun s ->
      Lwt_io.close in_chan >>= fun () ->
      Lwt.return (s = file_contents));

  test "many read file"
    (fun () ->
      let rec loop i =
        open_and_read_filename () >>= fun () ->
        if i > 10000 then Lwt.return_true
        else loop (i + 1)
      in
      loop 0);

  test "remove file"
    (fun () ->
      Unix.unlink test_file;
      Lwt.return_true);

]
