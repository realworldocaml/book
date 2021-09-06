(*
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org/>
*)

open Lwt.Infix

exception Library_exception

module To_test = struct
  let lowercase = String.lowercase_ascii
  let lowercase_lwt s = Lwt.return (lowercase s)
  let exn () = raise Library_exception
  let exn_lwt_toplevel () : unit Lwt.t = raise Library_exception
  let exn_lwt_internal () : unit Lwt.t = Lwt.return (raise Library_exception)
end

(* The tests *)
let test_lowercase () =
  Alcotest.(check string) "same string" "hello!" (To_test.lowercase "hELLO!")

let test_lowercase_lwt _ () =
  To_test.lowercase_lwt "hELLO!"
  >|= Alcotest.(check string) "same string" "hello!"

let test_exn () =
  Alcotest.check_raises "custom exception" Library_exception To_test.exn

let lwt_check_raises f =
  Lwt.catch
    (fun () -> f () >|= fun () -> `Ok)
    (function e -> Lwt.return @@ `Error e)
  >|= function
  | `Ok -> Alcotest.fail "No exception was thrown"
  | `Error Library_exception -> Alcotest.(check pass) "Correct exception" () ()
  | `Error _ -> Alcotest.fail "Incorrect exception was thrown"

let test_exn_lwt_toplevel _ () = lwt_check_raises To_test.exn_lwt_toplevel
let test_exn_lwt_internal _ () = lwt_check_raises To_test.exn_lwt_internal
let switch = ref None

let test_switch_alloc s () =
  Lwt.return_unit >|= fun () ->
  switch := Some s;
  Alcotest.(check bool)
    "Passed switch is initially on" (Lwt_switch.is_on s) true

let test_switch_dealloc _ () =
  Lwt.return_unit >|= fun () ->
  match !switch with
  | None -> Alcotest.fail "No switch left by `test_switch_alloc` test"
  | Some s ->
      Alcotest.(check bool)
        "Switch is disabled after test" (Lwt_switch.is_on s) false

(* Run it *)
let () =
  let open Alcotest_lwt in
  Lwt_main.run
  @@ run "LwtUtils"
       [
         ( "basic",
           [
             test_case_sync "Plain" `Quick test_lowercase;
             test_case "Lwt" `Quick test_lowercase_lwt;
           ] );
         ( "exceptions",
           [
             test_case_sync "Plain" `Quick test_exn;
             test_case "Lwt toplevel" `Quick test_exn_lwt_toplevel;
             test_case "Lwt internal" `Quick test_exn_lwt_internal;
           ] );
         ( "switches",
           [
             test_case "Allocate resource" `Quick test_switch_alloc;
             test_case "Check resource deallocated" `Quick test_switch_dealloc;
           ] );
       ]
