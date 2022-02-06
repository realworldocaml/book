(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

let () =
  Test.run "domain" [
    Test_lwt_domain.suite;
  ]
