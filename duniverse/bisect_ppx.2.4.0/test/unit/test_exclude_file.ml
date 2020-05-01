(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



open Test_helpers

let tests =
  compile_compare
    (fun () ->
      with_bisect_args "--exclusions ../fixtures/exclude-file/exclusions")
    "exclude-file"
