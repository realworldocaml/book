open! Core
open! Async
open! Import

let prev = {|
min=0|max=10
|}

let next = {|
min=5|max=10
|}

let%expect_test "pipe" =
  let%bind () = patdiff ~prev ~next ~extra_flags:[] in
  [%expect
    {|
    (fg:red)------ (+bold)prev/file
    (fg:green)++++++ (+bold)next/file
    (fg:black)@|(+bold)-1,2 +1,2(off) ============================================================
    (fg:black) |
    (fg:black bg:red)-|(off)min=(fg:red)0(off)|max=10
    (fg:black bg:green)+|(off)min=(fg:green)5(off)|max=10
    ("Unclean exit" (Exit_non_zero 1)) |}];
  return ()
;;
