open! Import
module Int63_emul = Base.Int63.Private.Emul

let%expect_test _ =
  let s63 = Int63.(Hex.to_string min_value) in
  let s63_emul = Int63_emul.(Hex.to_string min_value) in
  print_s [%message (s63 : string) (s63_emul : string)];
  require [%here] (String.equal s63 s63_emul);
  [%expect
    {|
    ((s63      -0x4000000000000000)
     (s63_emul -0x4000000000000000)) |}]
;;
