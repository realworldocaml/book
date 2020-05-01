open OUnit2

open Mirage_crypto

open Test_common

(* Xor *)

let xor_cases =
  cases_of (f2_eq ~msg:"xor" Uncommon.Cs.xor) [
    "00 01 02 03 04 05 06 07 08 09 0a 0b 0c" ,
    "0c 0b 0a 09 08 07 06 05 04 03 02 01 00" ,
    "0c 0a 08 0a 0c 02 00 02 0c 0a 08 0a 0c" ;

    "00 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f" ,
    "0f 0e 0d 0c 0b 0a 09 08 07 06 05 04 03 02 01 00" ,
    "0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f 0f" ;

    "00 01 02", "00", "00" ;

    "00", "00 01 02", "00" ;
  ]

let suite = [
  "XOR" >::: [ "example" >::: xor_cases ];
]
