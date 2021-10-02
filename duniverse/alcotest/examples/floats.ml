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

let e = epsilon_float

let nan () =
  Alcotest.(check @@ float e) "NaN is NaN" nan nan;
  Alcotest.(check @@ neg @@ float e) "NaN is not number" nan 7.;
  Alcotest.(check @@ neg @@ float e) "number is not NaN" 8. nan

let infinity () =
  Alcotest.(check @@ float e) "+∞ is +∞" infinity infinity;
  Alcotest.(check @@ float e) "-∞ is -∞" neg_infinity neg_infinity;
  Alcotest.(check @@ neg @@ float e) "+∞ is not -∞" infinity neg_infinity;
  Alcotest.(check @@ neg @@ float e) "-∞ is not +∞" neg_infinity infinity;
  Alcotest.(check @@ neg @@ float e) "+∞ is not 3" infinity 3.

let others () =
  Alcotest.(check @@ float e) "0 is 0" 0. 0.;
  Alcotest.(check @@ float e) "0 is epsilon" 0. e;
  Alcotest.(check @@ neg @@ float e) "0 is not 1" 0. 1.;
  Alcotest.(check @@ neg @@ float e) "1 is not 0" 1. 0.;
  Alcotest.(check @@ float e) ".3 is .3" (0.1 +. 0.2) 0.3

let edge_set = [ ("NaN", `Quick, nan); ("∞", `Quick, infinity) ]
let others_set = [ ("others", `Quick, others) ]

let () =
  Alcotest.run "Float tests"
    [ ("Edge cases", edge_set); ("Other floats", others_set) ]
