%token A
%start<unit> fib

%%

(* An intensive inlining test.
   The size of the grammar after inlining is exponential.
   For efficiency, inlining must be performed bottom-up,
   beginning by inlining [fib0] and [fib1] into [fib2],
   then inlining [fib2] at its use site, and so on.
   A top-down strategy, without memoization, would cause
   repeated work. *)

let fib0 == A
let fib1 == A
let fib2 == fib0; fib1
let fib3 == fib1; fib2
let fib4 == fib2; fib3
let fib5 == fib3; fib4
let fib6 == fib4; fib5
let fib7 == fib5; fib6
let fib8 == fib6; fib7
let fib9 == fib7; fib8
let fib10 == fib8; fib9
let fib11 == fib9; fib10
let fib12 == fib10; fib11
let fib13 == fib11; fib12
let fib14 == fib12; fib13
let fib15 == fib13; fib14
let fib16 == fib14; fib15
let fib17 == fib15; fib16
let fib18 == fib16; fib17 (*  4 seconds *)
let fib19 == fib17; fib18 (* 11 seconds *)
let fib20 == fib18; fib19 (* 31 seconds *)

let fib  := fib9
