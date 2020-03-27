open Parser.MenhirInterpreter

(* This module offers the functionality required by the functor
   [ErrorReporting.Printers.Make]. *)

val terminal2token: _ terminal -> token

