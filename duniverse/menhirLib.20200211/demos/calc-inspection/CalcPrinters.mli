open Parser.MenhirInterpreter

(* This module offers the functionality required by the functor
   [MenhirLib.Printers.Make]. *)

val print: string -> unit
val print_symbol: xsymbol -> unit
val print_element: (element -> unit) option

