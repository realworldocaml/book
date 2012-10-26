open Core.Std

val color_to_int
  : [ `Basic of ([ `Black   | `Blue | `Cyan  | `Green
                 | `Magenta | `Red  | `White | `Yellow ]
                 * [ `Bold | `Regular ])
    | `Gray of int
    | `RGB  of int * int * int ]
  ->  int

val extended_color_to_int
  : [ `Basic of ([ `Black   | `Blue | `Cyan  | `Green
                 | `Magenta | `Red  | `White | `Yellow ]
                 * [ `Bold | `Regular ])
    | `Gray of int
    | `RGB  of int * int * int
    | `RGBA of int * int * int * int ]
  ->  int
