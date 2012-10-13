open Core.Std

type basic_color =
    Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

let basic_color_to_int = function
  | Black -> 0 | Red     -> 1 | Green -> 2 | Yellow -> 3
  | Blue  -> 4 | Magenta -> 5 | Cyan  -> 6 | White  -> 7

type weight = Regular | Bold
type color =
| Basic of basic_color * weight (* basic colors, regular and bold *)
| RGB   of int * int * int       (* 6x6x6 color cube *)
| Gray  of int                   (* 24 grayscale levels *)

let color_to_int = function
  | Basic (basic_color,weight) ->
    let base = match weight with Bold -> 8 | Regular -> 0 in
    base + basic_color_to_int basic_color
  | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i

type extended_color =
| Basic of basic_color * weight (* basic colors, regular and bold *)
| RGB   of int * int * int       (* 6x6x6 color space *)
| RGBA  of int * int * int * int (* 6x6x6x6 color space *)
| Gray  of int                   (* 24 grayscale levels *)


let extended_color_to_int = function
  | (Basic _ | RGB _ | Gray _) as color -> color_to_int color
  | RGBA (r,g,b,a) -> 256 + a + b * 6 + g * 36 + r * 216
