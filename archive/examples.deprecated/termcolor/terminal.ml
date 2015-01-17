open Core.Std

type std_color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
with sexp
type weight = Regular | Bold
with sexp

type color =
| Standard of std_color * weight
| RGB of int * int * int  (* base-6 *)
| Gray of int (* 24 grayscale levels *)
with sexp

let std_color_to_int = function
  | Black   -> 0 | Red     -> 1 | Green   -> 2 | Yellow  -> 3
  | Blue    -> 4 | Magenta -> 5 | Cyan    -> 6 | White   -> 7

let to_int = function
  | Standard (std_color,weight) ->
    let base = match weight with Bold -> 8 | Regular -> 0 in
    base + std_color_to_int std_color
  | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i

let color_by_number number text =
  sprintf "\027[38;5;%dm%s\027[0m" number text;;

let fg_escape_code x =
  sprintf "\027[38;5;%dm" x

let end_code = "\027[0m"

let cprint color =
  printf "%s%s%s\n"
    (fg_escape_code (to_int color))
    (sexp_of_color color |! Sexp.to_string)
    end_code

let () = Random.self_init ()
let rand_rgb () =
  RGB (Random.int 6,Random.int 6,Random.int 6)


let () =
  let std_colors = [ Black; Red; Green; Yellow; Blue; Magenta; Cyan; White] in
  List.iter std_colors ~f:(fun c ->
    cprint (Standard (c,Regular));
    cprint (Standard (c,Bold)));
  cprint (RGB (0,0,0));
  cprint (RGB (1,1,1));
  cprint (RGB (2,2,2));
  cprint (RGB (3,3,3));
  cprint (RGB (4,4,4));
  cprint (RGB (5,5,5));
  cprint (rand_rgb ());
  cprint (rand_rgb ());
  cprint (rand_rgb ());
  cprint (rand_rgb ());
  cprint (rand_rgb ());
  cprint (rand_rgb ());
  cprint (rand_rgb ());
  cprint (rand_rgb ());
  cprint (rand_rgb ());
  cprint (rand_rgb ());
  cprint (rand_rgb ());
  cprint (rand_rgb ());
  for i = 0 to 23 do
    cprint (Gray i)
  done
;;
