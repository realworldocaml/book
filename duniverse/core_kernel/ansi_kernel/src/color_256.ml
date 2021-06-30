open Core_kernel

type t = int [@@deriving sexp_of, compare, hash, equal]

(* Internal type for turning palette values into RGB levels -- typically
   we want to convert these into 24-bit values (8-bits per channel) or into
   0-1000 for internal [Jane_curses] use. *)
type level_map_t =
  { zero_level : int
  ; half_level : int
  ; full_level : int
  ; normal_white_level : int
  ; bright_black_level : int
  ; gray_base : int
  ; gray_stride : int
  ; color_cube_map : int list
  ; interpolated_map : float list
  }

(* NOTE: these constant structures mirror the xterm palette colour scheme, as
   per expected terminal interpretation.  The Jane_curses mapping of the
   1000-per-channel palette into RGB levels is linear, with black and
   bright-black both mapping to outright black (see
   lib/jane_curses/backends/lambda-term/lib/curses_screen.ml). *)
let level_map_8bit_per_channel =
  { zero_level = 0
  ; half_level = 128
  ; full_level = 255
  ; normal_white_level = 192
  ; bright_black_level = 128
  ; gray_base = 8
  ; gray_stride = 10
  ; color_cube_map = [ 0; 95; 135; 175; 215; 255 ]
  ; interpolated_map = [ 47.5; 115.; 155.; 195.; 235. ]
  }
;;

let level_map_1000_per_channel =
  { zero_level = 0
  ; half_level = 500
  ; full_level = 1000
  ; normal_white_level = 750
  ; bright_black_level = 500
  ; gray_base = 20
  ; gray_stride = 40
  ; color_cube_map = [ 0; 372; 529; 686; 843; 1000 ]
  ; interpolated_map = [ 186.; 450.5; 607.5; 764.5; 921.5 ]
  }
;;

(* Given a value in some integer range (here, [0,255] or [0,1000]), find the
   closest matching value in the interpolated color-cube map and return its
   index, in [0,5]. *)
let closest_cube_index v ~iterp_map =
  match
    List.findi iterp_map ~f:(fun _idx iterp_val ->
      Float.( < ) (Float.of_int v) iterp_val)
  with
  | Some (level, _) -> level
  | None -> 5
;;

let closest_8bit_cube_index =
  closest_cube_index ~iterp_map:level_map_8bit_per_channel.interpolated_map
;;

let closest_int1k_cube_index =
  closest_cube_index ~iterp_map:level_map_1000_per_channel.interpolated_map
;;

let to_int c = c

let of_int_exn i =
  if i < 0 || i > 255
  then
    failwithf
      "Attr.Color_256.of_int_exn: value %d is outside of the closed range [0-255]"
      i
      ()
  else i
;;

let of_rgb6_exn (r, g, b) =
  let in_vals = [ r; g; b ] in
  let scalers = [ 36; 6; 1 ] in
  List.fold2_exn in_vals scalers ~init:16 ~f:(fun acc v s ->
    if v >= 0 && v <= 5
    then acc + (v * s)
    else
      failwithf
        "RGB value %d for 256-color palette is outside of the closed range [0-5]"
        v
        ())
  |> of_int_exn
;;

let of_rgb rgb =
  (* Map float values from [0. -> 1.] to int [0 -> 5], reducing non-finites to 0.

     NOTE: Float.clamp_exn only throws an exception if something is wrong with
     respect to the ~min and ~max values; however, we only pass finite values
     regardless.

     NOTE: The mapping here is linear in RGB terms (for the standard 256-color
     palette used), mapped to the closest [rgb6] value.  I.e. a value of 0.1 will
     result in an output of 0/5 (black level); a value of 0.9 will result in an
     output of 4/5; and a value of 0.95 will result in an output of 5/5 (white
     level).
  *)
  Tuple3.map rgb ~f:(fun f ->
    (if Float.is_finite f then Float.clamp_exn ~min:0. ~max:1. f else 0.) *. 255.
    |> Float.round_nearest
    |> Float.to_int
    |> closest_8bit_cube_index)
  (* Assert: this cannot throw an exception, as the values are always in-range *)
  |> of_rgb6_exn
;;

let of_rgb_8bit rgb =
  (* Map integer values from [0 -> 255] to [0 -> 5]. *)
  Tuple3.map rgb ~f:closest_8bit_cube_index
  (* Assert: this cannot throw an exception, as the values are always in-range. *)
  |> of_rgb6_exn
;;

let of_rgb_int1k rgb =
  (* Map integer values from [0 -> 1000] to [0 -> 5]. *)
  Tuple3.map rgb ~f:closest_int1k_cube_index
  (* Assert: this cannot throw an exception, as the values are always in-range. *)
  |> of_rgb6_exn
;;

let of_gray24_exn g =
  (if g >= 0 && g <= 23
   then g + 232
   else failwithf "Grayscale value %d for 256-color palette out of range [0-23]" g ())
  |> of_int_exn
;;

let to_rgb_ints (c : t) ~(level_map : level_map_t) : int * int * int =
  let bit_select v b v_set =
    if v land (1 lsl b) <> 0 then v_set else level_map.zero_level
  in
  let bit3_result v v_set = Tuple3.map (0, 1, 2) ~f:(fun b -> bit_select v b v_set) in
  let ival = to_int c in
  if ival < 7
  then bit3_result ival level_map.half_level
  else if ival = 7
  then
    ( level_map.normal_white_level
    , level_map.normal_white_level
    , level_map.normal_white_level )
  else if ival = 8
  then
    ( level_map.bright_black_level
    , level_map.bright_black_level
    , level_map.bright_black_level )
  else if ival < 16
  then bit3_result (ival - 8) level_map.full_level
  else if ival > 255
  then
    (* This should not be possible, but we'll guard against it regardless *)
    level_map.full_level, level_map.full_level, level_map.full_level
  else if ival >= 232
  then (
    let gr_val = ival - 232 in
    (* [232,255] -> [0,23] ;  generate RGB levels starting at 8 in increments of 10. *)
    let gr_part = level_map.gray_base + (gr_val * level_map.gray_stride) in
    gr_part, gr_part, gr_part)
  else (
    let rgb_val = ival - 16 in
    (* [16,231] -> [0,215] *)
    let r = rgb_val / 36 in
    let g = rgb_val / 6 mod 6 in
    let b = rgb_val mod 6 in
    let r_part =
      List.nth level_map.color_cube_map r |> Option.value ~default:level_map.full_level
    in
    let g_part =
      List.nth level_map.color_cube_map g |> Option.value ~default:level_map.full_level
    in
    let b_part =
      List.nth level_map.color_cube_map b |> Option.value ~default:level_map.full_level
    in
    r_part, g_part, b_part)
;;

let to_rgb_bytes = to_rgb_ints ~level_map:level_map_8bit_per_channel

let to_rgb c =
  let i = to_int c in
  if i < 16
  then `Primary i
  else `RGB (to_rgb_bytes c |> Tuple3.map ~f:(fun rgb -> rgb // 255))
;;

let to_rgb_hex24 c =
  let r, g, b = to_rgb_bytes c in
  sprintf "#%2.2x%2.2x%2.2x" r g b
;;

let tuple3_fold_two
      (t1 : ('a, 'a, 'a) Tuple3.t)
      (t2 : ('b, 'b, 'b) Tuple3.t)
      ~(init : 'c)
      ~(f : 'a -> 'b -> 'c -> 'c)
  : 'c
  =
  f (fst3 t1) (fst3 t2) init |> f (snd3 t1) (snd3 t2) |> f (trd3 t1) (trd3 t2)
;;

let to_luma c =
  let rgb_bytes = to_rgb_bytes c in
  (* ITU BT.601 *)
  let weights = 0.299, 0.587, 0.114 in
  tuple3_fold_two rgb_bytes weights ~init:0. ~f:(fun byte_c weight acc ->
    acc +. (Float.of_int byte_c *. weight))
  /. 255.0
  |> Float.clamp_exn ~min:0. ~max:1.
;;

let to_rgb_8bit = to_rgb_bytes
let to_rgb_int1k c = to_rgb_ints ~level_map:level_map_1000_per_channel c

let to_rgb6 c =
  let offset_ival = to_int c - 16 in
  if offset_ival < 0 || offset_ival > 215
  then
    (* Closest approximation in the color-cube *)
    to_rgb_bytes c |> Tuple3.map ~f:closest_8bit_cube_index
  else (
    let r = offset_ival / 36 in
    let g = offset_ival / 6 mod 6 in
    let b = offset_ival mod 6 in
    r, g, b)
;;
