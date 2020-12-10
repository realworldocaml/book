(** A module internal to [Core_bench]. Please look at {!Bench}.

    Abstracts the representation, choice and scaling of units for each variable.
*)
open Core
open Poly

type t =
  | Words
  | Time
  | Gc
  | Cycles
  | Count
  | Percentage



let display_minimum = function
  | Words      -> 0.1
  | Time       -> 0.01
  | Gc         -> 1E-5
  | Cycles     -> 1.0
  | Count      -> 0.01
  | Percentage -> 0.0001 (* 100%=1., not 100.*)

module Magnitude = struct
  type t = One | Kilo | Mega | Giga | Milli | Micro | Nano

  let max = Giga

  let magnitude_ignoring_minimum  v =
    let v = Float.abs v in
    if v < 1E-6 then Nano
    else if v < 1E-3 then Micro
    else if v < 1.0 then Milli
    else if v < 1E3 then One
    else if v < 1E6 then Kilo
    else if v < 1E9 then Mega
    else Giga

  let magnitude unit v =
    let v = Float.abs v in
    if v < display_minimum unit
    then Giga
    else magnitude_ignoring_minimum v

  let float_hum ~decimals suffix div v =
    (Float.to_string_hum ~decimals (v /. div)) ^ suffix

  let f2s suffix div v = float_hum ~decimals:2 suffix div v

  let to_string_nanos = function
    | Nano        -> f2s "e-9ns" 1E-9
    | Micro       -> f2s "e-6ns" 1E-6
    | Milli | One -> f2s "ns"    1.0
    | Kilo        -> f2s "us"    1E3
    | Mega        -> f2s "ms"    1E6
    | Giga        -> f2s "s"     1E9

  let to_string_cycles = function
    | Nano        -> f2s "e-9c" 1E-9
    | Micro       -> f2s "e-6c" 1E-6
    | Milli | One -> f2s "c"    1.0
    | Kilo        -> f2s "kc"   1E3
    | Mega        -> f2s "Mc"   1E6
    | Giga        -> f2s "Gc"   1E9

  let to_string_words = function
    | Nano        -> f2s "e-9w" 1E-9
    | Micro       -> f2s "e-6w" 1E-6
    | Milli | One -> f2s "w"  1.0
    | Kilo        -> f2s "kw" 1E3
    | Mega        -> f2s "Mw" 1E6
    | Giga        -> f2s "Gw" 1E9

  let to_string_count = function
    | Nano  -> f2s "e-9" 1E-9
    | Micro -> f2s "e-6" 1E-6
    | Milli -> f2s "e-3" 1E-3
    | One   -> f2s ""  1.0
    | Kilo  -> f2s "k" 1E3
    | Mega  -> f2s "M" 1E6
    | Giga  -> f2s "G" 1E9


  let to_string_gc = function
    | Nano          -> f2s "e-9" 1E-9
    | Micro | Milli -> f2s "e-3" 1E-3
    | One           -> f2s ""    1.0
    | Kilo          -> f2s "k"   1E3
    | Mega          -> f2s "M"   1E6
    | Giga          -> f2s "G"   1E9

  let to_string_percentage _ =
    f2s "%" 1E-2

  let smaller t1 t2 =
    match t1, t2 with
    | Nano,  _ | _, Nano  -> Nano
    | Micro, _ | _, Micro -> Micro
    | Milli, _ | _, Milli -> Milli
    | One,   _ | _, One   -> One
    | Kilo,  _ | _, Kilo  -> Kilo
    | Mega,  _ | _, Mega  -> Mega
    | _,     _            -> Giga

end

let is_displayed ~show_all_values t v =
  show_all_values || (Float.abs v > display_minimum t)

let is_displayed_opt ~show_all_values t v =
  match v with
  | Some v -> is_displayed ~show_all_values t v
  | None -> false

let to_string ~show_all_values t exp v : Ascii_table.Attr.t list * string  =
  let to_string = function
    | Words      -> Magnitude.to_string_words
    | Time       -> Magnitude.to_string_nanos
    | Gc         -> Magnitude.to_string_gc
    | Cycles     -> Magnitude.to_string_cycles
    | Count      -> Magnitude.to_string_count
    | Percentage -> Magnitude.to_string_percentage
  in
  if is_displayed ~show_all_values:false t v
  then ([], to_string t exp v)
  else if show_all_values then
    let exp = Magnitude.magnitude_ignoring_minimum v in
    ([`Dim], to_string t exp v)
  else ([], "")

let to_ci_string ~show_all_values t exp (left, right) : Ascii_table.Attr.t list * string  =
  let (a1, left), (a2, right) =
    (to_string ~show_all_values t exp left),
    (to_string ~show_all_values t exp right)
  in
  if left = "" && right = ""
  then ([], "")
  else (a1 @ a2, sprintf "%s +%s" left right)

let to_string_opt ~show_all_values t exp = function
  | Some v -> to_string ~show_all_values t exp v
  | None -> ([], "")


