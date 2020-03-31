[@@@ocaml.text "/*"]
module Bisect_visit___expression___ml =
  struct
    let ___bisect_visit___ =
      let point_definitions =
        "\132\149\166\190\000\000\000x\000\000\000\025\000\000\000a\000\000\000a\b\000\000`\000\160M@\160\000MB\160\000NA\160\000VC\160\001\001\tD\160\001\001EE\160\001\001}F\160\001\001\191G\160\001\001\249H\160\001\002\016I\160\001\002\129J\160\001\002\189K\160\001\002\198L\160\001\002\239M\160\001\002\255N\160\001\003(O\160\001\0030P\160\001\003XQ\160\001\003xR\160\001\003\167S\160\001\003\209T\160\001\004+V\160\001\0042U\160\001\004VW" in
      let `Staged cb =
        Bisect.Runtime.register_file "expression.ml" ~point_count:24
          ~point_definitions in
      cb
  end
open Bisect_visit___expression___ml
[@@@ocaml.text "/*"]
let fn _ = ___bisect_visit___ 0; ()
let () =
  if true
  then ((fn 1)[@coverage off])
  else
    (___bisect_visit___ 2;
     (let ___bisect_result___ = fn 2 in
      ___bisect_visit___ 1; ___bisect_result___))
;;let ___bisect_result___ = fn 3 in ___bisect_visit___ 3; ___bisect_result___
;;((fn 4)[@coverage off])
;;((fn (if true then 5 else 6))[@coverage off])
let () =
  let ___bisect_result___ = fn () in
  ___bisect_visit___ 4; ___bisect_result___
let () = ((fn)[@coverage off]) ()
let () =
  (let ___bisect_result___ = fn () in
   ___bisect_visit___ 5; ___bisect_result___);
  ()
let () = fn (); ((())[@coverage off])
let () =
  let ___bisect_result___ = fn @@ () in
  ___bisect_visit___ 6; ___bisect_result___
let () = ((fn)[@coverage off]) @@ ()
let () =
  let ___bisect_result___ = () |> fn in
  ___bisect_visit___ 7; ___bisect_result___
let () = () |> ((fn)[@coverage off])
let fn' _ _ = ___bisect_visit___ 8; ()
let () =
  let ___bisect_result___ = () |> (fn' ()) in
  ___bisect_visit___ 9; ___bisect_result___
let () = () |> ((fn' ())[@coverage off])
let () = () |> (((fn')[@coverage off]) ())
let () =
  (let ___bisect_result___ = () |> fn in
   ___bisect_visit___ 10; ___bisect_result___);
  ()
let () = () |> fn; ((())[@coverage off])
let _ =
  if true
  then (___bisect_visit___ 11; true)
  else if false then (___bisect_visit___ 12; true) else false
let _ =
  if ((true)[@coverage off])
  then true
  else if false then (___bisect_visit___ 13; true) else false
let _ =
  if true
  then (___bisect_visit___ 14; true)
  else if ((false)[@coverage off]) then true else false
let _ =
  if true
  then (___bisect_visit___ 15; true)
  else
    if
      (if true
       then (___bisect_visit___ 16; true)
       else if ((true)[@coverage off]) then true else false)
    then true
    else false
let _ =
  if true
  then (___bisect_visit___ 17; true)
  else
    if
      (if ((true)[@coverage off])
       then true
       else if true then (___bisect_visit___ 18; true) else false)
    then true
    else false
class foo = object method bar = ___bisect_visit___ 19; () end
let () =
  let _ =
    let ___bisect_result___ = new foo in
    ___bisect_visit___ 20; ___bisect_result___ in
  ()
let () = let _ = new foo in ((())[@coverage off])
let () =
  let o =
    let ___bisect_result___ = new foo in
    ___bisect_visit___ 22; ___bisect_result___ in
  (let ___bisect_result___ = o#bar in
   ___bisect_visit___ 21; ___bisect_result___);
  ()
let () =
  let o =
    let ___bisect_result___ = new foo in
    ___bisect_visit___ 23; ___bisect_result___ in
  o#bar; ((())[@coverage off])
