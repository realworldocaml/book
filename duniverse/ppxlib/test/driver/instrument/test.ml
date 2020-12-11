open Ppxlib

let extend_list_by name = object
  inherit Ast_traverse.map as super

  method! expression e =
    match e.pexp_desc with
    | Pexp_construct ({txt = Lident "[]"; _}, None) -> Ast_builder.Default.elist ~loc:e.pexp_loc [Ast_builder.Default.estring ~loc:e.pexp_loc name]
    | _ -> super#expression e
end
[%%expect{|
val extend_list_by : string -> Ast_traverse.map = <fun>
|}]

let () =
  let name = "a: instr pos=Before" in
  let transform = extend_list_by name in
  Driver.(register_transformation ~instrument:(Instrument.make ~position:Before transform#structure) name)

let () =
  let name = "b: instr pos=After" in
  let transform = extend_list_by name in
  Driver.(register_transformation ~instrument:(Instrument.make ~position:After transform#structure) name)

let () =
  let name = "c: impl" in
  let transform = extend_list_by name in
  Driver.register_transformation ~impl:transform#structure name

(* The order of the list should only depend on how the rewriters got registered,
   not on the alphabetic order of the names they got registered with. *)
let x = []
[%%expect{|
val x : string list =
  ["a: instr pos=Before"; "c: impl"; "b: instr pos=After"]
|}]
