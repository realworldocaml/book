open Import

type t = Astlib.Location.Error.t

let to_extension (error : Astlib.Location.Error.t) =
  let open Astlib.Location.Error in
  let open Ast_helper in
  if not (is_well_formed error) then
    raise (Invalid_argument "to_extension: expected kind Report_error");
  let sub_msgs = sub_msgs error in
  let main_msg = main_msg error in
  let err_extension_name loc = { Location.loc; txt = "ocaml.error" } in
  let mk_string_constant x = Str.eval (Exp.constant (Const.string x)) in
  let extension_of_sub_msg (sub_msg : string Location.loc) =
    Str.extension
      (err_extension_name sub_msg.loc, PStr [ mk_string_constant sub_msg.txt ])
  in
  ( err_extension_name main_msg.loc,
    Parsetree.PStr
      (mk_string_constant main_msg.txt :: List.map extension_of_sub_msg sub_msgs)
  )

let register_error_of_exn = Astlib.Location.register_error_of_exn

let message error =
  let { Astlib.Location.txt; _ } = Astlib.Location.Error.main_msg error in
  txt

let set_message = Astlib.Location.Error.set_main_msg

let make ~loc txt ~sub =
  let sub = List.map (fun (loc, txt) -> { Astlib.Location.loc; txt }) sub in
  Astlib.Location.Error.make ~sub { loc; txt }

let update_loc = Astlib.Location.Error.set_main_loc

let get_location error =
  let { Astlib.Location.loc; _ } = Astlib.Location.Error.main_msg error in
  loc

let of_exn = Astlib.Location.Error.of_exn
let raise error = raise (Astlib.Location.Error error)
