open Ppxlib

(* --------------------------- Test Setup ----------------------------------- *)

(* These tests check that the inside of payloads is properly expanded or not
   expanded by the driver. *)

let expr_description ~loc ~error expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_integer _) ->
    Ast_builder.Default.estring ~loc "Payload is an integer"
  | Pexp_extension _ ->
    Ast_builder.Default.estring ~loc "Payload is an extension point"
  | _ -> error ()

[%%expect{|
val expr_description :
  loc:location -> error:(unit -> expression) -> expression -> expression =
  <fun>
|}]

let payload_description ~loc ~transformation_name payload =
  let error () =
    Location.raise_errorf ~loc "Invalid %s payload!" transformation_name
  in
  (match payload with
   | PStr [{ pstr_desc = Pstr_eval (expr, _attr); _ }] ->
     expr_description ~loc ~error expr
   | _ -> error ())

[%%expect{|
val payload_description :
  loc:location -> transformation_name:string -> payload -> expression = <fun>
|}]

(* A legacy transformation, rewriting [%legacy_add_one ...] as
   a string, describing the kind of the payload. Only accepts integer and
   extensions as payloads. *)
let legacy_describe_payload =
  object
    inherit Ast_traverse.map as super

    method! expression expr = 
      match expr.pexp_desc with
      | Pexp_extension ({txt = "legacy_describe_payload"; _}, payload) ->
        let loc = expr.pexp_loc in
        payload_description ~loc ~transformation_name:"legacy_describe_payload"
          payload
      | _ -> super#expression expr
    end

let () =
  Driver.register_transformation
    ~impl:legacy_describe_payload#structure
    "legacy_describe_payload"

[%%expect{|
val legacy_describe_payload : Ast_traverse.map = <obj>
|}]

(* A legacy attribute-based generator implemented as a whole AST transformation.
   [type _ = _ [@@gen_x payload]] generates an extra [let x = <string>] where
   [<string>] is a descriptiong of the kind of [payload]. Only accepts integer
   and extensions as payloads. *)
let legacy_deriver =
  let get_gen_x attrs =
      List.find_map
        (function
          | {attr_name = {txt = "gen_x"; _}; attr_payload; attr_loc} ->
            Some (attr_payload, attr_loc)
          | _ -> None)
        attrs
  in
  object(self)
    inherit Ast_traverse.map

    method! structure str =
      List.concat_map
        (fun stri ->
           match stri.pstr_desc with
           | Pstr_type (_, [{ptype_attributes = (_::_ as attrs); _}]) ->
             (match get_gen_x attrs with
              | Some (payload, loc) ->
                let value =
                  payload_description ~loc ~transformation_name:"gen_x" payload
                in
                let stri = self#structure_item stri in
                let x_binding = [%stri let x = [%e value]] in
                [stri; x_binding]
              | None -> [self#structure_item stri])
           | _ -> [self#structure_item stri])
        str
  end

let () =
  Driver.register_transformation
    ~impl:legacy_deriver#structure
    "legacy_deriver"

[%%expect{|
val legacy_deriver : Ast_traverse.map = <obj>
|}]

(* An expression extension that simply expands to its payload.
   I.e. [[%id 1]] expands to [1]. *)
let id =
  Extension.V3.declare
    "id"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~ctxt:_ expr -> expr)
  |> Context_free.Rule.extension

let () = Driver.register_transformation ~rules:[id] "id"

[%%expect{|
val id : Context_free.Rule.t = <abstr>
|}]

(* ------------------------- Actual Test ----------------------------------- *)

(* Context free transformations are applied inside payload of extensions or
   attributes that aren't themselves expanded by context-free rules

   The examples below are expected to display that their paylaod is an integer
   as the extension inside the payload should be expanded during the
   context-free rule pass, that happens before whole AST transformations. *)
let x = [%legacy_describe_payload [%id 1]]

[%%expect{|
val x : string = "Payload is an integer"
|}]

type t = unit
[@@gen_x [%id 1]]

[%%expect{|
type t = unit
val x : string = "Payload is an integer"
|}]

(* --------------------------- Test Setup ----------------------------------- *)

(* The same transformation as [legacy_describe_payload] but written as a
   context-free rule *)
let describe_payload =
  Extension.V3.declare
    "describe_payload"
    Extension.Context.expression
    Ast_pattern.__
    (fun ~ctxt payload ->
       let loc = Expansion_context.Extension.extension_point_loc ctxt in
       payload_description ~loc ~transformation_name:"describe_payload" payload)
  |> Context_free.Rule.extension

let () = Driver.register_transformation ~rules:[describe_payload] "describe_payload"

[%%expect{|
val describe_payload : Context_free.Rule.t = <abstr>
|}]

(* A deriver that accepts a [payload] argument. It generates a value binding
   to a string describing the nature of its payload.
   E.g. [type t = _ [@@deriving x ~payload:1]] will derive
   [let x = "Payload is an integer"].
   The value argument only accepts integer and extensions. *)
let deriver =
  let expand ~ctxt _type_decl payload =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    let value =
      match payload with
      | None -> Location.raise_errorf ~loc "payload argument is mandatory"
      | Some expr ->
        let error () =
          Location.raise_errorf ~loc "Invalid 'deriving x' payload!"
        in
        expr_description ~loc ~error expr
    in
    [%str let x = [%e value]]
  in
  let args =
    let open Deriving.Args in
    let payload = arg "payload" Ast_pattern.__ in
    empty +> payload
  in
  let str_type_decl =
    Deriving.Generator.V2.make args expand
  in
  Deriving.add ~str_type_decl "x"

[%%expect{|
val deriver : Deriving.t = <abstr>
|}]

(* ------------------------- Actual Test ----------------------------------- *)

(* Context-free transformations cannot be applied inside the payload of
   extensions that are themselves expanded by a context-free rule,
   simply because the outermost extension is expanded first.

   The example below should describe their payload to be an extension
   because the extension inside their payload should NOT be expanded when they
   run.

   This is an expected and relatively sane behaviour. As Carl Eastlund pointed
   out, it might make sense at some point to allow expander to ask ppxlib to
   expand a node explicitly via a callback but it shouldn't be done by default.
   *)
let y = [%describe_payload [%id 1]]

[%%expect{|
val y : string = "Payload is an extension point"
|}]

(* Context-free transformations should not be applied inside the payload of
   attributes interpreted by other context-free rules. This is a bug introduced
   in https://github.com/ocaml-ppx/ppxlib/pull/279.

   The example below should report the payload as being an extension point as
   the [value] argument in the paylaod should NOT be expanded.

   Here, just as in extensions, we might eventually provide a callback to expand
   nodes explicitly. *)
type u = unit
[@@deriving x ~payload:[%id 1]]

[%%expect{|
type u = t
val x : string = "Payload is an extension point"
|}]
