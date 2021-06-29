open Base
open Ppxlib
open Ast_builder.Default

let string_to_v loc s =
  let s =
    String.filter s ~f:(function
      | '_' -> false
      | _ -> true)
  in
  let is_decimal_literal =
    (* Note that we don't have to handle things like ".99v" or "9.9.9v", because ocaml
       treats that as a syntax error *)
    String.for_all s ~f:(function
      | '-' | '+' | '.' | '0' .. '9' -> true
      | _ -> false)
  in
  if not is_decimal_literal
  then
    Location.raise_errorf
      ~loc
      "Unsupported literal for Fixed_literal %S@,Only decimal literals are supported"
      s;
  let decimal_part, no_decimal_string =
    match String.lsplit2 ~on:'.' s with
    | None -> "", s
    | Some (integer_part, decimal_part) -> decimal_part, integer_part ^ decimal_part
  in
  let decimals = String.length decimal_part in
  let gloc = { loc with loc_ghost = true } in
  let validate_digit_for_precision body =
    let loc = gloc in
    let precision = String.rstrip decimal_part ~drop:(Char.equal '0') |> String.length in
    let name = Printf.sprintf "With_%i_digits_precision" precision in
    [%expr
      let (_ : Fixed_literal.precision) = [%e pexp_variant ~loc name None] in
      [%e body]]
  in
  (* Only the application node gets a real location, so when we ask merlin for
     the type of the literal in the source, we get [Fixed.t], not [int]. *)
  let function_expr =
    let name =
      Printf.sprintf "Fixed_literal.With_%i_decimals.of_int_exact_exn" decimals
      |> Longident.parse
    in
    pexp_ident ~loc:gloc (Located.mk ~loc:gloc name)
  in
  pexp_apply
    ~loc
    function_expr
    [ Nolabel, pexp_constant ~loc:gloc (Pconst_integer (no_decimal_string, None)) ]
  |> validate_digit_for_precision
;;

let () =
  Driver.register_transformation
    "fixed_literal"
    ~rules:
      [ Context_free.Rule.constant Float 'v' string_to_v
      ; Context_free.Rule.constant Integer 'v' string_to_v
      ]
;;
