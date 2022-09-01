open! Base
open! Ppxlib
open Attrs

module Generic = struct
  type 'specific t =
    | Omit_nil
    | Sexp_array of core_type
    | Sexp_bool
    | Sexp_list of core_type
    | Sexp_option of core_type
    | Specific of 'specific
end

open Generic

let get_attribute attr ld ~f =
  Option.map (Attribute.get attr ld) ~f:(fun x -> f x, Attribute.name attr)
;;

let create ~loc specific_getters ld ~if_no_attribute =
  let generic_getters =
    [ get_attribute omit_nil ~f:(fun () -> Omit_nil)
    ; (fun ld ->
         match ld.pld_type with
         | ty when Option.is_some (Attribute.get bool ld) ->
           (match ty with
            | [%type: bool] -> Some (Sexp_bool, "[@sexp.bool]")
            | _ -> invalid_attribute ~loc bool "bool")
         | ty when Option.is_some (Attribute.get option ld) ->
           (match ty with
            | [%type: [%t? ty] option] -> Some (Sexp_option ty, "[@sexp.option]")
            | _ -> invalid_attribute ~loc option "_ option")
         | ty when Option.is_some (Attribute.get list ld) ->
           (match ty with
            | [%type: [%t? ty] list] -> Some (Sexp_list ty, "[@sexp.list]")
            | _ -> invalid_attribute ~loc list "_ list")
         | ty when Option.is_some (Attribute.get array ld) ->
           (match ty with
            | [%type: [%t? ty] array] -> Some (Sexp_array ty, "[@sexp.array]")
            | _ -> invalid_attribute ~loc array "_ array")
         | _ -> None)
    ]
  in
  let getters =
    let wrapped_getters =
      List.map specific_getters ~f:(fun get ld ->
        Option.map (get ld) ~f:(fun (specific, string) -> Specific specific, string))
    in
    List.concat [ wrapped_getters; generic_getters ]
  in
  match List.filter_map getters ~f:(fun f -> f ld) with
  | [] -> Specific if_no_attribute
  | [ (v, _) ] -> v
  | _ :: _ :: _ as attributes ->
    Location.raise_errorf
      ~loc
      "The following elements are mutually exclusive: %s"
      (String.concat ~sep:" " (List.map attributes ~f:snd))
;;

let strip_attributes =
  object
    inherit Ast_traverse.map
    method! attributes _ = []
  end
;;

let lift_default ~loc ld expr =
  let ty = strip_attributes#core_type ld.pld_type in
  Lifted.create ~loc ~prefix:"default" ~ty expr
;;

let lift_drop_default ~loc ld expr =
  let ty = strip_attributes#core_type ld.pld_type in
  Lifted.create
    ~loc
    ~prefix:"drop_default"
    ~ty:[%type: [%t ty] -> [%t ty] -> Stdlib.Bool.t]
    expr
;;

let lift_drop_if ~loc ld expr =
  let ty = strip_attributes#core_type ld.pld_type in
  Lifted.create ~loc ~prefix:"drop_if" ~ty:[%type: [%t ty] -> Stdlib.Bool.t] expr
;;

module Of_sexp = struct
  type t =
    | Default of expression Lifted.t
    | Required

  let create ~loc ld =
    create
      ~loc
      [ get_attribute default ~f:(fun { to_lift = default } ->
          Default (lift_default ~loc ld default))
      ]
      ld
      ~if_no_attribute:Required
  ;;
end

module Sexp_of = struct
  module Drop = struct
    type t =
      | No_arg
      | Compare
      | Equal
      | Sexp
      | Func of expression Lifted.t
  end

  type t =
    | Drop_default of Drop.t
    | Drop_if of expression Lifted.t
    | Keep

  let create ~loc ld =
    create
      ~loc
      [ get_attribute drop_default ~f:(function
          | None -> Drop_default No_arg
          | Some { to_lift = e } -> Drop_default (Func (lift_drop_default ~loc ld e)))
      ; get_attribute drop_default_equal ~f:(fun () -> Drop_default Equal)
      ; get_attribute drop_default_compare ~f:(fun () -> Drop_default Compare)
      ; get_attribute drop_default_sexp ~f:(fun () -> Drop_default Sexp)
      ; get_attribute drop_if ~f:(fun { to_lift = x } -> Drop_if (lift_drop_if ~loc ld x))
      ]
      ld
      ~if_no_attribute:Keep
  ;;
end
