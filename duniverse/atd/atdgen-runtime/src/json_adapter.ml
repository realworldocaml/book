(* Json adapters. See .mli. *)

module type S = sig
  val normalize : Yojson.Safe.t -> Yojson.Safe.t
  val restore : Yojson.Safe.t -> Yojson.Safe.t
end

module Type_field = struct
  module type Param = sig
    val type_field_name : string
  end

  module Make (Param : Param) : S = struct
    open Yojson.Safe

    open Param

    let normalize (x : t) : t =
      match x with
      | `Assoc fields ->
          (match List.assoc type_field_name fields with
           | `String type_ -> `List [ `String type_; x ]
           | exception Not_found -> x
           | _ -> x (* malformed *)
          )
      | `String type_ as x -> x
      | malformed -> malformed

    let restore (x : t) : t =
      match x with
      | `List [ `String type_; `Assoc fields ] ->
          let fields =
            (type_field_name, `String type_) ::
            List.filter (fun (k, v) -> k <> type_field_name) fields
          in
          `Assoc fields
      | `String type_ as x -> x
      | malformed -> malformed
  end

  module Default_param : Param = struct
    let type_field_name = "type"
  end

  include Make (Default_param)
end

module One_field = struct
  open Yojson.Safe

  let normalize (x : t) : t =
    match x with
    | `Assoc [name, value] -> `List [`String name; value]
    | `String _ as x -> x
    | malformed -> malformed

  let restore (x : t) : t =
    match x with
    | `List [`String name; value] -> `Assoc [name, value]
    | `String _ as x -> x
    | malformed -> malformed
end

module Type_and_value_fields = struct
  module type Param = sig
    val type_field_name : string
    val value_field_name : string
    val known_tags : (string list * string) option
  end

  module Make (Param : Param) : S = struct
    open Yojson.Safe
    open Param

    let is_known_tag =
      match known_tags with
      | None -> (fun _ -> true)
      | Some (l, _) ->
          let tbl = Hashtbl.create (2 * List.length l) in
          List.iter (fun x -> Hashtbl.add tbl x ()) l;
          Hashtbl.mem tbl

    let is_catch_all_tag =
      match known_tags with
      | None -> (fun _ -> false)
      | Some (_, s) -> ((=) s)

    let catch_all_tag () =
      match known_tags with
      | None -> assert false
      | Some (_, s) -> s

    let wrap_variant type_ value =
      let variant = `List [`String type_; value] in
      if is_known_tag type_ then
        variant
      else
        `List [ `String (catch_all_tag ()); variant ]

    let wrap_enum type_ =
      if is_known_tag type_ then
        `String type_
      else
        `List [ `String (catch_all_tag ()); `Null ]

    let normalize (x : t) : t =
      let open Yojson.Safe.Util in
      match x with
      | `Assoc fields ->
          let type_ = member type_field_name x |> to_string in
          let found = ref false in
          let fields =
            List.map (fun ((k, v) as field) ->
              if k = value_field_name then (
                found := true;
                (k, wrap_variant type_ v)
              )
              else
                field
            ) fields
          in
          let fields =
            if !found then
              fields
            else
              (value_field_name, wrap_enum type_) :: fields
          in
          `Assoc fields
      | malformed -> malformed

    let unwrap_value (x : t) =
      match x with
      | `String tag -> (tag, None)
      | `List [`String tag; v] ->
          if is_catch_all_tag tag then (
            match v with
            | `List [`String real_tag; `Null] -> (real_tag, None)
            | `List [`String real_tag; real_v] -> (real_tag, Some real_v)
            | _ -> failwith ("Malformed json field " ^ value_field_name)
          )
          else
            (tag, Some v)
      | malformed -> failwith ("Malformed json field " ^ value_field_name)

    let restore (x : t) : t =
      match x with
      | `Assoc fields ->
          let type_ = ref None in
          let fields =
            List.fold_right (fun ((k, tagged) as field) acc ->
              if k = value_field_name then (
                let tag, opt_value = unwrap_value tagged in
                type_ := Some tag;
                match opt_value with
                | None -> acc
                | Some v -> (value_field_name, v) :: acc
              )
              else if k = type_field_name then
                acc
              else
                field :: acc
              ) fields []
          in
          let fields =
            match !type_ with
            | None -> fields
            | Some tag -> (type_field_name, `String tag) :: fields
          in
          `Assoc fields
      | malformed -> malformed
  end
end
