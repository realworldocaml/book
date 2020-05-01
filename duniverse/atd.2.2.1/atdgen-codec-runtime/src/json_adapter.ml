(* Json adapters. See .mli. *)

module type S = sig
  val normalize : Json.t -> Json.t
  val restore : Json.t -> Json.t
end

module Type_field = struct
  module type Param = sig
    val type_field_name : string
  end

  module Make (Param : Param) : S = struct
    open Param

    let normalize (x : Json.t) : Json.t =
      match x with
      | `Assoc fields ->
          (match List.assoc type_field_name fields with
           | `String type_ -> `List [ `String type_; x ]
           | exception Not_found -> x
           | _ -> x (* malformed *)
          )
      | `String _ as x -> x
      | malformed -> malformed

    let restore (x : Json.t) : Json.t =
      match x with
      | `List [ `String type_; `Assoc fields ] ->
          let fields =
            (type_field_name, `String type_) ::
            List.filter (fun (k, _) -> k <> type_field_name) fields
          in
          `Assoc fields
      | `String _ as x -> x
      | malformed -> malformed
  end

  module Default_param : Param = struct
    let type_field_name = "type"
  end

  include Make (Default_param)
end
