open! Core_kernel
open! Import

module type S = sig
  type t [@@deriving sexp_of]

  val all : t list
end

type 'a t = (module S with type t = 'a)

let command_friendly_name t =
  t |> String.tr ~target:'_' ~replacement:'-' |> String.lowercase
;;

let atom (type a) (m : a t) a =
  let module M = (val m) in
  match [%sexp_of: M.t] a with
  | Atom s -> s
  | List _ as sexp -> raise_s [%sexp "Enum.t expects atomic sexps.", (sexp : Sexp.t)]
;;

let to_string_hum m a = command_friendly_name (atom m a)

let check_field_name (type a) (t : a t) (a : a) field =
  [%test_eq: string] (to_string_hum t a) (command_friendly_name (Field.name field))
;;

let enum (type a) (m : a t) =
  let module M = (val m) in
  List.map M.all ~f:(fun a -> to_string_hum m a, a)
;;

let assert_alphabetic_order_exn here (type a) (m : a t) =
  let module M = (val m) in
  let as_strings = List.map M.all ~f:(atom m) in
  [%test_result: string list]
    ~here:[ here ]
    ~message:"This enumerable type is intended to be defined in alphabetic order"
    ~expect:(List.sort as_strings ~compare:String.compare)
    as_strings
;;

let arg_type' = Command.Arg_type.of_alist_exn
let arg_type m = arg_type' (enum m)

let doc' ?represent_choice_with enum ~doc =
  let choices =
    enum
    |> List.map ~f:fst
    |> List.sort ~compare:String.compare
    |> String.concat ~sep:"|"
  in
  match represent_choice_with with
  | None -> sprintf "(%s) %s" choices doc
  | Some represent_choice_with ->
    let doc =
      if String.is_empty doc
      then ""
      else (
        let separator =
          match doc.[String.length doc - 1] with
          | ',' | '.' -> ""
          | _ -> ","
        in
        sprintf " %s%s" doc separator)
    in
    sprintf "%s%s %s can be (%s)" represent_choice_with doc represent_choice_with choices
;;

let doc ?represent_choice_with m ~doc = doc' ?represent_choice_with (enum m) ~doc

module Make_param = struct
  type 'a t =
    { arg_type : 'a Command.Arg_type.t
    ; doc : string
    }

  let create ?represent_choice_with ~doc m =
    let enum = enum m in
    { arg_type = arg_type' enum; doc = doc' ?represent_choice_with enum ~doc }
  ;;
end

type ('a, 'b) make_param =
  ?represent_choice_with:string
  -> ?aliases:string list
  -> string
  -> doc:string
  -> 'a t
  -> 'b Command.Param.t

let make_param ~f ?represent_choice_with ?aliases flag_name ~doc m =
  let { Make_param.arg_type; doc } = Make_param.create ?represent_choice_with ~doc m in
  Command.Param.flag ?aliases flag_name ~doc (f arg_type)
;;

let make_param_optional_with_default_doc
      ~default
      ?represent_choice_with
      ?aliases
      flag_name
      ~doc
      m
  =
  let { Make_param.arg_type; doc } = Make_param.create ?represent_choice_with ~doc m in
  Command.Param.flag_optional_with_default_doc
    ?aliases
    flag_name
    arg_type
    (fun default -> Sexp.Atom (to_string_hum m default))
    ~default
    ~doc
;;

module Make_stringable (M : S) = struct
  let to_string = to_string_hum (module M)

  let of_string =
    let known_values =
      lazy
        (List.fold
           [%all: M.t]
           ~init:(Map.empty (module String))
           ~f:(fun map t -> Map.set map ~key:(to_string t) ~data:t))
    in
    fun s ->
      match Map.find (force known_values) s with
      | None ->
        let known_values = Map.keys (force known_values) in
        raise_s [%message "Unknown value." s (known_values : string list)]
      | Some t -> t
  ;;
end
