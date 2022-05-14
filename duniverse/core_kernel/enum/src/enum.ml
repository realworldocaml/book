open! Core
open! Import
include Enum_intf

module Single = struct
  module type S = Sexp_of

  type 'a t = (module S with type t = 'a)

  let command_friendly_name s =
    s
    |> String.tr ~target:'_' ~replacement:'-'
    |> String.lowercase
    |> String.filter ~f:(Char.( <> ) '\'')
  ;;

  let atom (type a) (m : a t) a =
    let module M = (val m) in
    match [%sexp_of: M.t] a with
    | Atom s -> s
    | List _ as sexp -> raise_s [%sexp "Enum.t expects atomic sexps.", (sexp : Sexp.t)]
  ;;

  let to_string_hum m a = command_friendly_name (atom m a)

  let check_field_name t a field =
    [%test_eq: string] (to_string_hum t a) (command_friendly_name (Field.name field))
  ;;
end

type 'a t = (module S with type t = 'a)

let to_string_hum (type a) ((module M) : a t) a = Single.to_string_hum (module M) a

let check_field_name (type a) ((module M) : a t) a field =
  Single.check_field_name (module M) a field
;;

let enum (type a) ((module M) : a t) =
  List.map M.all ~f:(fun a -> to_string_hum (module M) a, a)
;;

let assert_alphabetic_order_exn here (type a) ((module M) : a t) =
  let as_strings = List.map M.all ~f:(Single.atom (module M)) in
  [%test_result: string list]
    ~here:[ here ]
    ~message:"This enumerable type is intended to be defined in alphabetic order"
    ~expect:(List.sort as_strings ~compare:String.compare)
    as_strings
;;

module Rewrite_sexp_of (S : S) : S with type t = S.t = struct
  include S

  let sexp_of_t t = to_string_hum (module S) t |> Sexp.of_string
end

let arg_type
      (type t)
      ?case_sensitive
      ?key
      ?list_values_in_help
      (module S : S with type t = t)
  =
  Command.Arg_type.enumerated_sexpable
    ?key
    ?list_values_in_help
    ?case_sensitive
    (module Rewrite_sexp_of (S))
;;

module Make_param = struct
  type 'a t =
    { arg_type : 'a Command.Arg_type.t
    ; doc : string
    }

  let create ?key ?represent_choice_with ?list_values_in_help ~doc m =
    let doc =
      match represent_choice_with with
      | None -> " " ^ doc
      | Some represent_choice_with -> represent_choice_with ^ " " ^ doc
    in
    { arg_type = arg_type ?key ?list_values_in_help m; doc }
  ;;
end

type ('a, 'b) make_param =
  ?represent_choice_with:string
  -> ?list_values_in_help:bool
  -> ?aliases:string list
  -> ?key:'a Univ_map.Multi.Key.t
  -> string
  -> doc:string
  -> 'a t
  -> 'b Command.Param.t

let make_param
      ~f
      ?represent_choice_with
      ?list_values_in_help
      ?aliases
      ?key
      flag_name
      ~doc
      m
  =
  let { Make_param.arg_type; doc } =
    Make_param.create ?key ?represent_choice_with ?list_values_in_help ~doc m
  in
  Command.Param.flag ?aliases flag_name ~doc (f arg_type)
;;

let make_param_optional_with_default_doc
      (type a)
      ~default
      ?represent_choice_with
      ?list_values_in_help
      ?aliases
      ?key
      flag_name
      ~doc
      (m : a t)
  =
  let { Make_param.arg_type; doc } =
    Make_param.create ?key ?represent_choice_with ?list_values_in_help ~doc m
  in
  Command.Param.flag_optional_with_default_doc
    ?aliases
    flag_name
    arg_type
    (fun default -> Sexp.Atom (to_string_hum (module (val m)) default))
    ~default
    ~doc
;;

let make_param_one_of_flags
      ?(if_nothing_chosen = Command.Param.If_nothing_chosen.Raise)
      ?aliases
      ~doc
      m
  =
  Command.Param.choose_one
    ~if_nothing_chosen
    (List.map (enum m) ~f:(fun (name, enum) ->
       let aliases = Option.map aliases ~f:(fun aliases -> aliases enum) in
       let doc = doc enum in
       Command.Param.flag ?aliases name (Command.Param.no_arg_some enum) ~doc))
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
