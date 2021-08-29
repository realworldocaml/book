open! Core_kernel
module Color_256 = Color_256

(* NOTE: assorted content lifted from lib/console/src/console.ml *)
module Color = struct
  type primary =
    [ `Black
    | `Red
    | `Green
    | `Yellow
    | `Blue
    | `Magenta
    | `Cyan
    | `White
    ]
  [@@deriving sexp_of, compare, hash, equal]

  type t =
    [ primary
    | `Color_256 of Color_256.t
    ]
  [@@deriving sexp_of, compare, hash, equal]

  let to_int_list = function
    | `Black -> [ 30 ]
    | `Red -> [ 31 ]
    | `Green -> [ 32 ]
    | `Yellow -> [ 33 ]
    | `Blue -> [ 34 ]
    | `Magenta -> [ 35 ]
    | `Cyan -> [ 36 ]
    | `White -> [ 37 ]
    | `Color_256 c -> [ 38; 5; Color_256.to_int c ]
  ;;
end

module Attr = struct
  type t =
    [ `Bright
    | `Dim
    | `Underscore
    | `Reverse
    | Color.t
    | `Bg of Color.t
    ]
  [@@deriving sexp_of, compare, hash, equal]

  let to_int_list = function
    | `Bright -> [ 1 ]
    | `Dim -> [ 2 ]
    | `Underscore -> [ 4 ]
    | `Reverse -> [ 7 ]
    (* Background colors are 40..47, foreground 30..37.
       256-color codes start with 48 (bg) or 38 (fg). *)
    | #Color.t as c -> Color.to_int_list c
    | `Bg bg ->
      (match Color.to_int_list bg with
       | ansi_code :: rest -> (ansi_code + 10) :: rest
       | [] -> [] (* NOTE: impossible, but appropriate *))
  ;;

  let list_to_string = function
    | [] -> ""
    | l ->
      sprintf
        "\027[%sm"
        (String.concat
           ~sep:";"
           (List.concat_map l ~f:(fun att ->
              to_int_list att |> List.map ~f:string_of_int)))
  ;;
end

module With_all_attrs = struct
  type t =
    [ Attr.t
    | `Reset
    | `Blink
    | `Hidden
    ]
  [@@deriving sexp_of, compare, hash, equal]

  let to_int_list = function
    | `Reset -> [ 0 ]
    | `Blink -> [ 5 ]
    | `Hidden -> [ 8 ]
    | #Attr.t as attr -> Attr.to_int_list attr
  ;;

  let list_to_string = function
    | [] -> ""
    | l ->
      sprintf
        "\027[%sm"
        (String.concat
           ~sep:";"
           (List.concat_map l ~f:(fun att ->
              to_int_list att |> List.map ~f:string_of_int)))
  ;;
end
