open! Import
include Conv_intf

module Make
    (Mode : Mode)
    (Sexp_parser : Parser.S with type parsed_value = Mode.parsed_sexp)
    (Positions_parser : Parser.S with type parsed_value = Positions.t) =
struct
  let reraise positions parsed_value ~sub user_exn =
    let location = Mode.find positions parsed_value ~sub in
    Of_sexp_error.raise ~user_exn ~sub_sexp:sub ~location
  ;;

  let parse_string_exn str f =
    let parsed_value = Sexp_parser.parse_string_exn str in
    match Mode.apply_f parsed_value ~f with
    | x -> x
    | exception Sexp.Of_sexp_error (exn, sub) ->
      let positions = Positions_parser.parse_string_exn str in
      reraise positions parsed_value exn ~sub
  ;;

  let parse_string str f : (_, Conv_error.t) result =
    match parse_string_exn str f with
    | x -> Ok x
    | exception Parse_error.Parse_error e -> Error (Parse_error e)
    | exception Of_sexp_error.Of_sexp_error e -> Error (Of_sexp_error e)
  ;;

  let conv_exn (parsed_value, positions) f =
    match Mode.apply_f parsed_value ~f with
    | x -> x
    | exception Sexp.Of_sexp_error (exn, sub) -> reraise positions parsed_value exn ~sub
  ;;

  let conv x f =
    match conv_exn x f with
    | x -> Ok x
    | exception Of_sexp_error.Of_sexp_error e -> Error e
  ;;

  let conv_combine result f : (_, Conv_error.t) result =
    match result with
    | Error e -> Error (Parse_error e)
    | Ok x ->
      (match conv x f with
       | Ok _ as r -> r
       | Error e -> Error (Of_sexp_error e))
  ;;
end
