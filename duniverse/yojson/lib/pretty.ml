open Printf

let array = Easy_format.list
let record = Easy_format.list
let tuple = { Easy_format.list with
                space_after_opening = false;
                space_before_closing = false;
                align_closing = false }
let variant = { Easy_format.list with
                  space_before_closing = false; }

let rec format std (x : t) =
  match x with
      `Null -> Easy_format.Atom ("null", Easy_format.atom)
    | `Bool x -> Easy_format.Atom ((if x then "true" else "false"), Easy_format.atom)
    | `Int x -> Easy_format.Atom (json_string_of_int x, Easy_format.atom)
    | `Float x ->
        let s =
          if std then std_json_string_of_float x
          else json_string_of_float x
        in
        Easy_format.Atom (s, Easy_format.atom)
    | `String s -> Easy_format.Atom (json_string_of_string s, Easy_format.atom)
    | `Intlit s
    | `Floatlit s
    | `Stringlit s -> Easy_format.Atom (s, Easy_format.atom)
    | `List [] -> Easy_format.Atom ("[]", Easy_format.atom)
    | `List l -> Easy_format.List (("[", ",", "]", array), List.map (format std) l)
    | `Assoc [] -> Easy_format.Atom ("{}", Easy_format.atom)
    | `Assoc l -> Easy_format.List (("{", ",", "}", record), List.map (format_field std) l)
    | `Tuple l ->
        if std then
          format std (`List l)
        else
          if l = [] then
            Easy_format.Atom ("()", Easy_format.atom)
          else
            Easy_format.List (("(", ",", ")", tuple), List.map (format std) l)

    | `Variant (s, None) ->
        if std then
          format std (`String s)
        else
          Easy_format.Atom ("<" ^ json_string_of_string s ^ ">", Easy_format.atom)

    | `Variant (s, Some x) ->
        if std then
          format std (`List [ `String s; x ])
        else
          let op = "<" ^ json_string_of_string s ^ ":" in
          Easy_format.List ((op, "", ">", variant), [format std x])

and format_field std (name, x) =
  let s = sprintf "%s:" (json_string_of_string name) in
  Easy_format.Label ((Easy_format.Atom (s, Easy_format.atom), Easy_format.label), format std x)


let format ?(std = false) x =
  if std && not (is_object_or_array x) then
    json_error
      "Root is not an object or array as requested by the JSON standard"
  else
    format std (x :> t)

let to_string ?std x =
  Easy_format.Pretty.to_string (format ?std x)

let to_channel ?std oc x =
  Easy_format.Pretty.to_channel oc (format ?std x)
