(* This code is in the public domain *)

open Astring

(* Version number (v|V).major.minor[.patch][(+|-)info] *)

let parse_version : string -> (int * int * int * string option) option =
fun s -> try
  let parse_opt_v s = match String.Sub.head s with
  | Some ('v'|'V') -> String.Sub.tail s
  | Some _ -> s
  | None -> raise Exit
  in
  let parse_dot s = match String.Sub.head s with
  | Some '.' -> String.Sub.tail s
  | Some _ | None -> raise Exit
  in
  let parse_int s =
    match String.Sub.span ~min:1 ~sat:Char.Ascii.is_digit s with
    | (i, _) when String.Sub.is_empty i -> raise Exit
    | (i, s) ->
        match String.Sub.to_int i with
        | None -> raise Exit | Some i -> i, s
  in
  let maj, s = parse_int (parse_opt_v (String.sub s)) in
  let min, s = parse_int (parse_dot s) in
  let patch, s = match String.Sub.head s with
  | Some '.' -> parse_int (parse_dot s)
  | _ -> 0, s
  in
  let info = match String.Sub.head s with
  | Some ('+' | '-') -> Some (String.Sub.(to_string (tail s)))
  | Some _ -> raise Exit
  | None -> None
  in
  Some (maj, min, patch, info)
with Exit -> None

(* Key value bindings *)

let parse_env : string -> string String.map option =
fun s -> try
  let skip_white s = String.Sub.drop ~sat:Char.Ascii.is_white s in
  let parse_key s =
    let id_char c = Char.Ascii.is_letter c || c = '_' in
    match String.Sub.span ~min:1 ~sat:id_char s with
    | (key, _) when String.Sub.is_empty key -> raise Exit
    | (key, rem) -> (String.Sub.to_string key), rem
  in
  let parse_eq s = match String.Sub.head s with
  | Some '=' -> String.Sub.tail s
  | Some _ | None -> raise Exit
  in
  let parse_value s = match String.Sub.head s with
  | Some '"' -> (* quoted *)
      let is_data = function '\\' | '"' -> false | _ -> true in
      let rec loop acc s =
        let data, rem = String.Sub.span ~sat:is_data s in
        match String.Sub.head rem with
        | Some '"' ->
            let acc = List.rev (data :: acc) in
            String.Sub.(to_string @@ concat acc), (String.Sub.tail rem)
        | Some '\\' ->
            let rem = String.Sub.tail rem in
            begin match String.Sub.head rem with
            | Some ('"' | '\\' as c) ->
                let acc = String.(sub (of_char c)) :: data :: acc in
                loop acc (String.Sub.tail rem)
            | Some _ | None -> raise Exit
            end
        | None | Some _ -> raise Exit
      in
      loop [] (String.Sub.tail s)
  | Some _ ->
      let is_data c = not (Char.Ascii.is_white c) in
      let data, rem = String.Sub.span ~sat:is_data s in
      String.Sub.to_string data, rem
  | None -> "", s
  in
  let rec parse_bindings acc s =
    if String.Sub.is_empty s then acc else
    let key, s = parse_key s in
    let value, s = s |> skip_white |> parse_eq |> skip_white |> parse_value in
    parse_bindings (String.Map.add key value acc) (skip_white s)
  in
  Some (String.sub s |> skip_white |> parse_bindings String.Map.empty)
with Exit -> None
