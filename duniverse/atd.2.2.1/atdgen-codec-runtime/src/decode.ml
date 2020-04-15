type 'a t = Json.t -> 'a

let make f = f

exception DecoderError

let decode f json = f json

let unit = function
  | `Null -> ()
  | _ -> raise DecoderError

let bool = function
  | `Bool b -> b
  | _ -> raise DecoderError

let int = function
  | `Int i -> i
  | _ -> raise DecoderError

let float = function
  | `Float f -> f
  | _ -> raise DecoderError

let char = function
  | `String s when String.length s = 1 -> s.[0]
  | _ -> raise DecoderError

let string = function
  | `String s -> s
  | _ -> raise DecoderError

let list f = function
  | `List l -> List.map f l
  | _ -> raise DecoderError

let array f = function
  | `List l -> Array.map f (Array.of_list l)
  | _ -> raise DecoderError

let obj_list f = function
  | `Assoc l -> List.map (fun (k, v) -> k, f v) l
  | _ -> raise DecoderError

let obj_array f = function
  | `Assoc l -> Array.map (fun (k, v) -> k, f v) (Array.of_list l)
  | _ -> raise DecoderError

let optional f j =
  match f j with
  | exception DecoderError -> None
  | v -> Some v

let map f c j = f (c j)

let field s f = function
  | `Assoc v -> f (List.assoc s v)
  | _ -> raise DecoderError

let fieldOptional s f = function
  | `Assoc v ->
      begin match List.assoc s v with
        | exception Not_found -> None
        | v -> Some (f v)
      end
  | _ -> raise DecoderError

let fieldDefault s default f =
  fieldOptional s f
  |> map (function
    | None -> default
    | Some s -> s)

let tuple1 a = function
  | `Tuple [w]
  | `List [w] -> a w
  | _ -> raise DecoderError

let tuple2 a b = function
  | `Tuple [w ; x]
  | `List [w ; x] -> (a w, b x)
  | _ -> raise DecoderError

let tuple3 a b c = function
  | `Tuple [w; x; y]
  | `List [w; x; y] -> (a w, b x, c y)
  | _ -> raise DecoderError

let tuple4 a b c d = function
  | `Tuple [w; x; y; z]
  | `List [w; x; y; z] -> (a w, b x, c y, d z)
  | _ -> raise DecoderError

let enum l = function
  | `Variant (s, None)
  | `String s ->
      begin match List.assoc s l with
        | exception Not_found -> raise DecoderError
        | `Single a -> a
        | `Decode _ -> raise DecoderError
      end
  | `Variant (s, Some args)
  | `List [`String s; args] ->
      begin match List.assoc s l with
        | exception Not_found -> raise DecoderError
        | `Single _ -> raise DecoderError
        | `Decode d -> decode d args
      end
  | _ -> raise DecoderError

let option_as_constr f =
  enum
    [ "None", `Single None
    ; "Some", `Decode (map (fun x -> Some x) f)
    ]

let nullable f = function
  | `Null -> None
  | x -> Some (f x)

let adapter (normalize: Json.t -> Json.t) (reader: 'a t) json =
  reader (normalize json)
