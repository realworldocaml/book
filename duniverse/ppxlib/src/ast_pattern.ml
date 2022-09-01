open! Import
include Ast_pattern0

let save_context ctx = ctx.matched
let restore_context ctx backup = ctx.matched <- backup
let incr_matched c = c.matched <- c.matched + 1

let parse_res (T f) loc ?on_error x k =
  try Ok (f { matched = 0 } loc x k)
  with Expected (loc, expected) -> (
    match on_error with
    | None -> Error (Location.Error.createf ~loc "%s expected" expected, [])
    | Some f -> Ok (f ()))

let parse (T f) loc ?on_error x k =
  match parse_res (T f) loc ?on_error x k with
  | Ok r -> r
  | Error (r, _) -> Location.Error.raise r

module Packed = struct
  type ('a, 'b) t = T : ('a, 'b, 'c) Ast_pattern0.t * 'b -> ('a, 'c) t

  let create t f = T (t, f)
  let parse_res (T (t, f)) loc x = parse_res t loc x f
  let parse (T (t, f)) loc x = parse t loc x f
end

let __ =
  T
    (fun ctx _loc x k ->
      incr_matched ctx;
      k x)

let __' =
  T
    (fun ctx loc x k ->
      incr_matched ctx;
      k { loc; txt = x })

let drop =
  T
    (fun ctx _loc _ k ->
      incr_matched ctx;
      k)

let as__ (T f1) =
  T
    (fun ctx loc x k ->
      let k = f1 ctx loc x (k x) in
      k)

let cst ~to_string ?(equal = Poly.equal) v =
  T
    (fun ctx loc x k ->
      if equal x v then (
        incr_matched ctx;
        k)
      else fail loc (to_string v))

let int v = cst ~to_string:Int.to_string v
let char v = cst ~to_string:(Printf.sprintf "%C") v
let string v = cst ~to_string:(Printf.sprintf "%S") v
let float v = cst ~to_string:Float.to_string v
let int32 v = cst ~to_string:Int32.to_string v
let int64 v = cst ~to_string:Int64.to_string v
let nativeint v = cst ~to_string:Nativeint.to_string v
let bool v = cst ~to_string:Bool.to_string v

let false_ =
  T
    (fun ctx loc x k ->
      match x with
      | false ->
          ctx.matched <- ctx.matched + 1;
          k
      | _ -> fail loc "false")

let true_ =
  T
    (fun ctx loc x k ->
      match x with
      | true ->
          ctx.matched <- ctx.matched + 1;
          k
      | _ -> fail loc "true")

let nil =
  T
    (fun ctx loc x k ->
      match x with
      | [] ->
          ctx.matched <- ctx.matched + 1;
          k
      | _ -> fail loc "[]")

let ( ^:: ) (T f0) (T f1) =
  T
    (fun ctx loc x k ->
      match x with
      | x0 :: x1 ->
          ctx.matched <- ctx.matched + 1;
          let k = f0 ctx loc x0 k in
          let k = f1 ctx loc x1 k in
          k
      | _ -> fail loc "::")

let none =
  T
    (fun ctx loc x k ->
      match x with
      | None ->
          ctx.matched <- ctx.matched + 1;
          k
      | _ -> fail loc "None")

let some (T f0) =
  T
    (fun ctx loc x k ->
      match x with
      | Some x0 ->
          ctx.matched <- ctx.matched + 1;
          let k = f0 ctx loc x0 k in
          k
      | _ -> fail loc "Some")

let pair (T f1) (T f2) =
  T
    (fun ctx loc (x1, x2) k ->
      let k = f1 ctx loc x1 k in
      let k = f2 ctx loc x2 k in
      k)

let ( ** ) = pair

let triple (T f1) (T f2) (T f3) =
  T
    (fun ctx loc (x1, x2, x3) k ->
      let k = f1 ctx loc x1 k in
      let k = f2 ctx loc x2 k in
      let k = f3 ctx loc x3 k in
      k)

let alt (T f1) (T f2) =
  T
    (fun ctx loc x k ->
      let backup = save_context ctx in
      try f1 ctx loc x k
      with e1 -> (
        let m1 = save_context ctx in
        restore_context ctx backup;
        try f2 ctx loc x k
        with e2 ->
          let m2 = save_context ctx in
          if m1 >= m2 then (
            restore_context ctx m1;
            raise e1)
          else raise e2))

let ( ||| ) = alt
let map (T func) ~f = T (fun ctx loc x k -> func ctx loc x (f k))
let map' (T func) ~f = T (fun ctx loc x k -> func ctx loc x (f loc k))
let map_result (T func) ~f = T (fun ctx loc x k -> f (func ctx loc x k))
let ( >>| ) t f = map t ~f
let map0 (T func) ~f = T (fun ctx loc x k -> func ctx loc x (k f))
let map1 (T func) ~f = T (fun ctx loc x k -> func ctx loc x (fun a -> k (f a)))

let map2 (T func) ~f =
  T (fun ctx loc x k -> func ctx loc x (fun a b -> k (f a b)))

let map0' (T func) ~f = T (fun ctx loc x k -> func ctx loc x (k (f loc)))

let map1' (T func) ~f =
  T (fun ctx loc x k -> func ctx loc x (fun a -> k (f loc a)))

let map2' (T func) ~f =
  T (fun ctx loc x k -> func ctx loc x (fun a b -> k (f loc a b)))

let alt_option some none =
  alt (map1 some ~f:(fun x -> Some x)) (map0 none ~f:None)

let many (T f) =
  T
    (fun ctx loc l k ->
      let rec aux accu = function
        | [] -> k (List.rev accu)
        | x :: xs -> f ctx loc x (fun x -> aux (x :: accu) xs)
      in
      aux [] l)

let loc (T f) = T (fun ctx _loc (x : _ Loc.t) k -> f ctx x.loc x.txt k)
let pack0 t = map t ~f:(fun f -> f ())
let pack2 t = map t ~f:(fun f x y -> f (x, y))
let pack3 t = map t ~f:(fun f x y z -> f (x, y, z))

include Ast_pattern_generated

let echar t = pexp_constant (pconst_char t)
let estring t = pexp_constant (pconst_string t drop drop)
let efloat t = pexp_constant (pconst_float t drop)
let pchar t = ppat_constant (pconst_char t)
let pstring t = ppat_constant (pconst_string t drop drop)
let pfloat t = ppat_constant (pconst_float t drop)
let int' (T f) = T (fun ctx loc x k -> f ctx loc (int_of_string x) k)
let int32' (T f) = T (fun ctx loc x k -> f ctx loc (Int32.of_string x) k)
let int64' (T f) = T (fun ctx loc x k -> f ctx loc (Int64.of_string x) k)

let nativeint' (T f) =
  T (fun ctx loc x k -> f ctx loc (Nativeint.of_string x) k)

let const_int t = pconst_integer (int' t) none
let const_int32 t = pconst_integer (int32' t) (some (char 'l'))
let const_int64 t = pconst_integer (int64' t) (some (char 'L'))
let const_nativeint t = pconst_integer (nativeint' t) (some (char 'n'))
let eint t = pexp_constant (const_int t)
let eint32 t = pexp_constant (const_int32 t)
let eint64 t = pexp_constant (const_int64 t)
let enativeint t = pexp_constant (const_nativeint t)
let pint t = ppat_constant (const_int t)
let pint32 t = ppat_constant (const_int32 t)
let pint64 t = ppat_constant (const_int64 t)
let pnativeint t = ppat_constant (const_nativeint t)
let single_expr_payload t = pstr (pstr_eval t nil ^:: nil)
let no_label t = cst Asttypes.Nolabel ~to_string:(fun _ -> "Nolabel") ** t

let extension (T f1) (T f2) =
  T
    (fun ctx loc ((name : _ Loc.t), payload) k ->
      let k = f1 ctx name.loc name.txt k in
      let k = f2 ctx loc payload k in
      k)

let rec parse_elist (e : Parsetree.expression) acc =
  Common.assert_no_attributes e.pexp_attributes;
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> List.rev acc
  | Pexp_construct ({ txt = Lident "::"; _ }, Some arg) -> (
      Common.assert_no_attributes arg.pexp_attributes;
      match arg.pexp_desc with
      | Pexp_tuple [ hd; tl ] -> parse_elist tl (hd :: acc)
      | _ -> fail arg.pexp_loc "list")
  | _ -> fail e.pexp_loc "list"

let elist (T f) =
  T
    (fun ctx _loc e k ->
      let l = parse_elist e [] in
      incr_matched ctx;
      k (List.map l ~f:(fun x -> f ctx x.Parsetree.pexp_loc x (fun x -> x))))

let esequence (T f) =
  T
    (fun ctx _loc e k ->
      let rec parse_seq expr acc =
        match expr.pexp_desc with
        | Pexp_sequence (expr, next) -> parse_seq next (expr :: acc)
        | _ -> expr :: acc
      in
      k
        (List.rev_map (parse_seq e []) ~f:(fun expr ->
             f ctx expr.pexp_loc expr (fun x -> x))))

let of_func f = T f
let to_func (T f) = f
