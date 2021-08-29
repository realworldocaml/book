open Yojson.Basic
open Util

(* http://hackage.haskell.org/package/pandoc-types-1.19/docs/Text-Pandoc-Definition.html *)

type format = string

(** Attributes: identifier, classes, key/values. *)
type attr = string * string list * (string * string) list

(** Target: url, title. *)
type target = string * string

type list_number_style = DefaultStyle | Example | Decimal | LowerRoman | UpperRoman | LowerAlpha | UpperAlpha

type list_number_delim = DefaultDelim | Period | OneParen | TwoParensPeriod

type list_attributes = int * list_number_style * list_number_delim

type math_type = DisplayMath | InlineMath

type quote_type = DoubleQuote | SingleQuote

type inline =
  | Code of attr * string
  | Emph of inline list
  | Image of attr * inline list * target
  | Link of attr * inline list * target
  (* | Math of math_type * string *)
  (* | Note of block list *)
  | Quoted of quote_type * inline list
  | RawInline of string * string
  (* | SoftBreak *)
  | Space
  | SmallCaps of inline list
  | Str of string
  | UnhandledInline of Yojson.Basic.t

and block =
  | BulletList of block list list
  | CodeBlock of attr * string
  | Header of int * attr * inline list
  | OrderedList of list_attributes * block list list
  | Para of inline list
  | Plain of inline list
  | RawBlock of format * string
  | Div of attr * block list
  | UnhandledBlock of Yojson.Basic.t

type t = { api_version : int list; meta : Yojson.Basic.t; blocks : block list }

module JSON = struct
  let element_type e =
    Util.to_string (List.assoc "t" (to_assoc e))

  let element_contents e =
    List.assoc "c" (to_assoc e)

  let to_pair p =
    match Util.to_list p with
    | [x; y] -> x, y
    | _ -> assert false

  let to_triple p =
    match Util.to_list p with
    | [x; y; z] -> x, y, z
    | _ -> assert false

  let to_attr attr =
    let id, classes, keyvals = to_triple attr in
    let id = Util.to_string id in
    let classes = List.map Util.to_string (Util.to_list classes) in
    let keyvals = List.map Util.to_list (Util.to_list keyvals) in
    let keyvals = List.map (function [k;v] -> (Util.to_string k, Util.to_string v) | _ -> assert false) keyvals in
    id, classes, keyvals

  let to_target t =
    let url, title = to_pair t in
    Util.to_string url, Util.to_string title

  let to_list_attributes a =
    let n, ns, nd = to_triple a in
    let n = Util.to_int n in
    let ns =
      match element_type ns with
      | "DefaultStyle" -> DefaultStyle
      | "Example" -> Example
      | "Decimal" -> Decimal
      | "LowerRoman" -> LowerRoman
      | "UpperRoman" -> UpperRoman
      | "LowerAlpha" -> LowerAlpha
      | "UpperAlpha" -> UpperAlpha
      | _ -> assert false
    in
    let nd =
      match element_type nd with
      | "DefaultDelim" -> DefaultDelim
      | "Period" -> Period
      | "OneParen" -> OneParen
      | "TwoParensPeriod" -> TwoParensPeriod
      | _ -> assert false
      in
    n, ns, nd

  (* let to_math_type t = *)
    (* match element_type t with *)
    (* | "DisplayMath" -> DisplayMath *)
    (* | "InlineMath" -> InlineMath *)
    (* | _ -> assert false *)

  let rec to_inline e =
    match element_type e with
    | "Code" ->
       let a, c = to_pair (element_contents e) in
       let a = to_attr a in
       let c = Util.to_string c in
       Code (a, c)
    | "Emph" -> Emph (List.map to_inline (Util.to_list (element_contents e)))
    | "Image" ->
       let a, i, t = to_triple (element_contents e) in
       let a = to_attr a in
       let i = List.map to_inline (Util.to_list i) in
       let t = to_target t in
       Image (a, i, t)
    | "Link" ->
       let a, i, t = to_triple (element_contents e) in
       let a = to_attr a in
       let i = List.map to_inline (Util.to_list i) in
       let t = to_target t in
       Link (a, i, t)
    (* | "Math" -> *)
       (* let t, m = to_pair (element_contents e) in *)
       (* let t = to_math_type t in *)
       (* let m = Util.to_string m in *)
       (* Math (t, m) *)
    (* | "Note" -> Note (List.map to_block (Util.to_list (element_contents e))) *)
    | "Quoted" ->
       let q, l = to_pair (element_contents e) in
       let q =
         match element_type q with
         | "DoubleQuote" -> DoubleQuote
         | "SingleQuote" -> SingleQuote
         | q -> failwith ("Unhandled quote type "^q)
       in
       let l = List.map to_inline (Util.to_list l) in
       Quoted (q, l)
    | "SmallCaps" -> SmallCaps (List.map to_inline (Util.to_list (element_contents e)))
    (* | "SoftBreak" -> SoftBreak *)
    | "Str" -> Str (Util.to_string (element_contents e))
    | "Space" -> Space
    | _ -> UnhandledInline e
      
  and to_block e =
    match element_type e with
    | "BulletList" ->
       let l = Util.to_list (element_contents e) in
       let l = List.map (fun l -> List.map to_block (Util.to_list l)) l in
       BulletList l
    | "CodeBlock" ->
       let attr, code = to_pair (element_contents e) in
       CodeBlock (to_attr attr, Util.to_string code)
    | "Header" ->
       let n, a, t = to_triple (element_contents e) in
       let n = Util.to_int n in
       let a = to_attr a in
       let t = List.map to_inline (Util.to_list t) in
       Header (n, a, t)
    | "OrderedList" ->
       let la, l = to_pair (element_contents e) in
       let la = to_list_attributes la in
       let l = Util.to_list l in
       let l = List.map (fun l -> List.map to_block (Util.to_list l)) l in
       OrderedList (la, l)
    | "Para" -> Para (List.map to_inline (Util.to_list (element_contents e)))
    | "Plain" -> Plain (List.map to_inline (Util.to_list (element_contents e)))
    | "RawBlock" ->
       let fmt, contents = to_pair (element_contents e) in
       RawBlock (Util.to_string fmt, Util.to_string contents)
    | "Div" ->
       let a, l = to_pair (element_contents e) in
       let a = to_attr a in
       let l = Util.to_list l in
       let l = List.map to_block l in
       Div (a, l)
    | _ -> UnhandledBlock e

  let element t c = `Assoc ["t", `String t; "c", c]

  let element_nc t = `Assoc ["t", `String t]

  let of_attr (id, classes, keyvals) =
    let id = `String id in
    let classes = `List (List.map (fun s -> `String s) classes) in
    let keyvals = `List (List.map (fun (k,v) -> `List [`String k; `String v]) keyvals) in
    `List [id; classes; keyvals]

  let of_list_attr (n, style, delim) =
    let n = `Int n in
    let style = match style with
      | DefaultStyle -> "DefaultStyle"
      | Example -> "Example"
      | Decimal -> "Decimal"
      | LowerRoman -> "LowerRoman"
      | UpperRoman -> "UpperRoman"
      | LowerAlpha -> "LowerAlpha"
      | UpperAlpha -> "UpperAlpha"
    in
    let delim = match delim with
      | DefaultDelim -> "DefaultDelim"
      | Period -> "Period"
      | OneParen -> "OneParen"
      | TwoParensPeriod -> "TwoParensPeriod"
    in
    let style = `Assoc ["t", `String style] in
    let delim = `Assoc ["t", `String delim] in
    `List [n; style; delim]

  let of_target (url, title) =
    `List [`String url; `String title]

  let rec of_block = function
    | BulletList l -> element "BulletList" (`List (List.map of_blocks l))
    | CodeBlock (a, s) -> element "CodeBlock" (`List [of_attr a; `String s])
    | Header (n, a, t) -> element "Header" (`List [`Int n; of_attr a; of_inlines t])
    | OrderedList (la, l) -> element "OrderedList" (`List [of_list_attr la; `List (List.map of_blocks l)])
    | Para l -> element "Para" (of_inlines l)
    | Plain l -> element "Plain" (of_inlines l)
    | RawBlock (f, c) -> element "RawBlock" (`List [`String f; `String c])
    | Div (a, l) -> element "Div" (`List [of_attr a; of_blocks l])
    | UnhandledBlock b -> b
  and of_blocks l =
    `List (List.map of_block l)
  and of_inline = function
    | Code (a, t) ->
       let a = of_attr a in
       let t = `String t in
       element "Code" (`List [a; t])
    | Emph i ->
      let i = List.map of_inline i in
      element "Emph" (`List i)
    | Image (a, i, t) ->
       let a = of_attr a in
       let i = of_inlines i in
       let t = of_target t in
       element "Image" (`List [a; i; t])
    | Link (a, i, t) ->
       let a = of_attr a in
       let i = of_inlines i in
       let t = of_target t in
       element "Link" (`List [a; i; t])
    | Quoted (q, i) ->
       let q =
         match q with
         | DoubleQuote -> element_nc "DoubleQuote"
         | SingleQuote -> element_nc "SingleQuote"
       in
       let i = List.map of_inline i in
       let i = `List i in
       element "Quoted" (`List [q; i])
    | RawInline (f, s) ->
      element "RawInline" (`List [`String f; `String s])
    | SmallCaps i -> element "SmallCaps" (of_inlines i)
    | Space -> element_nc "Space"
    | Str s -> element "Str" (`String s)
    | UnhandledInline i -> i
  and of_inlines l =
    `List (List.map of_inline l)
end

(** {2 Reading and writing} *)

let of_json json =
  let json = Util.to_assoc json in
  let blocks = Util.to_list (List.assoc "blocks" json) in
  let blocks = List.map JSON.to_block blocks in
  let api_version = List.assoc "pandoc-api-version" json in
  let api_version = List.map Util.to_int (Util.to_list api_version) in
  let meta = List.assoc "meta" json in
  { blocks; api_version; meta }

let to_json p =
  let blocks = `List (List.map JSON.of_block p.blocks) in
  let api_version = `List (List.map (fun n -> `Int n) p.api_version) in
  let meta = p.meta in
  `Assoc ["blocks", blocks; "pandoc-api-version", api_version; "meta", meta]

(** JSON from markdown file. *)
let json_of_md_file fname =
  let tmp = Filename.temp_file "pandoc" ".json" in
  let cmd = Printf.sprintf "pandoc -f markdown -t json %s -o %s" fname tmp in
  let n = Sys.command cmd in
  assert (n = 0);
  let json = from_file tmp in
  Sys.remove tmp;
  json

let of_md_file fname =
  let json = json_of_md_file fname in
  of_json json

let api_version p = p.api_version

let blocks p = p.blocks

(** {2 Metadata} *)

type meta_value =
  | MetaBool of bool
  | MetaInlines of inline list
  | MetaString of string
  | MetaUnhandled of Yojson.Basic.t

let of_meta e =
  match JSON.element_type e with
  | "MetaBool" -> MetaBool (Util.to_bool (JSON.element_contents e))
  | "MetaInlines" -> MetaInlines (JSON.element_contents e |> Util.to_list |> List.map JSON.to_inline)
  | "MetaString" -> MetaString (Util.to_string (JSON.element_contents e))
  | _ -> MetaUnhandled e

let meta p =
  let m = Util.to_assoc p.meta in
  List.map (fun (k,v) -> k, of_meta v) m

let meta_bool p k =
  match List.assoc k (meta p) with
  | MetaBool b -> b
  | MetaString "yes"
  | MetaInlines [Str "yes"] -> true
  | MetaString "no"
  | MetaInlines [Str "no"] -> false
  | _ -> raise Not_found

let meta_string p k =
  match List.assoc k (meta p) with
  | MetaInlines l ->
    List.map (function Str s -> s | Space -> " " | _ -> raise Not_found) l |> String.concat ""
  | MetaString s -> s
  | _ -> raise Not_found

(** {2 Transforming} *)

(** Change the list of blocks. *)
let replace_blocks f p =
  { p with blocks = f p.blocks }

let map ?(block=(fun _ -> None)) ?(inline=(fun _ -> None)) p =
  let rec map_block b =
    match block b with
    | Some bb -> bb
    | None ->
      match b with
      | BulletList l -> [BulletList (List.map map_blocks l)]
      | CodeBlock _ -> [b]
      | Header (n, a, t) -> [Header (n, a, map_inlines t)]
      | OrderedList (la, l) -> [OrderedList (la, List.map map_blocks l)]
      | Para ii -> [Para (map_inlines ii)]
      | Plain ii -> [Plain (map_inlines ii)]
      | RawBlock _ -> [b]
      | Div (la, l) -> [Div (la, map_blocks l)]
      | UnhandledBlock _ -> [b]
  and map_inline i =
    match inline i with
    | Some ii -> ii
    | None ->
      match i with
      | Code _ -> [i]
      | Emph i -> [Emph (map_inlines i)]
      | Image (a, i, t) -> [Image (a, map_inlines i, t)]
      | Link (a, i, t) -> [Link (a, map_inlines i, t)]
      | Quoted (q, i) -> [Quoted (q, map_inlines i)]
      | RawInline _ -> [i]
      | SmallCaps i -> [SmallCaps (map_inlines i)]
      | Space -> [i]
      | Str _ -> [i]
      | UnhandledInline _ -> [i]
  and map_blocks bb = List.flatten (List.map map_block bb)
  and map_inlines ii = List.flatten (List.map map_inline ii) in
  replace_blocks map_blocks p

let map_inlines f p =
  let block = function
    | Para ii -> Para (f ii)
    | b -> b
  in
  replace_blocks (List.map block) p

let map_blocks f p = map ~block:f p

(** Map a function to every top-level block. *)
let map_top_blocks f p =
  replace_blocks (fun blocks -> List.flatten (List.map f blocks)) p
