open Core
open Async

type attributes = (string * string) list
  [@@deriving sexp]

type element = {
  name : string;
  attrs : attributes;
  childs : item list;
}

and item = [
| `Element of element
| `Data of string
] [@@deriving sexp]

type t = item list
  [@@deriving sexp]

let rec item_of_nethtml (doc:Nethtml.document) : item =
  match doc with
  | Nethtml.Element(name,attrs,childs) ->
    `Element {name; attrs; childs = List.map childs ~f:item_of_nethtml}
  | Nethtml.Data x ->
    `Data x

let rec item_to_nethtml (item:item) : Nethtml.document =
  match item with
  | `Element x ->
    Nethtml.Element(x.name, x.attrs, List.map x.childs ~f:item_to_nethtml)
  | `Data x ->
    Nethtml.Data x

let of_string s =
  Netchannels.with_in_obj_channel
    (new Netchannels.input_string s)
    (Nethtml.parse ~dtd:[])
  |> List.map ~f:item_of_nethtml

let of_file file =
  Reader.file_contents file >>| of_string

let item_of_string_helper ?filename s = match of_string s with
  | x::[] -> Ok x
  | l -> (
    let msg = "expected single HTML item but got" in
    let n = List.length l in
    match filename with
    | None -> error msg n sexp_of_int
    | Some filename -> error msg (filename,n) [%sexp_of: string * int]
  )

let item_of_string s = item_of_string_helper s

let item_of_file filename =
  Reader.file_contents filename
  >>| item_of_string_helper ~filename

let to_string t =
  let docs = List.map t ~f:item_to_nethtml in
  let buf = Buffer.create 2048 in
  Netchannels.with_out_obj_channel (new Netchannels.output_buffer buf)
    (Fn.flip Nethtml.write docs)
  ;
  Buffer.contents buf

let is_elem_node item name' = match item with
  | `Data _ -> false
  | `Element {name; _} -> name' = name

let has_html_extension file =
  Filename.split_extension file
  |> snd
  |> function Some "html" -> true | Some _ | None -> false

let html_files_of_dir dir =
  Sys.readdir dir
  >>| Array.to_list
  >>| List.filter ~f:has_html_extension
  >>| List.map ~f:(Filename.concat dir)

let get_all_nodes tag t =
  let rec helper t =
    List.fold t ~init:[] ~f:(fun accum item ->
      match item with
      | `Element {name; childs; _} ->
        if name = tag then
          item::accum
        else
          (helper childs)@accum
      | `Data _ -> accum
    )
  in
  helper t |> List.rev


let is_nested name t =
  let rec loop have_seen = function
    | `Data _ -> false
    | `Element {name=name'; childs; _} ->
      if have_seen && (name = name') then
        true
      else
        let have_seen = have_seen || (name = name') in
        List.exists childs ~f:(loop have_seen)
  in
  List.exists t ~f:(loop false)


let print_elements_only ?(exclude_elements=[]) ?(keep_attrs=[]) t =
  let rec print_item depth = function
    | `Data _ -> ()
    | `Element {name; attrs; childs} ->
      let equal = String.equal in
      if List.mem ~equal exclude_elements name then
        ()
      else (
        let padding = String.init (2*depth) ~f:(fun _ -> ' ') in
        let attrs =
          List.filter_map attrs ~f:(fun (attr,value) ->
            if List.mem ~equal keep_attrs attr then
              Some (sprintf "%s=%s" attr value)
            else
              None
          )
          |> String.concat ~sep:" "
        in
        printf "%s%s %s\n" padding name attrs;
        List.iter childs ~f:(print_item (depth+1))
      )
  in
  List.iter t ~f:(print_item 0)


let filter_whitespace t =
  let rec f item : item option = match item with
    | `Data x -> (
      if String.for_all x ~f:Char.is_whitespace
      then None
      else Some item
    )
    | `Element {name; attrs; childs} ->
      Some (`Element {
        name;
        attrs;
        childs = List.filter_map childs ~f
      } )
  in
  List.filter_map t ~f

let fold t ~init ~f =
  let rec loop accum item = match item with
    | `Data _ ->
      f accum item
    | `Element {childs;_} ->
      List.fold childs ~init:(f accum item) ~f:loop
  in
  List.fold t ~init ~f:loop

let get_body_childs ~filename t =
  match get_all_nodes "body" t with
  | [] -> failwithf "%s: <body> not found" filename ()
  | _::_::_ -> failwithf "%s: multiple <body> tags found" filename ()
  | (`Data _)::[] -> assert false
  | (`Element {name="body";childs;_})::[] -> childs
  | (`Element _)::[] -> assert false

let replace_id_node_with t ~id ~with_ =
  let rec loop = function
    | [] -> []
    | (`Data _ as x)::rest -> x::(loop rest)
    | (`Element {name;attrs;childs;_})::rest ->
      if List.mem ~equal:Rwo_util.string_pair_equal attrs ("id",id) then
        with_@(loop rest)
      else
        (`Element {name; attrs; childs = loop childs})::(loop rest)
  in
  loop t

let encode x =
  x
  |> String.substr_replace_all ~pattern:"&" ~with_:"&amp;"
  |> String.substr_replace_all ~pattern:"<" ~with_:"&lt;"
  |> String.substr_replace_all ~pattern:">" ~with_:"&gt;"
  |> String.substr_replace_all ~pattern:"\027" ~with_:"^["


(******************************************************************************)
(* Constructors                                                               *)
(******************************************************************************)
let elem tag ?(a=[]) childs = `Element{name=tag; attrs=a; childs}

let div = elem "div"
let span = elem "span"
let p = elem "p"
let pre = elem "pre"
let article = elem "article"
let body = elem "body"
let html = elem "html"

let a = elem "a"
let i = elem "i"
let br = elem "br" []

let ul = elem "ul"
let li = elem "li"

let h1 = elem "h1"
let h2 = elem "h2"
let h3 = elem "h3"
let h4 = elem "h4"
let h5 = elem "h5"
let h6 = elem "h6"

let small = elem "small"
let sup = elem "sup"

let table = elem "table"
let thead = elem "thead"
let th = elem "th"
let tbody = elem "tbody"
let tr = elem "tr"
let td = elem "td"

let dl = elem "dl"
let dd = elem "dd"

let head = elem "head"
let meta = elem "meta"
let title = elem "title"
let script = elem "script"
let link = elem "link"

let nav = elem "nav"
let footer = elem "footer"


(******************************************************************************)
(* Attributes                                                                 *)
(******************************************************************************)
let get_all_attributes t =
  let rec helper t =
    List.fold t ~init:String.Set.empty ~f:(fun accum item -> match item with
    | `Data _ -> accum
    | `Element {name=_; attrs; childs} -> (
      List.fold attrs ~init:accum ~f:(fun accum (name,_) -> Set.add accum name)
      |> Set.union (helper childs)
    ) )
  in
  helper t |> Set.to_list

let check_attrs ?(required=[]) ?(allowed=`Any) attrs_list =
  let attrs_list = List.map attrs_list ~f:fst in
  let attrs = String.Set.of_list attrs_list in
  let required = String.Set.of_list required in
  match List.find_a_dup ~compare:(String.compare) attrs_list with
  | Some x ->
    error "attribute repeated" x sexp_of_string
  | None ->
    if not (Set.is_subset required ~of_:attrs) then
      error "expected attributes not present"
        (Set.diff required attrs) String.Set.sexp_of_t
    else
      match allowed with
      | `Any -> Ok ()
      | `Some allowed ->
        let allowed = String.Set.of_list allowed in
        let remaining = Set.diff attrs required in
        if Set.is_subset remaining ~of_:allowed then
          Ok ()
        else
          error "unexpected attributes present"
            (Set.diff remaining allowed)
            String.Set.sexp_of_t
