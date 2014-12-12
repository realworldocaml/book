open Core.Std
open Async.Std

type item
= Nethtml.document =
  | Element of (string * (string * string) list * item list)
  | Data of string
with sexp

type t = item list
with sexp

type attributes = (string * string) list
with sexp

let of_string s =
  Netchannels.with_in_obj_channel
    (new Netchannels.input_string s)
    Nethtml.parse

let of_file file =
  Reader.file_contents file >>| of_string

let item_of_string_helper ?filename s = match of_string s with
  | x::[] -> Ok x
  | l -> (
    let msg = "expected single HTML item but got" in
    let n = List.length l in
    match filename with
    | None -> error msg n sexp_of_int
    | Some filename -> error msg (filename,n) <:sexp_of< string * int >>
  )

let item_of_string s = item_of_string_helper s

let item_of_file filename =
  Reader.file_contents filename
  >>| item_of_string_helper ~filename

let to_string docs =
  let buf = Buffer.create 2048 in
  Netchannels.with_out_obj_channel (new Netchannels.output_buffer buf)
    (Fn.flip Nethtml.write docs)
  ;
  Buffer.contents buf


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
      | Nethtml.Element (name,_,childs) ->
        if name = tag then
          item::accum
        else
          (helper childs)@accum
      | Nethtml.Data _ -> accum
    )
  in
  helper t |> List.rev


let is_nested name t =
  let rec loop have_seen = function
    | Nethtml.Data _ -> false
    | Nethtml.Element (name', _, childs) ->
      if have_seen && (name = name') then
        true
      else
        let have_seen = have_seen || (name = name') in
        List.exists childs ~f:(loop have_seen)
  in
  List.exists t ~f:(loop false)


let print_elements_only ?(exclude_elements=[]) ?(keep_attrs=[]) t =
  let rec print_item depth = function
    | Nethtml.Data _ -> ()
    | Nethtml.Element (name, attrs, childs) ->
      if List.mem exclude_elements name then
        ()
      else (
        let padding = String.init (2*depth) ~f:(fun _ -> ' ') in
        let attrs =
          List.filter_map attrs ~f:(fun (attr,value) ->
            if List.mem keep_attrs attr then
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


(******************************************************************************)
(* Constructors                                                               *)
(******************************************************************************)
let item tag ?(a=[]) childs =
  Nethtml.Element(tag, a, childs)

let data s = Nethtml.Data s

let div = item "div"
let span = item "span"

let a = item "a"
let i = item "i"
let br = item "br" []

let ul = item "ul"
let li = item "li"

let h1 = item "h1"
let h2 = item "h2"
let h3 = item "h3"
let h4 = item "h4"
let h5 = item "h5"
let h6 = item "h6"

let table = item "table"
let thead = item "thead"
let th = item "th"
let tbody = item "tbody"
let tr = item "tr"
let td = item "td"

let dl = item "dl"
let dd = item "dd"

let script = item "script"
let link = item "link"


(******************************************************************************)
(* Attributes                                                                 *)
(******************************************************************************)
let get_all_attributes t =
  let rec helper t =
    List.fold t ~init:String.Set.empty ~f:(fun accum item -> match item with
    | Nethtml.Data _ -> accum
    | Nethtml.Element (_, attrs, childs) -> (
      List.fold attrs ~init:accum ~f:(fun accum (name,_) -> Set.add accum name)
      |> Set.union (helper childs)
    ) )
  in
  helper t |> Set.to_list

let check_attrs ?(required=[]) ?(allowed=`Any) attrs_list =
  let attrs_list = List.map attrs_list ~f:fst in
  let attrs = String.Set.of_list attrs_list in
  let required = String.Set.of_list required in
  match List.find_a_dup attrs_list with
  | Some x ->
    error "attribute repeated" x sexp_of_string
  | None ->
    if not (Set.subset required attrs) then
      error "expected attributes not present"
        (Set.diff required attrs) String.Set.sexp_of_t
    else
      match allowed with
      | `Any -> Ok ()
      | `Some allowed ->
        let allowed = String.Set.of_list allowed in
        let remaining = Set.diff attrs required in
        if Set.subset remaining allowed then
          Ok ()
        else
          error "unexpected attributes present"
            (Set.diff remaining allowed)
            String.Set.sexp_of_t
