open Soup

exception Error of string

let err fmt = Fmt.kstrf (fun str -> raise (Error str)) fmt

let input =
  if Array.length Sys.argv = 2 then Sys.argv.(1)
  else (
    Fmt.epr "usage: html2md <file>";
    exit 1
  )

let i = Soup.(parse (read_file input))

type item = [
  | `Text   of string
  | `Strong of string
  | `Em     of string
  | `Code   of string
  | `A      of string option * string
  | `Idx    of string
]

and phrase = item list

and table = {
  id     : string option;
  caption: phrase option;
  headers: phrase list;
  body   : phrase list list;
}

type block = [
  | `Link of string * string * string option
  | `Para of item list
  | `Note of int * item list * block list
  | `List of block list
  | `Enum of block list
  | `Descr of (phrase * block) list
  | `Section of section
  | `Blocks of block list
  | `Table of table
  | `Sidebar of item list * block list
  | `Warning of warning
  | `Figure of string
  | `Safari of block list
]

and section = {
  id       : string;
  data_type: string;
  level    : int;
  title    : item list;
  body     : block list;
}

and warning = {
  allow_break: bool;
  w_title    : item list;
  w_body     : block list;
}

let dump ppf n = Fmt.of_to_string Soup.to_string ppf n

let pp_level ppf l =
  let s = String.make l '#' in
  Fmt.string ppf s

let pp_link ppf (rel, href, part) =
  Fmt.pf ppf "<link rel=%S href=%S %a/>\n" rel href
    Fmt.(option (fun ppf -> Fmt.pf ppf "part=%S ")) part

let pp_href ppf = function
  | None  , t -> Fmt.string ppf t
  | Some h, t -> Fmt.pf ppf "[%s](%s)" t h

let pp_item ppf (i:item) = match i with
  | `Em e     -> Fmt.pf ppf "_%s_" e
  | `Strong s -> Fmt.pf ppf "*%s*" s
  | `Idx s    -> Fmt.pf ppf "<idx>%s</idx>" s
  | `Code c   -> Fmt.pf ppf "`%s`" c
  | `A href   -> pp_href ppf href
  | `Text s   -> Fmt.string ppf s

let pp_items ppf is = Fmt.(list ~sep:(unit " ") pp_item) ppf is

let pp_table ppf t =
  let aux ppf () =
    let item_lengh e = String.length (Fmt.to_to_string pp_item e) in
    let len t = List.fold_left (fun acc x -> 1 + item_lengh x + acc) 0 t in
    Fmt.(list ~sep:(unit " | ") pp_items) ppf t.headers;
    Fmt.(list ~sep:(unit "-|-") string) ppf
      (List.map (fun s -> String.make (len s) '-') t.headers);
    Fmt.(list ~sep:(unit "\n")
           Fmt.(list ~sep:(unit " | ") pp_items)
        ) ppf t.body;
    match t.caption with
    | None   -> ()
    | Some c -> Fmt.pf ppf "\nTable: %a" pp_items c
  in
  match t.id with
  | None    -> Fmt.pf ppf "%a\n" aux ()
  | Some id -> Fmt.pf ppf "::: {#%s}\n%a\n:::\n\n" id aux ()

let pp_figure ppf t =
  Fmt.pf ppf
    "<figure style=\"float: 0\">\n\
     \  <img src=%S/>\n\
     </figure>\n"
    t

let rec pp_block ppf (t:block) = match t with
  | `Blocks l     -> pp_blocks ppf l
  | `Link l       -> pp_link ppf l
  | `Para is      -> Fmt.pf ppf "%a\n" pp_items is
  | `Table t      -> pp_table ppf t
  | `Sidebar s    -> pp_sidebar ppf s
  | `Figure f     -> pp_figure ppf f
  | `Descr d      -> pp_descr ppf d
  | `List bs      ->
    (* FIXME: handle depth *)
    List.iter (Fmt.pf ppf "- %a" pp_block) bs
  | `Enum bs      ->
    List.iteri (fun i -> Fmt.pf ppf "%d. %a" i pp_block) bs
  | `Note (l, t, bs) ->
    Fmt.pf ppf "::: data-type=note\n%a %a\n\n%a\n:::\n"
      pp_level l pp_items t pp_blocks bs
  | `Section s    ->
    Fmt.pf ppf "%a %a {#%s data-type=%S}\n\n%a\n"
      pp_level s.level pp_items s.title s.id s.data_type pp_blocks s.body
  | `Safari bs    -> Fmt.pf ppf "::: .safarienabled\n%a\n:::\n\n" pp_blocks bs
  | `Warning w    ->
    let pp_break ppf () =
      if w.allow_break then Fmt.string ppf ".allow_break " else ()
    in
    Fmt.pf ppf "::: %adata-type=warning\n## %a\n\n%a\n:::\n\n"
      pp_break () pp_items w.w_title pp_blocks w.w_body

and pp_blocks ppf bs = Fmt.(list ~sep:(unit "\n") pp_block) ppf bs

and pp_descr ppf t =
  Fmt.pf ppf "%a\n" Fmt.(list ~sep:(unit "\n") pp_list) t

and pp_list ppf (title, body) =
  Fmt.pf ppf "%a\n: %a" pp_items title pp_block body

and pp_sidebar ppf (title, body) =
  Fmt.pf ppf "<aside data-type=\"sidebar\">\n\n%a %a\n\n%a\n\n</aside>"
    pp_level 5 pp_items title pp_blocks body

let rec dump_item ppf (i:item) = match i with
  | `Em e     -> Fmt.pf ppf "Em %S" e
  | `Strong s -> Fmt.pf ppf "Strong %S" s
  | `Idx s    -> Fmt.pf ppf "Idx %S" s
  | `Code c   -> Fmt.pf ppf "Code %S" c
  | `A (h, l) -> Fmt.pf ppf "A (%a, %s)" Fmt.(Dump.option string) h l
  | `Text s   -> Fmt.pf ppf "Text %S" s

and dump_items ppf t = Fmt.Dump.list dump_item ppf t

and dump_block ppf (t:block) = match t with
  | `Blocks l     -> pp_blocks ppf l
  | `Link l       -> pp_link ppf l
  | `Figure f     -> Fmt.pf ppf "@[<2>Figure %S@]\n" f
  | `Para is      -> Fmt.pf ppf "@[<2>Para@ (%a)@]\n" dump_items is
  | `List bs      -> Fmt.pf ppf "@[<2>List@ (%a)@]\n" dump_blocks bs
  | `Descr d      -> Fmt.pf ppf "@[<2>Descr@ (%a)@]\n"
                       Fmt.Dump.(list (pair pp_items pp_block)) d
  | `Enum bs      -> Fmt.pf ppf "@[<2>Enum@ (%a)@]\n" dump_blocks bs
  | `Note (l,t,b) -> Fmt.pf ppf "@[<2>Note@ (%d,@ %a@, %a@)]\n"
                       l dump_items t pp_blocks b
  | `Section s    -> Fmt.pf ppf "@[<2>Section@ (%a)@]\n" dump_level s
  | `Table t      -> pp_table ppf t
  | `Sidebar s    -> pp_sidebar ppf s
  | `Safari s     -> Fmt.pf ppf "@[<2>Safari@ (%a)@]" dump_blocks s
  | `Warning w    -> Fmt.pf ppf "@[<2>Warning@ %a@]" dump_warning w

and dump_warning ppf t =
  Fmt.pf ppf "@[<2>{allow_break=%b; title=%a; body=%a}@]"
    t.allow_break pp_items t.w_title pp_blocks t.w_body

and dump_blocks ppf t = Fmt.Dump.list dump_block ppf t

and dump_level ppf t =
  Fmt.pf ppf "@[{name: %s; level: %a}@]" t.id dump_items t.title


let int_of_level = function
  | "h1" -> 1
  | "h2" -> 2
  | "h3" -> 3
  | "h4" -> 4
  | "h5" -> 5
  | "h6" -> 6
  | s    -> Fmt.invalid_arg "invalid level: %s" s

let filter_map f e =
  List.fold_left (fun acc e -> match f e with
      | None   -> acc
      | Some e -> e :: acc
    ) [] (List.rev e)

let flatten_map f l =
  List.fold_left (fun acc x -> List.rev_append (f x) acc) [] l
  |> List.rev

let pair_map l r x =
  let rec left acc = function
    | []   -> List.rev acc
    | h::t -> match l h with
      | None   -> left acc t
      | Some l -> right l acc t
  and right l acc = function
    | []   -> err "list is even"
    | h::t -> match r h with
      | None   -> right l acc t
      | Some r -> left ((l, r) :: acc) t
  in
  left [] x

let children x = Soup.(to_list @@ children x)

let rec find_header_node = function
  | []   -> err "no header node"
  | h::t ->
    match Soup.element h with
    | None   -> find_header_node t
    | Some e ->
      let i = int_of_level (Soup.name e) in
      i, children e, t

let one_child e = match children e with
  | []  -> err "no child"
  | [e] -> e
  | _   -> err "too many children: %a" dump e

let remove_padding s =
  let buf = Buffer.create (String.length s) in
  let skip = ref true in
  String.iter (function
      | '\n' -> skip := true; Buffer.add_char buf '\n'
      | ' ' | '\t' when !skip -> ()
      | c    -> skip := false; Buffer.add_char buf c
    ) s;
  Buffer.contents buf

module Parse = struct

  let text s = `Text s
  let strong s = `Strong s
  let em s = `Em s
  let a href link = `A (href, link)
  let idx s = `Idx s
  let code s = `Code s

  let rec items h: item list =
    let txt f =
      Soup.texts h
      |> List.map remove_padding
      |> List.map String.trim
      |> List.filter ((<>)"")
      |> String.concat ""
      |> function "" -> [] | s -> [f s]
    in
    match Soup.element h with
    | None   -> txt text
    | Some e ->
      match Soup.name e with
      | "strong" -> txt strong
      | "em"     -> txt em
      | "code"   -> txt code
      | "a"      -> txt (a (Soup.attribute "href" e))
      | "idx"    -> txt idx
      | "span"   ->
        (match Soup.attribute "class" e with
         | Some "command"       ->
           (match items (one_child e) with
            | [`Em x] ->
              (* <span class="command"><em>foo</em></span> == <code>foo</code> *)
              [`Code x]
            | _ -> err "class=\"command\"")
         (* XXX: not sure what it is used for *)
         | Some "keep-together" -> flatten_map items (children e)
         | _ -> err "TODO item: span %a" Fmt.(of_to_string Soup.to_string) e)
      | s -> err "TODO item: %s (%a)" s dump e

  let rec para acc = function
    | []   -> List.rev acc
    | h::t -> para (items h @ acc) t

  let find name f l =
    let rec aux = function
      | []   -> err "cannot find %s in %a" name Fmt.(Dump.list dump) l
      | h::t ->
        match Soup.element h with
        | None   -> aux t
        | Some e -> if Soup.name e = name then f (children e) else aux t
    in
    aux l

  let expect name f e =
    match Soup.element e with
    | None   -> None
    | Some e ->
      if Soup.name e = name then Some (f (children e))
      else err "expecting %s, got %s" name (Soup.name e)

  let find_all name f l =
    let rec aux acc =function
    | []   -> List.rev acc
    | h::t ->
      match Soup.element h with
      | None   -> aux acc t
      | Some e ->
        if Soup.name e <> name then aux acc t
        else aux (f (children e) :: acc) t
    in
    aux [] l

  let table id childs =
    let caption =
      try Some (find "caption" (flatten_map items) childs)
      with Error _ -> None
    in
    let td = find_all "td" (flatten_map items) in
    let th = find_all "th" (flatten_map items) in
    let headers = find "thead" (find "tr" th) childs in
    let body = find "tbody" (find_all "tr" td) childs in
    { id; caption; headers; body }

  let rec maybe_block ?(head=true) n: block option =
    match Soup.element n with
    | None   -> None
    | Some e ->
      match Soup.name e with
      | "section" ->
        let attrs = Soup.fold_attributes (fun acc k v -> (k, v) :: acc) [] e in
        let id =
          try List.assoc "id" attrs
          with Not_found -> err "id not found in %a" dump e
        in
        let data_type =
          try List.assoc "data-type" attrs
          with Not_found -> err "data-type not found in %a" dump e
        in
        let level, title, body = header e in
        let level = if head then level else level + 1 in
        Some (`Section { id; data_type; level; title; body })
      | "p" ->
        let p = para [] (children e) in
        Some (`Para p)
      | "link" ->
        let rel, href, part =
          match
            Soup.attribute "rel" e,
            Soup.attribute "href" e,
            Soup.attribute "part" e
          with
          | Some rel, Some href, part -> rel, href, part
          | None, _, _ -> err "missing rel in link: %a" dump e
          | _, None, _ -> err "missing hrel in link: %a" dump e
        in
        Some (`Link (rel, href, part))
      | "div" ->
        if Soup.attribute "data-type" e = Some "note" then
          let level, title, body = header e in
          Some (`Note (1+level, title, body))
        else if Soup.attribute "class" e = Some "safarienabled" then
          Some (`Safari (filter_map (maybe_block ~head:false) (children e)))
        else if Soup.attribute "data-type" e = Some "warning" then
          let allow_break = Soup.classes e = ["allow_break"] in
          let level, w_title, w_body = header e in
          if level <> 1 then err "wrong header level for warning section";
          let w = { allow_break; w_title; w_body } in
          Some (`Warning w)
        else
          err "unsuported div: %a" dump e
      | "ol" -> Some (`Enum (filter_map li (children e)))
      | "ul" -> Some (`List (filter_map li (children e)))
      | "dl" -> Some (`Descr (pair_map dt dd (children e)))
      | "table" ->
        let id = Soup.id e in
        Some (`Table (table id (children e)))
      | "aside" ->
        if Soup.attribute "data-type" e = Some "sidebar" then
          let level, title, body = header e in
          if level <> 5 then err "wrong header level in sidebar (%d)" level;
          Some (`Sidebar (title, body))
        else
          err "invalid aside %a" dump e
      | "figure" ->
        if Soup.attribute "style" e = Some "float: 0" then
          let img e = match Soup.element e with
            | None   -> None
            | Some e -> match Soup.name e with
              | "img" -> Soup.attribute "src" e
              | s     -> err "figure: %s" s
          in
          (match filter_map img (children e) with
           | [x] -> Some (`Figure x)
           |  _  -> err "figure")
        else
          err "invalid figure"
      | s -> err "TODO block: %s %a" s dump e

  and block ?head e: block = match maybe_block ?head e with
    | None   -> err "expecting a block, got %a" dump e
    | Some b -> b

  and blocks e = `Blocks (filter_map (maybe_block ~head:false) e)

  and header e =
    let level, h, children = find_header_node (children e) in
    let title = flatten_map items h in
    let body = filter_map (maybe_block ~head:false) children in
    level, title, body

  and li e = expect "li" blocks e

  and dt e = expect "dt" (flatten_map items) e

  and dd e = expect "dd" blocks e

end

let () =
  try
    let blocks =
      i |> Soup.children |> Soup.to_list |> filter_map Parse.maybe_block
    in
    List.iter (Fmt.pr "%a\n" pp_block) blocks
  with Error e ->
    Fmt.epr "%s: %s\n%!" input e;
    exit 1
