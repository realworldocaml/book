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

type block = [
  | `Link of string * string * string option
  | `Para of item list
  | `Note of item list * block list
  | `List of block list
  | `Enum of block list
  | `Section of section
  | `Blocks of block list
]

and section = {
  id       : string;
  data_type: string;
  level    : int;
  title    : item list;
  body     : block list;
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

let rec pp_block ppf (t:block) = match t with
  | `Blocks l     -> pp_blocks ppf l
  | `Link l       -> pp_link ppf l
  | `Para is      -> Fmt.pf ppf "%a\n" pp_items is
  | `List bs      ->
    (* FIXME: handle depth *)
    List.iter (Fmt.pf ppf "- %a" pp_block) bs
  | `Enum bs      ->
    List.iteri (fun i -> Fmt.pf ppf "%d. %a" i pp_block) bs
  | `Note (t, bs) ->
    Fmt.pf ppf "::: data-type=note\n# %a\n%a\n:::\n" pp_items t pp_blocks bs
  | `Section s    ->
    Fmt.pf ppf "%a %a {#%s data-type=%S}\n\n%a\n"
      pp_level s.level pp_items s.title s.id s.data_type pp_blocks s.body

and pp_blocks ppf bs = Fmt.(list ~sep:(unit "\n") pp_block) ppf bs

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
  | `Para is      -> Fmt.pf ppf "@[<2>Para@ (%a)@]\n" dump_items is
  | `List bs      -> Fmt.pf ppf "@[<2>List@ (%a)@]\n" dump_blocks bs
  | `Enum bs      -> Fmt.pf ppf "@[<2>Enum@ (%a)@]\n" dump_blocks bs
  | `Note (t, bs) -> Fmt.pf ppf "@[<2>Note@ (%a@, %a@)]\n" dump_items t pp_blocks bs
  | `Section s    -> Fmt.pf ppf "@[<2>Section@ (%a)@]\n" dump_level s

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
  | "h7" -> 7
  | s    -> Fmt.invalid_arg "invalid level: %s" s

let filter_map f e =
  List.fold_left (fun acc e -> match f e with
      | None   -> acc
      | Some e -> e :: acc
    ) [] (List.rev e)

let flatten_map f l =
  List.fold_left (fun acc x -> List.rev_append (f x) acc) [] l
  |> List.rev

let children x = Soup.(to_list @@ children x)

let rec find_header_node = function
  | []   -> failwith "no header node"
  | h::t ->
    match Soup.element h with
    | None   -> find_header_node t
    | Some e ->
      let i = int_of_level (Soup.name e) in
      i, children e, t

let one_child e = match children e with
  | []  -> failwith "no child"
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
            | [`Em x] -> [`Code x]
            | _ -> failwith "class=\"command\"")
         (* XXX: not sure what it is used for *)
         | Some "keep-together" -> flatten_map items (children e)
         | _ -> err "TODO item: span %a" Fmt.(of_to_string Soup.to_string) e)
      | s -> err "TODO item: %s (%a)" s dump e

  let rec parse_para acc = function
    | []   -> List.rev acc
    | h::t -> parse_para (items h @ acc) t

  let rec maybe_block n =
    match Soup.element n with
    | None   -> None
    | Some e ->
      match Soup.name e with
      | "section" ->
        let attrs = Soup.fold_attributes (fun acc k v -> (k, v) :: acc) [] e in
        let id = List.assoc "id" attrs in
        let data_type = List.assoc "data-type" attrs in
        let level, h, children = find_header_node (children e) in
        let title = flatten_map items h in
        let body = filter_map maybe_block children in
        Some (`Section { id; data_type; level; title; body })
      | "p" ->
        let p = parse_para [] (children e) in
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
          let _, h, children = find_header_node (children e) in
          let title = flatten_map items h in
          let body = filter_map maybe_block children in
          Some (`Note (title, body))
        else
          failwith "unsuported div"
      | "ol" -> Some (`Enum (filter_map li (children e)))
      | "ul" -> Some (`List (filter_map li (children e)))
      | s -> err "TODO block: %s %a" s dump e

  and block e = match maybe_block e with
    | None   -> err "expecting a block, got %a" dump e
    | Some b -> b

  and guess_param = function
    | []   -> None
    | h::t ->
      let e =
        try `Items (items h)
        with Error a ->
          (try `Blocks [block h]
           with Error b -> err "%s\n%s" a b)
      in
      match e, guess_param t with
      | _         , None              -> Some e
      | `Blocks e , Some (`Blocks t)  -> Some (`Blocks (e @ t))
      | `Items  e , Some (`Items t)   -> Some (`Items (e @ t))
      | `Items [] , Some (`Blocks t)  -> Some (`Blocks t)
      | `Items  e , Some (`Blocks []) -> Some (`Items e)
      | `Blocks [], Some (`Items t)   -> Some (`Items t)
      | `Blocks e , Some (`Items [])  -> Some (`Blocks e)
      | `Items  e , Some (`Blocks t)  ->
        err "cannot guess: items=%a blocks=%a" dump_items e dump_blocks t
      | `Blocks e , Some (`Items t)   ->
        err "cannot guess: blocks=%a items=%a" dump_blocks e dump_items t

  and li e =
    match Soup.element e with
    | None   -> None
    | Some e -> match Soup.name e with
      | "li" ->
        (match guess_param (children e) with
         | None  -> None
         | Some (`Blocks b) -> Some (`Blocks b)
         | Some (`Items b)  -> Some (`Para b))
      | s    -> err "was expecting <li>, got <%s>" s

end

let blocks =
  i |> Soup.children |> Soup.to_list |> filter_map Parse.maybe_block

let () =
  List.iter (Fmt.pr "%a\n" pp_block) blocks
