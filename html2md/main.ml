exception Error of string

let err fmt = Fmt.kstrf (fun str -> raise (Error str)) fmt

type item = [
  | `Text   of string
  | `Strong of item list
  | `Em     of string
  | `Code   of string
  | `A      of string * item list
  | `Idx    of string option * string
  | `Keep_together of string
  | `Index_term of string
  | `Xref of string * item list
  | `Filename of string
  (* only in 00-prologue *)
  | `Uri of string * item list
  | `A_hide of string * item list
  | `A_hide_i of string * item list
  | `Email of string
  | `Hyperlink of string
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
  | `Note of unit part
  | `List of block list
  | `Enum of block list
  | `Descr of (phrase * block) list
  | `Section of section part
  | `Blocks of block list
  | `Table of table
  | `Sidebar of item list * block list
  | `Warning of bool part
  | `Figure of string
  | `Safari of block list
  (* only in 00-prologue *)
  | `Simple_list of block list
]

and 'a part = {
  level: int;
  title: item list;
  body : block list;
  v    : 'a;
}

and section = {
  id       : string;
  data_type: string;
}

let dump ppf n = Fmt.of_to_string Soup.to_string ppf n

(* Like [Fmt.words] but without triming *)
let pp_words ppf s =
  let is_white = function ' ' | '\t' .. '\r'  -> true | _ -> false in
  let last = ref 'x' in
  String.iter (fun c ->
      if is_white c && is_white !last then ()
      else if is_white c then Format.pp_print_space ppf ()
      else Format.pp_print_char ppf c;
      last := c
    ) s

let list_v pp = Fmt.vbox Fmt.(list ~sep:(unit "@,@,") pp)
let list_h pp = Fmt.box Fmt.(list ~sep:(unit "") pp)

let listi_v pp ppf l =
  let l = List.mapi (fun i x -> i, x) l in
  list_v pp ppf l

let pp_level ppf l =
  let s = String.make l '#' in
  Fmt.string ppf s

let pp_link ppf (rel, href, part) =
  let pp_part ppf s = Fmt.pf ppf "part=\"%a\" " pp_words s in
  Fmt.pf ppf "<link rel=\"%a\" href=\"%a\" %a/>"
    pp_words rel pp_words href Fmt.(option pp_part) part

let pp_index_term ppf s =
  Fmt.pf ppf "<a data-type=\"indexterm\" data-startref=\"%s\">&nbsp;</a>" s

let rec pp_item ppf (i:item) = match i with
  | `Em e     -> Fmt.pf ppf "*%a*" pp_words e
  | `Strong s -> Fmt.pf ppf "**%a**" pp_items s
  | `Idx (None  , x) -> Fmt.pf ppf "<idx>%a</idx>" pp_words x
  | `Idx (Some y, x) -> Fmt.pf ppf "<idx id=\"%s\">%a</idx>" y pp_words x
  | `Code c   -> Fmt.pf ppf "`%a`" pp_words c
  | `A href   -> pp_href ppf href
  | `Xref x   -> pp_xref ppf x
  | `Text s   -> pp_words ppf s
  | `Email s  -> Fmt.pf ppf "@[[_%s_](mailto:%s){.email}@]" s s
  | `Keep_together s -> Fmt.pf ppf "<span class=\"keep-together\">%s</span>" s
  | `Index_term s    -> pp_index_term ppf s
  | `Filename f      -> Fmt.pf ppf "<em class=\"filename\">%a</em>" pp_words f
  | `A_hide (h, t)   -> Fmt.pf ppf "@[[%a](%s){.orm:hideurl}@]" pp_items t h
  | `A_hide_i (h, t) -> Fmt.pf ppf "@[[%a](%s){.orm:hideurl:ital}@]" pp_items t h
  | `Uri (h, t)      -> Fmt.pf ppf  "@[[%a](%s)@]" pp_items t h
  | `Hyperlink s     -> Fmt.pf ppf "<em class=\"hyperlink\">%a</em>" pp_words s

and pp_items ppf l = list_h pp_item ppf l

and pp_href ppf (h, t) = Fmt.pf ppf "@[[%a](%a)@]" pp_items t pp_words h

and pp_xref ppf (h, t) =
  Fmt.pf ppf "@[[%a](%a){data-type=xref}@]" pp_items t pp_words h

let pp_table ppf t =
  let aux ppf () =
    let item_lengh e = String.length (Fmt.to_to_string pp_item e) in
    let len t = List.fold_left (fun acc x -> 1 + item_lengh x + acc) 0 t in
    Fmt.(list ~sep:(unit " | ") pp_items) ppf t.headers;
    Fmt.pf ppf "@,";
    Fmt.(list ~sep:(unit "-|-") string) ppf
      (List.map (fun s -> String.make (len s) '-') t.headers);
    Fmt.pf ppf "@,";
    Fmt.(list ~sep:(unit "@,")
           Fmt.(list ~sep:(unit " | ") pp_items)
        ) ppf t.body;
    Fmt.pf ppf "@,";
    match t.caption with
    | None   -> ()
    | Some c -> Fmt.pf ppf "@,Table: %a" pp_items c
  in
  match t.id with
  | None    -> Fmt.pf ppf "%a@," aux ()
  | Some id -> Fmt.pf ppf "::: {#%s data-type=table}@,%a@,:::@,@," id aux ()

let pp_figure ppf t =
  Fmt.pf ppf
    "<figure style=\"float: 0\">@,\
     \  <img src=%S/>@,\
     </figure>@,"
    t

let rec pp_block ppf (t:block) = match t with
  | `Blocks l     -> pp_blocks ppf l
  | `Link l       -> pp_link ppf l
  | `Para is      -> pp_para ppf is
  | `Table t      -> pp_table ppf t
  | `Sidebar s    -> pp_sidebar ppf s
  | `Figure f     -> pp_figure ppf f
  | `Descr d      -> pp_descr ppf d
  | `List bs      -> pp_list ppf bs
  | `Enum bs      -> pp_enum ppf bs
  | `Note n       -> pp_note ppf n
  | `Section s    -> pp_section ppf s
  | `Safari bs    -> Fmt.pf ppf "::: safarienabled@,%a@,:::" pp_blocks bs
  | `Warning w    -> pp_warning ppf w
  | `Simple_list l -> pp_simple_list ppf l

and pp_note ppf n =
  Fmt.pf ppf "@[::: {data-type=note}@]@,@[%a %a@]@,@,@[%a@]@,:::@,"
    pp_level n.level pp_items n.title pp_blocks n.body

and pp_warning ppf w =
  let pp_break ppf b = if b then Fmt.string ppf ".allow_break " else () in
  Fmt.pf ppf "@[::: {%adata-type=warning}@]@.@[%a %a@]@.@.@[%a@]@.:::@."
    pp_break w.v pp_level w.level pp_items w.title pp_blocks w.body

and pp_section ppf s =
  Fmt.pf ppf "@[%a %a {#%s data-type=%S}@]@.@.%a@."
    pp_level s.level pp_items s.title s.v.id s.v.data_type pp_blocks s.body

and pp_para ppf p = Fmt.pf ppf "%a" (list_h pp_item) p
and pp_enum ppf s = listi_v pp_enum_descr ppf s
and pp_enum_descr ppf (i, s) = Fmt.pf ppf "@[<2>%d. %a@]" (i+1) pp_block s

and pp_blocks ppf bs = Fmt.pf ppf "@[<2>%a@]" (list_v pp_block) bs

and pp_list ppf t = list_v pp_list_elt ppf t
and pp_list_elt ppf t = Fmt.pf ppf "@[<2>- %a@]" pp_block t

and pp_simple_list ppf t = Fmt.pf ppf "::: {.simplelist}@,%a@,:::@," pp_list t

and pp_descr ppf t = list_v pp_descr_elt ppf t
and pp_descr_elt ppf (title, body) =
  Fmt.pf ppf "@[%a@,@[<2>: %a@]]" pp_items title pp_block body

and pp_sidebar ppf (title, body) =
  Fmt.pf ppf "<aside data-type=\"sidebar\">@,@,%a %a@,@,%a@,@,</aside>"
    pp_level 5 pp_items title pp_blocks body

let rec dump_item ppf (i:item) = match i with
  | `Em e     -> Fmt.pf ppf "Em %S" e
  | `Strong s -> Fmt.pf ppf "Strong %a" dump_items s
  | `Idx s    -> Fmt.pf ppf "Idx %a" Fmt.(Dump.pair Dump.(option string) string) s
  | `Code c   -> Fmt.pf ppf "Code %S" c
  | `A (h, l) -> Fmt.pf ppf "A (%S, %a)" h dump_items l
  | `Text s   -> Fmt.pf ppf "Text %S" s
  | `Email s  -> Fmt.pf ppf "Email %S" s
  | `Filename f      -> Fmt.pf ppf "Filename %S" f
  | `Keep_together s -> Fmt.pf ppf "Keep_together %S" s
  | `Index_term s    -> Fmt.pf ppf "Index_term %S" s
  | `Xref (h,l)      -> Fmt.pf ppf "Xref (%S, %a)" h dump_items l
  | `A_hide (h,l)    -> Fmt.pf ppf "A_hide (%S, %a)" h dump_items l
  | `A_hide_i (h,l)  -> Fmt.pf ppf "A_hide_i (%S, %a)" h dump_items l
  | `Uri (h,l)       -> Fmt.pf ppf "Uri (%S, %a)" h dump_items l
  | `Hyperlink s     -> Fmt.pf ppf "Hyperlink %S" s

and dump_items ppf t = Fmt.Dump.list dump_item ppf t

and dump_block ppf (t:block) = match t with
  | `Blocks l  -> Fmt.pf ppf "@[<2>Block %a@]" dump_blocks l
  | `Link l    -> pp_link ppf l
  | `Figure f  -> Fmt.pf ppf "@[<2>Figure %S@]" f
  | `Para is   -> Fmt.pf ppf "@[<2>Para@ (%a)@]" dump_items is
  | `List bs   -> Fmt.pf ppf "@[<2>List@ (%a)@]" dump_blocks bs
  | `Descr d   -> Fmt.pf ppf "@[<2>Descr@ (%a)@]" dump_descr d
  | `Enum bs   -> Fmt.pf ppf "@[<2>Enum@ (%a)@]" dump_blocks bs
  | `Note n    -> Fmt.pf ppf "@[<2>Note@ %a]" (dump_part dump_unit) n
  | `Section s -> Fmt.pf ppf "@[<2>Section@ (%a)@]" (dump_part dump_section) s
  | `Table t   -> dump_table ppf t
  | `Sidebar s -> pp_sidebar ppf s
  | `Safari s  -> Fmt.pf ppf "@[<2>Safari@ (%a)@]" dump_blocks s
  | `Warning w -> Fmt.pf ppf "@[<2>Warning@ %a@]" (dump_part Fmt.bool) w
  | `Simple_list s -> Fmt.pf ppf "@[<2>Simple_list (%a)@]" dump_blocks s

and dump_part: type a. a Fmt.t -> a part Fmt.t = fun dump_v ppf t ->
  Fmt.pf ppf "@[<2>{level=%d; title=%a; body=%a; %a}@]"
    t.level pp_items t.title dump_blocks t.body dump_v t.v

and dump_descr ppf d = Fmt.Dump.(list (pair dump_items pp_block)) ppf d
and dump_unit ppf () = Fmt.string ppf ""
and dump_blocks ppf t = Fmt.Dump.list dump_block ppf t

and dump_section ppf s =
  Fmt.pf ppf "@[id:@ %S;@ data_type:@ %S@]" s.id s.data_type

and dump_table ppf (t:table) =
  Fmt.pf ppf "@[<2>{id: %a; caption: %a; headers: %a; body: %a}@]"
    Fmt.(Dump.option string) t.id
    Fmt.(Dump.option dump_items) t.caption
    Fmt.(Dump.list dump_items) t.headers
    Fmt.(Dump.list (Dump.list dump_items)) t.body

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

module Parse = struct

  let text s = `Text s
  let strong s = `Strong s
  let em s = `Em s
  let idx x s = `Idx (x, s)
  let code s = `Code s
  let keep_together s = `Keep_together s
  let hyperlink s = `Hyperlink s
  let filename s = `Filename s

  let normalize t =
    let text txt = `Text (String.concat "" (List.rev txt)) in
    let return txt acc = match txt with
      | [] -> List.rev acc
      | _  -> List.rev (text txt :: acc)
    in
    let rec aux txt acc = function
      | [] -> return txt acc
      | `Text x :: t when String.trim x = "" -> aux txt acc t
      | `Text x :: t -> aux (x :: txt) acc t
      | h::t -> match txt with
        | [] -> aux [] (h::acc) t
        | _  -> aux [] (h :: text txt :: acc) t
    in
    aux [] [] t

  let attrs e =
    let compare (a, _) (b, _) = String.compare a b in
    let attrs = Soup.fold_attributes (fun acc k v -> (k, v) :: acc) [] e in
    List.sort compare attrs

  let link e: block =
    match attrs e with
    | ["href", href; "rel", rel] -> `Link (rel, href, None)
    | ["href", href; "part", part; "rel", rel] -> `Link (rel, href, Some part)
    | _ -> err "invalid link: %a" dump e

  let no_attrs e =
    match Soup.element e with
    | None   -> ()
    | Some e ->
      let attrs = attrs e in
      if attrs <> [] then
        err "item has attributes: %a"
          Fmt.(Dump.list Dump.(pair string string)) attrs

  let rec items h: item list =
    let txt f =
      Soup.texts h
      |> String.concat ""
      |> function "" -> [] | s -> [f s]
    in
    match Soup.element h with
    | None   -> no_attrs h; txt text
    | Some e ->
      match Soup.name e with
      | "strong" -> no_attrs h; [`Strong (normalize_items (children e))]
      | "em" when attrs e = ["class", "hyperlink"] -> txt hyperlink
      | "em" when attrs e = ["class", "filename"]  -> txt filename
      | "em"     -> no_attrs h; txt em
      | "code"   -> no_attrs h; txt code
      | "a"      -> [a e]
      | "idx"    -> (match attrs e with
          | ["id", id] -> txt (idx (Some id))
          | []        -> txt (idx None)
          | _         -> err "invalid idx: %a" dump e)
      | "span"   ->
        (match Soup.attribute "class" e with
         | Some "command"       ->
           (match items (one_child e) with
            | [`Em x] ->
              (* <span class="command"><em>foo</em></span> == <code>foo</code> *)
              [`Code x]
            | _ -> err "class=\"command\"")
         (* XXX: not sure what it is used for *)
         | Some "keep-together" -> txt keep_together
         | _ -> err "TODO item: span %a" dump e)
      | s -> err "TODO item: %s (%a)" s dump e

  and normalize_items h = normalize (flatten_map items h)

  and a e =
    let attrs = attrs e in
    let x = normalize_items (children e) in
    let email s =
      if String.length s < 7 then err "invalid email: %s" s
      else match String.sub s 0 7, String.sub s 7 (String.length s - 7) with
        | "mailto:", s -> s
        | _ -> err "invalid email: %s" s
    in
    match attrs with
    | [("href", h)] -> `A (h, x)
    | [("class", "email"); ("href", s)] -> `Email (email s)
    | [("class", "uri"); ("href", h)] -> `Uri (h, x)
    | [("class", "orm:hideurl"); ("href", h)] -> `A_hide (h, x)
    | [("class", "orm:hideurl:ital"); ("href", h)] -> `A_hide_i (h, x)
    | [("data-type", "xref"); ("href", h)] -> `Xref (h, x)
    | [("data-startref", s); ("data-type", "indexterm")] -> `Index_term s
    | _ -> err "invalid a: %a" dump e

  let para t =
    let rec aux acc = function
      | []   -> List.rev acc
      | h::t -> aux (items h @ acc) t
    in
    `Para (normalize (aux [] t))

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

  let name e =
    match Soup.element e with
    | None   -> None
    | Some e -> Some (Soup.name e)

  let rec maybe_block ~depth n: block option =
    match Soup.element n with
    | None   -> None
    | Some e ->
      match Soup.name e with
      | "section" ->
        let make data_type id =
          let level, title, body = header ~depth e in
          let level = if level < depth+1 then depth+level else level in
          Some (`Section { v = {id; data_type}; level; title; body })
        in
        (match attrs e with
         | [ ("class", l); ("data-type", data_type); ("id", id); ] ->
           if String.length l >= 5 && String.sub l 0 5 = "level" then
             (* added by pandoc *)
             make data_type id
           else err "invalid section: %a" dump e
         | [ ("data-type", data_type); ("id", id) ] -> make data_type id
         | _ -> err "invalid section: %a" dump e)
      | "p" ->
        no_attrs e;
        (match children e with
         | [e] when name e = Some "link" -> Some (link Soup.(require (element e)))
         | childs -> Some (para childs))
      | "link" -> Some (link e)
      | "div" ->
        (match attrs e with
         | ["data-type", "note"] ->
           let level, title, body = header ~depth e in
           let level = if level < depth+1 then depth+level else level in
           Some (`Note {level; title; body; v=()})
         | ["class", "safarienabled"] ->
           Some (`Safari (filter_map (maybe_block ~depth) (children e)))
         | ["data-type", "warning"] ->
           let allow_break = Soup.classes e = ["allow_break"] in
           let level, title, body = header ~depth e in
           if level <> 1 then err "wrong header level for warning section";
           let w = { level=1; v=allow_break; title; body } in
           Some (`Warning w)
         | ("data-type", "table") :: ([] | ["id", _]) -> (* added by pp_table *)
           let id = Soup.id e in
           (match filter_map (maybe_block ~depth) (children e) with
            | [`Table t] -> Some (`Table { t with id })
            | x -> err "unsuported table div: %a" dump_blocks x)
         | ["class", "simplelist"] -> (* added by pp_simple_list *)
           (match filter_map (maybe_block ~depth) (children e) with
            | [`List l] -> Some (`Simple_list l)
            | x -> err "unsupported simplelist div: %a" dump_blocks x)
         | _ -> err "unsuported div: %a" dump e)
      | "ol" ->
        (match attrs e with
         | ["type", _]  (* added by pandoc *)
         | [] ->Some (`Enum (filter_map li (children e)))
         | _ -> err "invalid ol: %a" dump e)
      | "ul" ->
        (match attrs e with
         | ["class", "simplelist"] ->
           Some (`Simple_list (filter_map li (children e)))
         | [] -> Some (`List (filter_map li (children e)))
         | _  -> err "invalid ul: %a" dump e)
      | "dl" -> no_attrs e; Some (`Descr (pair_map dt dd (children e)))
      | "table" ->
        (match attrs e with
         | ["id", _] | [] ->
           let id = Soup.id e in
           Some (`Table (table id (children e)))
         | _ -> err "invalid table: %a" dump e)
      | "aside" ->
        if Soup.attribute "data-type" e = Some "sidebar" then
          let level, title, body = header ~depth e in
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

  and block ~depth e: block = match maybe_block ~depth e with
    | None   -> err "expecting a block, got %a" dump e
    | Some b -> b

  and blocks ~depth e =
    let blocks = filter_map (maybe_block ~depth) e in
    let blocks = List.filter (function `Para p -> p <> [] | _ -> true) blocks in
    match blocks with
    | []  -> `Para []
    | [b] -> b
    | bs  -> `Blocks bs

  and header ~depth e =
    let level, h, children = find_header_node (children e) in
    let title = normalize_items h in
    let body = filter_map (maybe_block ~depth:(depth + 1)) children in
    level, title, body

  and li e =
    expect "li" (fun e ->
        try `Para (normalize_items e)
        with Error _ -> blocks ~depth:0 e
      ) e

  and dt e = expect "dt" (normalize_items) e

  and dd e = expect "dd" (blocks ~depth:0) e

end

let of_string str =
  str
  |> Soup.parse
  |> Soup.children
  |> Soup.to_list
  |> filter_map Parse.(maybe_block ~depth:0)

let of_file file =
  file
  |> Soup.read_file
  |> of_string

module Check = struct

  type 'a t = {
    name : string;
    pp   : 'a Fmt.t;
    eq   : 'a -> 'a -> bool;
  }

  let check t a b =
    if not (t.eq a b) then err "%s should be equal: %a %a" t.name t.pp a t.pp b

  let v name pp eq = { name; pp; eq }

  exception False

  (* ignore whitespace when comparing strings *)
  let ignore_ws x y =
    let v s = ref 0, s, String.length s in
    let sx = v x and sy = v y in
    let rec next (n, s, len as x) =
      if !n >= len then None else match String.get s !n with
        | ' ' | '\t' | '\n' -> incr n; next x
        | c -> incr n; Some c
    in
    let rec aux () = match next sx, next sy with
      | None  , None   -> ()
      | Some a, Some b -> if a <> b then raise False else aux ()
      | _              -> raise False
    in
    try aux (); true
    with False -> false

  let string name = v name (fun ppf -> Fmt.pf ppf "%S") ignore_ws
  let int name = v name Fmt.int (=)

  let list name t =
    let mk x y =
      let rec aux a b = match a, b with
        | [], [] -> true
        | [], x | x, [] ->
          let pp = Fmt.(Dump.list t.pp) in
          err "%s: list are of different size. Common: @[%a@] Remaining: @[%a@]"
            name pp x pp x
        | a::b, c::d ->
          check t a c;
          aux b d
      in
      aux x y
    in
    v name Fmt.Dump.(list t.pp) mk

  let pair name f g =
    let aux (a, b) (c, d) =
      check f a c;
      check g b d;
      true
    in
    v name Fmt.Dump.(pair f.pp g.pp) aux

  let option name f =
    let aux a b = match a, b with
      | None , None    -> true
      | Some a, Some b -> check f a b; true
      | _ ->
        let pp = Fmt.Dump.option f.pp in
        err "%s should be equal: %a %a" f.name pp a pp b
    in
    v name Fmt.Dump.(option f.pp) aux

  let rec item () =
    let href name = pair "a" (string "href") (list "txt" (item ())) in
    let rec aux (a:item) (b:item) = match a, b with
      | `Em a, `Em b -> check (string "em") a b; true
      | `Keep_together a, `Keep_together b ->
        check (string "keep-together") a b; true
      | `Strong a, `Strong b -> check (list "strong" (item ())) a b; true
      | `Idx a, `Idx b ->
        check (pair "idx" (option "id" (string "id")) (string "idx")) a b; true
      | `Code a, `Code b -> check (string "code") a b; true
      | `A a, `A b -> check (href "a") a b; true
      | `Text a, `Text b -> check (string "text") a b; true
      | `Email a, `Email b -> check (string "email") a b; true
      | `Xref a, `Xref b -> check (href "xref") a b; true
      | `Uri a, `Uri b -> check (href "uri") a b; true
      | `A_hide a, `A_hide b -> check (href "a:hide") a b; true
      | `A_hide_i a, `A_hide_i b -> check (href "a:hide:ital") a b; true
      | `Index_term a, `Index_term b -> check (string "indexterm") a b; true
      | `Hyperlink a, `Hyperlink b -> check (string "hyperlink") a b; true
      | `Filename a, `Filename b -> check (string "filename") a b; true
      | (`Em _ | `Keep_together _ | `Strong _ | `Idx _ | `Code _ | `Index_term _
        | `A _ | `Text _ | `Email _ | `Xref _ | `Uri _ | `A_hide _ | `Hyperlink _
        | `A_hide_i _ | `Filename _), _ -> false
    in
    v "item" dump_item aux

  let item = item ()
  let phrase = list "phrase" item

  let rec block () =
    let aux (a:block) (b:block) = match a, b with
      | `Figure a, `Figure b   -> check (string "figure") a b; true
      | `Descr a, `Descr b     ->
        check (list "descr" (pair "descr" phrase (block ()))) a b; true
      | `List a, `List b       -> check (list "list" (block ())) a b; true
      | `Table a, `Table b     -> check table a b; true
      | `Section a, `Section b -> check (part "section" section) a b; true
      | `Note a, `Note b       -> check (part "note" unit) a b; true
      | `Safari a, `Safari b   -> check (list "safari" (block ())) a b; true
      | `Sidebar a, `Sidebar b ->
        check (pair "sidebar" phrase (blocks ())) a b;
        true
      | `Blocks a, `Blocks b   -> check (blocks ()) a b; true
      | `Enum a, `Enum b       -> check (list "enum" (block ())) a b; true
      | `Para a, `Para b       -> check (list "para" item) a b; true
      | `Link a, `Link b       -> check link a b; true
      | `Warning a, `Warning b -> check (part "warning" bool) a b; true
      | `Simple_list a, `Simple_list b ->
        check (list "simplelist" (block ())) a b; true
      | (`Figure _ | `Descr _ | `List _ | `Table _ | `Section _ | `Note _
        | `Safari _ | `Sidebar _ | `Blocks _ | `Enum _ | `Para _ | `Link _
        | `Warning _ | `Simple_list _), _ -> false
    in
    v "block" dump_block aux

  and unit = v "unit" dump_unit (=)
  and link = v "link" pp_link (=)
  and blocks () = list "blocks" (block ())
  and bool = v "allow_break" Fmt.bool (=)

  and section =
    let aux (a:section) (b:section) =
      check (string "id") a.id b.id;
      check (string "data_type") a.data_type b.data_type;
      true
    in
    v "section" dump_section aux

  and part: type a. string -> a t -> a part t = fun name ty ->
    let aux (a:a part) (b:a part) =
      check phrase a.title b.title;
      let title = Fmt.to_to_string pp_items a.title in
      check (int ("level of " ^ title)) a.level b.level;
      check (list ("body of " ^ title) (block ())) a.body b.body;
      check ty a.v b.v;
      true
    in
    v name (dump_part ty.pp) aux

  and table =
    let aux (a:table) (b:table) =
      check (option "id" (string "id")) a.id b.id;
      check (option "caption" (list "caption" item)) a.caption b.caption;
      check (list "headers" (list "headers" item)) a.headers b.headers;
      check (list "body" (list "body" (list "body" item))) a.body b.body;
      true
    in
    v "table" dump_table aux

  let blocks = blocks ()

end

let check a b =
  let a = of_file a in
  let b = of_file b in
  Check.(check blocks) a b

let () =
  let input =
    if Array.length Sys.argv = 2 then Sys.argv.(1)
    else (
      Fmt.epr "usage: html2md <file>";
      exit 1
    ) in
  let output = (Filename.remove_extension input) ^ ".md" in
  let html_output = (Filename.remove_extension input) ^ "-new.html" in
  try
    let oc = open_out output in
    let ppf = Format.formatter_of_out_channel oc in
    let blocks = of_file input in
    List.iter (Fmt.pf ppf "%a\n" pp_block) blocks;
    Fmt.pf ppf "%!";
    close_out oc;
    let _ =
      Fmt.kstrf Sys.command
        "pandoc --section-divs -f markdown-smart %s -o %s"
        output html_output
    in
    check input html_output
  with Error e ->
    Fmt.epr "%s: %s\n%!" input e;
    exit 1
