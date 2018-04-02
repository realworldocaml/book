exception Error of string

let err fmt = Fmt.kstrf (fun str -> raise (Error str)) fmt

type idx = {
  id     : string option;
  sortas : string option;
  seealso: string option;
  v      : string;
}

type 'a a = {
  href: string;
  v   : 'a;
}

type 'a xref = {
  href : string;
  style: string option;
  v    : 'a;
}

type item = [
  | `Text   of string
  | `Strong of item list
  | `Em     of item list
  | `Code   of string
  | `A      of item list a
  | `Idx    of idx
  | `Keep_together of string
  | `Index_term of string
  | `Xref of item list xref
  | `Filename of string
  (* only in 00-prologue *)
  | `Uri of item list a
  | `A_hide of item list a
  | `A_hide_i of item list a
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

type link = {
  rel : string;
  href: string;
  part: string option;
}

type block = [
  | `Empty
  | `Link of link
  | `Para of item list
  | `Note of bool part
  | `Warning of bool part
  | `List of block list
  | `Enum of block list
  | `Descr of (phrase * phrase) list
  | `Section of section part
  | `Blocks of block list
  | `Table of table
  | `Sidebar of item list * block
  | `Figure of string
  | `Safari of block
  (* only in 00-prologue *)
  | `Simple_list of block list
  (* only in chapter 22 and 23 *)
  | `Tip of bool part
  | `Caution of bool part
]

and 'a part = {
  level: int;
  title: item list;
  body : block;
  v    : 'a;
}

and section = {
  id       : string;
  data_type: string;
}

let dump ppf n = Fmt.of_to_string Soup.to_string ppf n

let is_white = function ' ' | '\t' .. '\r'  -> true | _ -> false

let trim_left s =
  let i = ref 0 in
  let len = String.length s in
  while !i < len && is_white s.[!i] do incr i done;
  if !i = 0 then s else String.sub s !i (len - !i)

let trim_right s =
  let len = String.length s in
  let i = ref len in
  while !i > 0 && is_white s.[!i-1] do decr i done;
  if !i = len then s else String.sub s 0 !i

let map_text f (x:item) = match x with
  | `Text s -> `Text (f s)
  | _ -> x

let trim_items = function
  | []  -> []
  | [i] -> [map_text String.trim i]
  | h::(_::_ as t) ->
    let t, last = match List.rev t with
      | []   -> assert false
      | h::t -> List.rev t, h
    in
    map_text trim_left h :: t @ [map_text trim_right last]

(* Like [Fmt.words] but without triming *)
let pp_words ppf s =
  let last = ref 'x' in
  String.iter (fun c ->
      if is_white c && is_white !last then ()
      else if is_white c then Format.pp_print_space ppf ()
      else Format.pp_print_char ppf c;
      last := c
    ) s

let pp_html_words ppf s =
  let is_white = function ' ' | '\t' .. '\r'  -> true | _ -> false in
  let is_start = (=)'<' and is_end = (=) '>' in
  let last = ref 'x' in
  let needs_escaping = ref false in
  String.iter (fun c ->
      if is_white c && is_white !last then ()
      else if is_white c then (
        needs_escaping := false; (* <foo > is parser properly by pandoc *)
        Format.pp_print_space ppf ()
      ) else (
        if is_start c then needs_escaping := true;
        if is_end c && !needs_escaping then (
          needs_escaping := false;
          Format.pp_print_char ppf '\\';
        );
        Format.pp_print_char ppf c;
      );
      last := c
    ) s

let list_v pp = Fmt.vbox Fmt.(list ~sep:(unit "@,@,") pp)
let list_h pp = Fmt.box Fmt.(list ~sep:(unit "") pp)

let listi_v pp ppf l =
  let l = List.mapi (fun i x -> i, x) l in
  list_v pp ppf l

let pp_one_line pp ppf items =
  let x = Fmt.to_to_string pp items in
  (* cf. pipe_tables: The cells of pipe tables cannot contain block
     elements like paragraphs and lists, and cannot span multiple
     lines. Also cf multi-line headers which are not supported in
     markdown. *)
  let x = String.mapi (fun i -> function
      | '\n' -> ' '
      | c    -> c
        ) x in
  Fmt.string ppf x

let pp_level ppf l =
  let s = String.make l '#' in
  Fmt.string ppf s

let pp_link ppf {rel; href; part} =
  let pp_part ppf s = Fmt.pf ppf "part=@[\"%a\"@] " (pp_one_line pp_words) s in
  Fmt.pf ppf "<link rel=\"%a\" href=\"%a\" %a/>"
    pp_words rel pp_words href Fmt.(option pp_part) part

let pp_index_term ppf s =
  Fmt.pf ppf "<a data-type=\"indexterm\" data-startref=\"%s\">&nbsp;</a>" s

let pp_idx ppf {id; sortas; v} = match id, sortas with
  | None  , None   -> Fmt.pf ppf "[%a]{.idx}" pp_words v
  | Some i, None   -> Fmt.pf ppf "[%a]{.idx #%s}" pp_words v i
  | None  , Some s -> Fmt.pf ppf "[%a]{.idx data-primary-sortas=%s}" pp_words v s
  | Some i, Some s ->
    Fmt.pf ppf "[%a]{.idx #%s data-primary-sortas=%s}" pp_words v i s

let pp_code ppf s =
  let string_mem c =
    let r = ref false in
    String.iter (fun k -> if k=c then r := true) s;
    !r
  in
  if string_mem '`' then Fmt.pf ppf "` %a`" pp_words s
  else pp_words ppf s

let rec pp_item ppf (i:item) = match i with
  | `Em e     -> Fmt.pf ppf "@[*%a*@]" pp_items e
  | `Strong s -> Fmt.pf ppf "@[**%a**@]" pp_items s
  | `Idx i    -> pp_idx ppf i
  | `Code c   -> Fmt.pf ppf "@[`%a`@]" pp_code c
  | `A href   -> pp_href ppf href
  | `Xref x   -> pp_xref ppf x
  | `Text s   -> pp_html_words ppf s
  | `Email s  -> Fmt.pf ppf "@[[_%s_](mailto:%s){.email}@]" s s
  | `Keep_together s -> Fmt.pf ppf "<span class=\"keep-together\">%s</span>" s
  | `Index_term s    -> pp_index_term ppf s
  | `Filename f  -> Fmt.pf ppf "<em class=\"filename\">%a</em>" pp_words f
  | `A_hide a    -> Fmt.pf ppf "@[[%a](%s){.orm:hideurl}@]" pp_items a.v a.href
  | `A_hide_i a  -> Fmt.pf ppf "@[[%a](%s){.orm:hideurl:ital}@]" pp_items a.v a.href
  | `Uri a       -> Fmt.pf ppf  "@[[%a](%s)@]" pp_items a.v a.href
  | `Hyperlink s -> Fmt.pf ppf "<em class=\"hyperlink\">%a</em>" pp_words s

and pp_items ppf l = list_h pp_item ppf l

and pp_href ppf a = Fmt.pf ppf "@[[%a](%a)@]" pp_items a.v pp_words a.href

and pp_xref ppf a =
  let pp_style ppf = function
    | None   -> ()
    | Some s -> Fmt.pf ppf " data-xrefstyle=\"%a\"" pp_words s
  in
  Fmt.pf ppf "@[[%a](%a){data-type=xref%a}@]"
    pp_items a.v pp_words a.href pp_style a.style

let pp_table ppf t =
  let aux ppf () =
    let item_lengh e = String.length (Fmt.to_to_string pp_item e) in
    let pp_one_line = pp_one_line pp_items in
    let len t = List.fold_left (fun acc x -> 1 + item_lengh x + acc) 0 t in
    Fmt.(list ~sep:(unit " | ") pp_one_line) ppf t.headers;
    Fmt.pf ppf "@,";
    Fmt.(list ~sep:(unit "-|-") string) ppf
      (List.map (fun s -> String.make (len s - 1) '-') t.headers);
    Fmt.pf ppf "@,";
    Fmt.(list ~sep:(unit "@,")
           Fmt.(list ~sep:(unit " | ") pp_one_line)
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
  | `Empty        -> ()
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
  | `Safari bs    -> Fmt.pf ppf "::: safarienabled@,%a@,:::" pp_block bs
  | `Warning w    -> pp_warning ppf w
  | `Caution c    -> pp_caution ppf c
  | `Tip t        -> pp_tip ppf t
  | `Simple_list l -> pp_simple_list ppf l

and pp_break ppf b = if b then Fmt.string ppf ".allow_break " else ()

and pp_part name ppf n =
  Fmt.pf ppf "@[::: {%adata-type=%s}@]@,@[%a %a@]@,@,@[%a@]@,:::@,"
    pp_break n.v name pp_level n.level pp_items n.title pp_block n.body

and pp_note ppf n = pp_part "note" ppf n
and pp_warning ppf w = pp_part "warning" ppf w
and pp_tip ppf t = pp_part "tip" ppf t
and pp_caution ppf c = pp_part "caution" ppf c

and pp_section ppf s =
  Fmt.pf ppf "@[%a %a {#%s data-type=%S}@]@.@.%a@."
    pp_level s.level (pp_one_line pp_items)
    s.title s.v.id s.v.data_type pp_block s.body

and pp_para ppf p = list_h pp_item ppf (trim_items p)
and pp_enum ppf s = listi_v pp_enum_descr ppf s
and pp_enum_descr ppf (i, s) = Fmt.pf ppf "@[<2>%d. %a@]" (i+1) pp_block s

and pp_blocks ppf bs = Fmt.pf ppf "@[<2>%a@]" (list_v pp_block) bs

and pp_list ppf t = list_v pp_list_elt ppf t
and pp_list_elt ppf t = Fmt.pf ppf "@[<2>- %a@]" pp_block t

and pp_simple_list ppf t = Fmt.pf ppf "::: {.simplelist}@,%a@,:::@," pp_list t

and pp_descr ppf t = list_v pp_descr_elt ppf t
and pp_descr_elt ppf (title, body) =
  Fmt.pf ppf "@[<v>@[<h>%a@]@,@[<h-2>: @[%a@]@]@]"
    pp_items title pp_items (trim_items body)

and pp_sidebar ppf (title, body) =
  (* FIXME: pandoc has a bug here when using ### headers and
     --section-div: it doesn't close the section at the right place *)
  Fmt.pf ppf "<aside data-type=\"sidebar\">@,<h5>%a</h5>@,@,%a@,@,</aside>"
    pp_items title pp_block body

let dump_idx ppf {id; sortas; v} =
  Fmt.pf ppf "@[{id=%a;@ sortas=%a;@ v=%S}@]"
    Fmt.(Dump.option string) id
    Fmt.(Dump.option string) sortas
    v

let rec dump_item ppf (i:item) = match i with
  | `Em e     -> Fmt.pf ppf "Em %a" dump_items e
  | `Strong s -> Fmt.pf ppf "Strong %a" dump_items s
  | `Idx s    -> Fmt.pf ppf "Idx %a" dump_idx s
  | `Code c   -> Fmt.pf ppf "Code %S" c
  | `A a      -> Fmt.pf ppf "A %a" (dump_a dump_items) a
  | `Text s   -> Fmt.pf ppf "Text %S" s
  | `Email s  -> Fmt.pf ppf "Email %S" s
  | `Filename f      -> Fmt.pf ppf "Filename %S" f
  | `Keep_together s -> Fmt.pf ppf "Keep_together %S" s
  | `Index_term s    -> Fmt.pf ppf "Index_term %S" s
  | `Xref a          -> Fmt.pf ppf "Xref %a" (dump_xref dump_items) a
  | `A_hide a        -> Fmt.pf ppf "A_hide %a" (dump_a dump_items) a
  | `A_hide_i a      -> Fmt.pf ppf "A_hide_i %a" (dump_a dump_items) a
  | `Uri a           -> Fmt.pf ppf "Uri %a" (dump_a dump_items) a
  | `Hyperlink s     -> Fmt.pf ppf "Hyperlink %S" s

and dump_a pp ppf t = Fmt.pf ppf "@[<2>{href=%S;@ %a}@]" t.href pp t.v

and dump_xref pp ppf t =
  Fmt.pf ppf "@[<2>{href=%S;@ style=%a;@ %a}@]"
    t.href Fmt.(Dump.option string) t.style pp t.v

and dump_items ppf t = Fmt.Dump.list dump_item ppf t

and dump_block ppf (t:block) = match t with
  | `Empty     -> Fmt.string ppf "Empty"
  | `Blocks l  -> Fmt.pf ppf "@[<2>Block %a@]" dump_blocks l
  | `Link l    -> Fmt.pf ppf "@[<2>Link %a@]" dump_link l
  | `Figure f  -> Fmt.pf ppf "@[<2>Figure %S@]" f
  | `Para is   -> Fmt.pf ppf "@[<2>Para@ (%a)@]" dump_items is
  | `List bs   -> Fmt.pf ppf "@[<2>List@ (%a)@]" dump_blocks bs
  | `Descr d   -> Fmt.pf ppf "@[<2>Descr@ (%a)@]" dump_descr d
  | `Enum bs   -> Fmt.pf ppf "@[<2>Enum@ (%a)@]" dump_blocks bs
  | `Note n    -> Fmt.pf ppf "@[<2>Note@ %a]" (dump_part Fmt.bool) n
  | `Section s -> Fmt.pf ppf "@[<2>Section@ (%a)@]" (dump_part dump_section) s
  | `Table t   -> dump_table ppf t
  | `Sidebar s -> pp_sidebar ppf s
  | `Safari s  -> Fmt.pf ppf "@[<2>Safari@ (%a)@]" dump_block s
  | `Warning w -> Fmt.pf ppf "@[<2>Warning@ %a@]" (dump_part Fmt.bool) w
  | `Tip t     -> Fmt.pf ppf "@[<2>Tip@ %a@]" (dump_part Fmt.bool) t
  | `Caution c     -> Fmt.pf ppf "@[<2>Caution@ %a@]" (dump_part Fmt.bool) c
  | `Simple_list s -> Fmt.pf ppf "@[<2>Simple_list (%a)@]" dump_blocks s

and dump_part: type a. a Fmt.t -> a part Fmt.t = fun dump_v ppf t ->
  Fmt.pf ppf "@[<2>{level=%d;@ title=%a;@ body=%a;@ %a}@]"
    t.level pp_items t.title dump_block t.body dump_v t.v

and dump_link ppf {rel; href; part} =
  Fmt.pf ppf "@[<h-2>{rel=@[%a@];@ href=@[%a@];@ part=@[<h>%a@]}@]"
    pp_words rel pp_words href Fmt.(Dump.option pp_words) part

and dump_descr ppf d = Fmt.Dump.(list (pair dump_items dump_items)) ppf d
and dump_blocks ppf t = Fmt.Dump.list dump_block ppf t
and dump_section ppf s =
  Fmt.pf ppf "@[id:@ %S;@ data_type:@ %S@]" s.id s.data_type

and dump_table ppf (t:table) =
  Fmt.pf ppf "@[<2>{id: %a; caption: %a; headers: %a; body: %a}@]"
    Fmt.(Dump.option string) t.id
    Fmt.(Dump.option dump_items) t.caption
    Fmt.(Dump.list dump_items) t.headers
    Fmt.(Dump.list (Dump.list dump_items)) t.body

let int_of_level e = function
  | "h1" -> 1
  | "h2" -> 2
  | "h3" -> 3
  | "h4" -> 4
  | "h5" -> 5
  | "h6" -> 6
  | _    -> Fmt.invalid_arg "invalid level: %a" dump e

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

let rec find_first_node = function
  | []   -> None
  | h::t ->
    match Soup.element h with
    | None   -> find_first_node t
    | Some e -> Some (e, h, t)

let find_header_node e =
  match find_first_node (children e) with
  | None           -> err "no header node"
  | Some (e, h, t) ->
    let i = int_of_level e (Soup.name e) in
    i, children h, t

let one_child e = match children e with
  | []  -> err "no child"
  | [e] -> e
  | _   -> err "too many children: %a" dump e

module Parse = struct

  let text s = `Text s
  let strong s = `Strong s
  let em s = `Em s
  let idx ?id ?sortas ?seealso v = `Idx {id; sortas; v; seealso}
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

  let link e =
    match attrs e with
    | ["href", href; "rel", rel] -> {rel; href; part=None}
    | ["href", href; "part", p; "rel", rel] -> {rel; href; part=Some p}
    | _ -> err "invalid link: %a" dump e

  let no_attrs e =
    match Soup.element e with
    | None   -> ()
    | Some e ->
      let attrs = attrs e in
      if attrs <> [] then
        err "item has attributes: %a"
          Fmt.(Dump.list Dump.(pair string string)) attrs

  let txt h f =
    Soup.texts h
    |> String.concat ""
    |> function "" -> [] | s -> [f s]

  let idx h e =
    let attrs = List.filter (fun (x, _) -> x <> "class") (attrs e) in
    match attrs with
    | ["id", id]                 -> txt h (fun x -> idx ~id x)
    | ["data-seealso", s]        -> txt h (fun x -> idx ~seealso:s x)
    | ["data-primary-sortas", s] -> txt h (fun x -> idx ~sortas:s x)
    | ["data-primary-sortas", s; "id", id] ->
      txt h (fun x -> idx ~id ~sortas:s x)
    | []        -> txt h (fun x -> idx x)
    | _         -> err "invalid idx: %a" dump e

  let rec items h: item list =
    let txt = txt h and idx = idx h in
    match Soup.element h with
    | None   -> no_attrs h; txt text
    | Some e ->
      match Soup.name e with
      | "strong" -> no_attrs h; [`Strong (normalize_items (children e))]
      | "em" when attrs e = ["class", "hyperlink"] -> txt hyperlink
      | "em" when attrs e = ["class", "filename"]  -> txt filename
      | "em"     -> no_attrs h; [`Em (normalize_items (children e))]
      | "code"   -> no_attrs h; txt code
      | "a"      -> [a e]
      | "idx"    -> idx e
      | "span"   ->
        (match Soup.attribute "class" e with
         | Some "command"       ->
           (match items (one_child e) with
            | [`Em [`Text x]] ->
              (* <span class="command"><em>foo</em></span> == <code>foo</code> *)
              [`Code x]
            | _ -> err "class=\"command\"")
         (* XXX: not sure what it is used for *)
         | Some "keep-together" -> txt keep_together
         | Some "idx" -> idx e
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
    let a href v = { href; v } in
    let xref href style v = { href; style; v } in
    match attrs with
    | [("href", h)] -> `A (a h x)
    | [("class", "email"); ("href", s)] -> `Email (email s)
    | [("class", "uri"); ("href", h)] -> `Uri (a h x)
    | [("class", "orm:hideurl"); ("href", h)] -> `A_hide (a h x)
    | [("class", "orm:hideurl:ital"); ("href", h)] -> `A_hide_i (a h x)
    | [("data-type", "xref"); ("href", h)] -> `Xref (xref h None x)
    | [("data-type", "xref"); ("data-xrefstyle", s); ("href", h)] ->
      `Xref (xref h (Some s) x)
    | [("data-startref", s); ("data-type", "indexterm")] -> `Index_term s
    | _ -> err "invalid a: %a" dump e

  let para t =
    let rec aux acc = function
      | []   -> List.rev acc
      | h::t -> aux (items h @ acc) t
    in
    match normalize (aux [] t) with
    | [] -> `Empty
    | is -> `Para is

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
      try Some (normalize (find "caption" (flatten_map items) childs))
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
        (match attrs e with
         | [ ("class", l); ("data-type", data_type); ("id", id); ] ->
           if String.length l >= 5 && String.sub l 0 5 = "level" then
             (* 'class' is added by pandoc *)
             Some (`Section (section ~depth ~data_type ~id e))
           else err "invalid section: %a" dump e
         | [ ("data-type", data_type); ("id", id) ] ->
           Some (`Section (section ~depth ~data_type ~id e))
         | _ -> err "invalid section: %a" dump e)
      | "p" ->
        no_attrs e;
        (* <p><link ... .></p> is added by pandoc. <link> should not
           appear inside <p> so sometimes lambdasoup creates an empty
           <p>. *)
        (match children e with
         | []     -> None
         | childs ->
           match find_first_node childs with
           | None          -> Some (para childs)
           | Some (h, _, t) ->
             match Soup.name h, t with
             | "link", [] -> Some (`Link (link h))
             | _          -> Some (para childs))
      | "link" -> Some (`Link (link e))
      | "div" ->
        (match attrs e with
         | ("data-type", "table") :: ([] | ["id", _]) -> (* added by pp_table *)
           let id = Soup.id e in
           (match blocks ~depth (children e) with
            | `Table t -> Some (`Table { t with id })
            | x -> err "unsuported table div: %a" dump_block x)
         | [("class", "allow_break"); ("data-type", x)]
         | ["data-type", x] ->
           let f p = match x with
             | "note"    -> Some (`Note p)
             | "warning" -> Some (`Warning p)
             | "tip"     -> Some (`Tip p)
             | "caution" -> Some (`Caution p)
             | x -> err "invalid part: %s" x
           in
           let v = Soup.classes e = ["allow_break"] in
           let level, title, body = header ~depth e in
           let level = if level < depth then depth else level in
           f {level; title; body; v}
         | ["class", "safarienabled"] ->
           Some (`Safari (blocks ~depth (children e)))
         | ["class", "simplelist"] -> (* added by pp_simple_list *)
           (match blocks ~depth (children e) with
            | `List l -> Some (`Simple_list l)
            | x -> err "unsupported simplelist div: %a" dump_block x)
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
        (match attrs e with
         | ["data-type", "sidebar"] ->
           let make e =
             let level, title, body = header ~depth e in
             if level <> 5 then err "wrong header level in sidebar (%d)" level;
             Some (`Sidebar (title, body))
           in
           (match find_first_node (children e) with
            | None             -> err "wrong sidebar: %a" dump e
            | Some (hd, n', _) ->
              (match Soup.name hd with
               | "section" (* section is added by pandoc *) ->
                 (match attrs hd with
                  | [ ("class", l); ("id", id); ] ->
                    if String.length l >= 5 && String.sub l 0 5 = "level" then
                      let s = section ~depth ~data_type:"foo" ~id hd in
                      Some (`Sidebar (s.title, s.body))
                    else
                      err "invalid section/aside: %a" dump e
                  | _ -> err "invalid aside (1) %a" dump e)
               | _ -> make e))
         | _ -> err "invalid aside (2) %a" dump e)
      | "figure" ->
        (match attrs e with
         | ["style", "float: 0"] ->
           let img e = match Soup.element e with
             | None   -> None
             | Some e -> match Soup.name e with
               | "img" -> Soup.attribute "src" e
               | s     -> err "figure: %s" s
           in
           (match filter_map img (children e) with
            | [x] -> Some (`Figure x)
            |  _  -> err "figure")
         | _ -> err "invalid figure")
      | s -> err "TODO block: %s %a" s dump e

  and block ~depth e: block = match maybe_block ~depth e with
    | None   -> err "expecting a block, got %a" dump e
    | Some b -> b

  and blocks ~depth e =
    let bs = filter_map (maybe_block ~depth) e in
    let bs = List.filter ((<>) `Empty) bs in
    match bs with
    | []  -> `Empty
    | [b] -> b
    | bs  -> `Blocks bs

  and header ~depth e =
    let level, h, children = find_header_node e in
    let title = normalize_items h in
    let body = blocks ~depth:(depth + 1) children in
    level, title, body

  and section ~depth ~data_type ~id e =
    let level, title, body = header ~depth e in
    let level = if level < depth then depth else level in
    { v = {id; data_type}; level; title; body }

  and li e =
    expect "li" (fun e ->
        try para e
        with Error _ -> blocks ~depth:1 e
      ) e

  and dt e = expect "dt" (normalize_items) e

  and dd e = expect "dd" (normalize_items) e

end

let of_string str =
  str
  |> Soup.parse
  |> Soup.children
  |> Soup.to_list
  |> filter_map Parse.(maybe_block ~depth:1)

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
        | [], r | r, [] ->
          let pp = Fmt.(Dump.list t.pp) in
          err "%s: list are of different size (%d and %d). \
               Common:@,@[%a@]@.@.Remaining:@,@[%a@]"
            name (List.length x) (List.length y)
            pp x pp r
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

  let idx =
    let aux (a:idx) (b:idx) =
      check (option "id" (string "id")) a.id b.id;
      check (option "sortas" (string "sortas")) a.sortas b.sortas;
      check (string "idx") a.v b.v;
      true
    in
    v "idx" dump_idx aux

  let rec item name =
    let rec aux (a:item) (b:item) = match a, b with
      | `Em a, `Em b -> check (items "name") a b; true
      | `Keep_together a, `Keep_together b ->
        check (string "keep-together") a b; true
      | `Strong a, `Strong b -> check (items "strong") a b; true
      | `Idx a, `Idx b -> check idx a b; true
      | `Code a, `Code b -> check (string "code") a b; true
      | `A a, `A b -> check (href "a") a b; true
      | `Text a, `Text b -> check (string "text") a b; true
      | `Email a, `Email b -> check (string "email") a b; true
      | `Xref a, `Xref b -> check (xref "xref") a b; true
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

  and href name =
    let aux (x: item list a) (y: item list a) =
      check (string "href") x.href y.href;
      check (list name (item name)) x.v y.v;
      true
    in
    v name (dump_a dump_items) aux

  and xref name =
    let aux (x: item list xref) (y: item list xref) =
      check (string "href") x.href y.href;
      check (option "style" (string "style")) x.style y.style;
      check (list name (item name)) x.v y.v;
      true
    in
    v name (dump_xref dump_items) aux

  and items name = list name (item name)

  let item = item "item"
  let phrase = list "phrase" item

  let rec block name: block t =
    let aux (a:block) (b:block) = match a, b with
      | `Empty, `Empty         -> true
      | `Figure a, `Figure b   -> check (string "figure") a b; true
      | `Descr a, `Descr b     ->
        check (list "descr" (pair "descr" phrase phrase)) a b; true
      | `List a, `List b       -> check (list "list" (block "list")) a b; true
      | `Table a, `Table b     -> check table a b; true
      | `Section a, `Section b -> check (part "section" section) a b; true
      | `Note a, `Note b       -> check (part "note" bool) a b; true
      | `Safari a, `Safari b   -> check (block "safari") a b; true
      | `Sidebar a, `Sidebar b ->
        check (pair "sidebar" phrase (block "sidebar")) a b;
        true
      | `Blocks a, `Blocks b   -> check (blocks ()) a b; true
      | `Enum a, `Enum b       -> check (list "enum" (block "enum")) a b; true
      | `Para a, `Para b       -> check (list "para" item) a b; true
      | `Link a, `Link b       -> check link a b; true
      | `Warning a, `Warning b -> check (part "warning" bool) a b; true
      | `Caution a, `Caution b -> check (part "caution" bool) a b; true
      | `Tip a, `Tip b         -> check (part "tip" bool) a b; true
      | `Simple_list a, `Simple_list b ->
        check (list "simplelist" (block "simplelist")) a b; true
      | (`Figure _ | `Descr _ | `List _ | `Table _ | `Section _ | `Note _
        | `Safari _ | `Sidebar _ | `Blocks _ | `Enum _ | `Para _ | `Link _
        | `Warning _ | `Simple_list _ | `Empty | `Tip _ | `Caution _), _ -> false
    in
    v name dump_block aux

  and link = v "link" dump_link (=)
  and blocks (): block list t = list "blocks" (block "blocks")

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
      check (block name) a.body b.body;
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
  (*  Fmt.pr "Parsing %s\n%!" a; *)
  let a = of_file a in
  (*  Fmt.pr "Parsing %s\n%!" b; *)
  let b = of_file b in
  Check.(check blocks) a b

let run input =
  let output = (Filename.remove_extension input) ^ ".md" in
  Fmt.pr "Generating %s\n%!" output;
  try
    let oc = open_out output in
    let ppf = Format.formatter_of_out_channel oc in
    let blocks = of_file input in
    List.iter (Fmt.pf ppf "%a\n" pp_block) blocks;
    Fmt.pf ppf "%!";
    close_out oc;
    let html_output = (Filename.remove_extension input) ^ ".2.html" in
    let _ =
      Fmt.kstrf Sys.command
        "pandoc --section-divs -f markdown-smart-auto_identifiers -t html5 %s -o %s"
        output html_output
    in
    check input html_output
  with Error e ->
    Fmt.epr "%s: %s\n%!" input e;
    exit 1

let starts_with_digit b =
  try Scanf.sscanf b "%d-" (fun _ -> ()); true
  with _ -> false

let () =
  let input =
    if Array.length Sys.argv = 2 then Sys.argv.(1)
    else (
      Fmt.epr "usage: html2md <file>";
      exit 1
    ) in
  match input with
  | "--all" | "-a" ->
    let dir = Unix.opendir "book" in
    let files = ref [] in
    let (/) = Filename.concat in
    let rec loop () =
      try
        let file = Unix.readdir dir in
        if Filename.extension file = ".html"
        && Filename.(extension (remove_extension file)) = ""
        && starts_with_digit file then
          files := ("book" / file) :: !files;
        loop ()
      with End_of_file ->
        List.sort String.compare !files
    in
    List.iter run (loop ())
  | _ -> run input
