(* This file is part of Lambda Soup, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/lambdasoup. *)

module String =
struct
  include String

  let trim =
    let whitespace = " \t\n\r" in
    fun s ->
      let rec measure_prefix index =
        if index = String.length s then index
        else
          if String.contains whitespace s.[index] then
            measure_prefix (index + 1)
          else index
      in
      let prefix_length = measure_prefix 0 in
      let s = String.sub s prefix_length (String.length s - prefix_length) in

      let rec measure_suffix rindex =
        if rindex = String.length s then rindex
        else
          if String.contains whitespace s.[String.length s - rindex - 1] then
            measure_suffix (rindex + 1)
          else rindex
      in
      let suffix_length = measure_suffix 0 in
      String.sub s 0 (String.length s - suffix_length)

  (* Convert multiple deprecation warnings into one warning. This function can
     be removed from the module if/when Lambda Soup supports only
     OCaml >= 4.03. *)
  let lowercase_ascii = lowercase [@ocaml.warning "-3"]
end

module Char =
struct
  include Char

  (* See comment by String.lowercase_ascii above. *)
  let lowercase_ascii = lowercase [@ocaml.warning "-3"]
end

type element = unit
type general = unit
type soup = unit

type element_values =
  {mutable name       : string;
   mutable attributes : (string * string) list;
   mutable children   : general node list}

and document_values =
  {mutable roots : general node list}

and 'a node =
  {mutable self   : 'b. 'b node option;
   mutable parent : general node option;
   values         : [ `Element of element_values
                    | `Text of string
                    | `Document of document_values ]}

let require_internal message = function
  | None -> failwith message
  | Some v -> v

let require = function
  | None -> failwith "require: argument is None"
  | Some v -> v

let forget_type : (_ node) -> (_ node) =
  fun n ->
    require_internal
      "Soup._forget_type: internal error: node's self reference not set"
      n.self

let coerce node = forget_type node

let create_element name attributes children =
  let values = {name; attributes; children} in
  let node = {self = None; parent = None; values = `Element values} in
  node.self <- Some node;
  children |> List.iter (fun child -> child.parent <- Some node);
  node

let create_text text =
  let node = {self = None; parent = None; values = `Text text} in
  node.self <- Some node;
  node

let create_document roots =
  let node = {self = None; parent = None; values = `Document {roots}} in
  node.self <- Some node;
  roots |> List.iter (fun root -> root.parent <- Some node);
  node

let create_soup () = create_document []

let from_signals signals =
  signals
  |> (fun s -> Markup.trees
    ~text:(fun ss -> create_text (String.concat "" ss))
    ~element:(fun name attributes children ->
      let attributes =
        attributes |> List.map (fun ((_, n), v) -> n, v) in
      create_element (snd name) attributes children)
    s)
  |> Markup.to_list
  |> create_document

let parse text =
  text
  |> Markup.string
  |> (fun s -> Markup.parse_html s)
  |> Markup.signals
  |> from_signals

let is_document node =
  match node.values with
  | `Element _ -> false
  | `Text _ -> false
  | `Document _ -> true

let is_element node =
  match node.values with
  | `Element _ -> true
  | `Text _ -> false
  | `Document _ -> false

let element node =
  if is_element node then Some (forget_type node) else None

type 'a stop = {throw : 'b. 'a -> 'b}

exception Stop of int64

let generate_id =
  let next = ref 0L in
  fun () ->
    let current = !next in
    next := Int64.succ current;
    current

let with_stop f =
  let result = ref None in
  let id = generate_id () in
  let stop = {throw = fun v -> result := Some v; raise_notrace (Stop id)} in
  try f stop
  with Stop id' when id' = id ->
    match !result with
    | None ->
      failwith "Soup.with_stop: internal error: !result = None" [@coverage off]
    | Some v -> v

let name = function
  | {values = `Element {name; _}; _} -> String.lowercase_ascii name
  | _ -> failwith "Soup.name: internal error: not an element" [@coverage off]

let fold_attributes f init = function
  | {values = `Element {attributes; _}; _} ->
    attributes |> List.fold_left (fun v (name, value) -> f v name value) init
  | _ ->
    failwith "Soup.fold_attributes: internal error: not an element"
      [@coverage off]

let attribute name node =
  with_stop (fun stop ->
    node |> fold_attributes (fun _ name' value ->
      if name' = name then stop.throw (Some value) else None)
      None)

let has_attribute name node =
  match attribute name node with
  | None -> false
  | Some _ -> true

let split_attribute s =
  let rec loop index vs =
    if index = String.length s then List.rev vs
    else
      let maybe_index' =
        try Some (String.index_from s index ' ')
        with Not_found -> None
      in
      match maybe_index' with
      | None -> (String.sub s index (String.length s - index))::vs |> List.rev
      | Some index' when index' = index -> loop (index' + 1) vs
      | Some index' ->
        (String.sub s index (index' - index))::vs |> loop (index' + 1)
  in
  loop 0 []

let classes node =
  match attribute "class" node with
  | None -> []
  | Some classes -> split_attribute classes

let id = attribute "id"

type 'a nodes = {eliminate : 'b. ('b -> 'a node -> 'b) -> 'b -> 'b}

let empty = {eliminate = fun _ init -> init}

let fold f init sequence = sequence.eliminate f init

let filter_map f sequence =
  {eliminate = fun f' init ->
    init |> sequence.eliminate (fun v node ->
      match f node with
      | None -> v
      | Some node' -> f' v node')}

let filter f = filter_map (fun node -> if f node then Some node else None)

let map f = filter_map (fun node -> Some (f node))

let flatten f sequence =
  {eliminate = fun f' init ->
    init |> sequence.eliminate (fun v node -> v |> (f node).eliminate f')}

let iter f sequence = fold (fun () node -> f node) () sequence

let nth index sequence =
  with_stop (fun stop ->
    sequence
    |> fold (fun index' node ->
      if index' = index then stop.throw (Some node) else index' + 1)
      1
    |> ignore;
    None)

let first sequence = nth 1 sequence

let last sequence = sequence |> fold (fun _ node -> Some node) None

let count sequence = sequence |> fold (fun count _ -> count + 1) 0

let to_list sequence = fold (fun l node -> node::l) [] sequence |> List.rev

let of_list l = {eliminate = fun f init -> List.fold_left f init l}

let rev sequence = sequence |> to_list |> List.rev |> of_list

let elements sequence =
  {eliminate = fun f init ->
    init |> sequence.eliminate (fun v node ->
      match element node with
      | None -> v
      | Some element -> f v element)}

let child_list = function
  | {values = `Element {children; _}; _} -> Some children
  | {values = `Document {roots}; _} -> Some roots
  | _ -> None

let children node =
  match child_list node with
  | Some children -> {eliminate = fun f init -> List.fold_left f init children}
  | _ -> empty

let rec descendants node =
  {eliminate = fun f init ->
    init |> (children node).eliminate (fun v child ->
      f v child |> (descendants (forget_type child)).eliminate f)}

let child node = node |> children |> first

let child_element node = node |> children |> elements |> first

let simple_parent node = node.parent

let parent node =
  match node.parent with
  | None -> None
  | Some node when is_document node -> None
  | Some node -> Some node

let rec general_ancestors get_parent node =
  {eliminate = fun f init ->
    match get_parent node with
    | None -> init
    | Some parent ->
      f init parent
      |> (general_ancestors get_parent (forget_type parent)).eliminate f}

let simple_ancestors = general_ancestors simple_parent
let ancestors node = general_ancestors parent node

let siblings node =
  match simple_parent node with
  | None -> empty
  | Some parent ->
    children parent
    |> filter (fun child -> child != (forget_type node))

let split_at_identity function_name v l =
  let rec loop prefix = function
    | [] ->
      failwith
        ("Soup." ^ function_name ^
         ": internal error: child not in parent's child list") [@coverage off]
    | u::suffix ->
      if u == v then prefix, suffix else loop (u::prefix) suffix
  in
  loop [] l

let sibling_lists function_name select node =
  match simple_parent node with
  | None -> empty
  | Some parent ->
    match child_list parent with
    | None ->
      failwith
        ("Soup." ^ function_name ^ ": internal error: parent has no children")
          [@coverage off]
    | Some children ->
      let lists =
        split_at_identity function_name (forget_type node) children in
      {eliminate = fun f init -> select lists |> List.fold_left f init}

let next_siblings node = sibling_lists "next_siblings" snd node
let previous_siblings node = sibling_lists "previous_siblings" fst node

let next_sibling node = next_siblings node |> first
let previous_sibling node = previous_siblings node |> first

let next_element node = next_siblings node |> elements |> first
let previous_element node = previous_siblings node |> elements |> first

let index_of node =
  match simple_parent node with
  | None -> 1
  | Some parent ->
    match child_list parent with
    | None ->
      failwith "Soup.index_of: internal error: parent has no children"
        [@coverage off]
    | Some children ->
      with_stop (fun stop ->
        children |> List.iteri (fun index child ->
          if child == (forget_type node) then stop.throw (index + 1));
        failwith
          "Soup.index_of: internal error: child not in parent's child list")
            [@coverage off]

let index_of_element element =
  match simple_parent element with
  | None -> 1
  | Some parent ->
    with_stop (fun stop ->
      parent
      |> children
      |> elements
      |> fold (fun index element' ->
        if element' == element then stop.throw index else index + 1) 1
      |> ignore [@coverage off];
      (failwith
        ("Soup.index_of_element: internal error: " ^
        "element is not a child of its own parent")) [@coverage off])

let at_most_n_children count node =
  match nth (count + 1) (children node) with
  | None -> true
  | Some _ -> false

let no_children node = at_most_n_children 0 node
let at_most_one_child node = at_most_n_children 1 node

let is_root node =
  match node.parent with
  | None -> not (is_document node)
  | Some parent -> is_document parent

let tags name' node =
  let name' = String.lowercase_ascii name' in
  node
  |> descendants
  |> elements
  |> filter (fun element -> name element = name')

let tag name node = tags name node |> first

let normalize_children trim children =
  let rec loop prefix = function
    | [] -> List.rev prefix
    | node::rest ->
      match node.values with
      | `Text s ->
        let s = trim s in
        if s = "" then loop prefix rest
        else
          (match prefix with
          | {values = `Text s'; _}::prefix' ->
            loop ((create_text (s' ^ s))::prefix') rest
          | _ -> loop ((create_text s)::prefix) rest)
      | _ -> loop (node::prefix) rest
  in

  loop [] children

let rec leaf_text node =
  let trim s = if String.trim s = "" then "" else s in

  match node.values with
  | `Text s -> Some s
  | `Element _
  | `Document _ ->
    let children =
      child_list node
      |> require_internal
        ("Soup.leaf_text: internal error: node is not a text node, " ^
         "but has no child list")
      |> normalize_children trim
    in
    match children with
    | [] -> Some ""
    | [child] -> leaf_text (forget_type child)
    | _ -> None

let rec texts node =
  match node.values with
  | `Text s -> [s]
  | `Element {children; _} ->
    children |> List.map forget_type |> List.map texts |> List.fold_left (@) []
  | `Document {roots} ->
    roots |> List.map forget_type |> List.map texts |> List.fold_left (@) []

let trimmed_texts node =
  texts node
  |> List.map String.trim
  |> List.filter (fun s -> String.length s > 0)

exception Parse_error of string

module Selector :
sig
  type t

  val parse : string -> t
  val select : (_ node) -> t -> element nodes
end =
struct
  type type_ = Name of string | Universal

  type attribute =
    | Present of string
    | Exactly of string * string
    | Member of string * string
    | HasDashSeparatedPrefix of string * string
    | Prefix of string * string
    | Suffix of string * string
    | Substring of string * string

  type pseudo_class =
    | Root
    | NthChild of int * int
    | NthLastChild of int * int
    | NthOfType of int * int
    | NthLastOfType of int * int
    | OnlyChild
    | OnlyOfType
    | Empty
    | Content of string
    | Not of simple_selector

  and simple_selector =
    | Type of type_
    | Attribute of attribute
    | Pseudo_class of pseudo_class

  type combinator =
    | Descendant
    | Child
    | AdjacentSibling
    | IndirectSibling

  type t = (combinator * simple_selector list) list

  let has_prefix prefix s = String.sub s 0 (String.length prefix) = prefix

  let has_suffix suffix s =
    let suffix_length = String.length suffix in
    String.sub s (String.length s - suffix_length) suffix_length = suffix

  let has_substring s' s =
    let substring_length = String.length s' in
    let rec loop index =
      if String.sub s index substring_length = s' then true
      else loop (index + 1)
    in
    try loop 0
    with Invalid_argument _ -> false

  let matches_attribute_selector node selector =
    let captured =
      "Soup.matches_attribute_selector: internal error: " ^
      "this exception should have been caught"
    in

    try
      match selector with
      | Present name -> has_attribute name node
      | Exactly (name, value) -> attribute name node = Some value
      | Member (name, value) ->
        attribute name node
        |> require_internal captured
        |> split_attribute
        |> List.mem value
      | HasDashSeparatedPrefix (name, value) ->
        let value' = attribute name node |> require_internal captured in
        value' = value || has_prefix (value ^ "-") value'
      | Prefix (name, value) ->
        attribute name node |> require_internal captured |> has_prefix value
      | Suffix (name, value) ->
        attribute name node |> require_internal captured |> has_suffix value
      | Substring (name, value) ->
        attribute name node |> require_internal captured |> has_substring value

    with _ -> false

  let element_count node =
    match simple_parent node with
    | None -> 1 [@coverage off]
    | Some parent -> parent |> children |> elements |> count

  let element_count_with_name name' node =
    match simple_parent node with
    | None -> 1 [@coverage off]
    | Some parent ->
      parent
      |> children
      |> elements
      |> filter (fun element -> name element = name')
      |> count

  let element_index_with_name name' node =
    match simple_parent node with
    | None -> 1 [@coverage off]
    | Some parent ->
      with_stop (fun stop ->
        flush stdout;
        parent
        |> children
        |> elements
        |> filter (fun element -> name element = name')
        |> fold (fun index element ->
          if element == node then stop.throw index else index + 1)
          1
        |> ignore [@coverage off];
        (failwith
          ("Soup.Selector.element_index_with_name: internal error: " ^
           "parent does not have given child")) [@coverage off])

  let conditional_mod n a = if a = 0 then n else n mod a

  let rec matches_pseudo_class_selector node selector =
    match selector with
    | Root -> parent node = None
    | NthChild (a, b) -> conditional_mod (index_of_element node) a = b
    | NthLastChild (a, b) ->
      let element_count = element_count node in
      conditional_mod (element_count - (index_of_element node) + 1) a = b
    | NthOfType (a, b) ->
      conditional_mod (element_index_with_name (name node) node) a = b
    | NthLastOfType (a, b) ->
      let name = name node in
      let element_count = element_count_with_name name node in
      conditional_mod
        (element_count - (element_index_with_name name node) + 1) a = b
    | OnlyChild -> element_count node = 1
    | OnlyOfType -> element_count_with_name (name node) node = 1
    | Empty -> no_children node
    | Content s -> texts node |> String.concat "" |> has_substring s
    | Not selector -> not (matches_simple_selector node selector)

  and matches_simple_selector node = function
    | Type Universal -> true
    | Type (Name name') -> name node = (String.lowercase_ascii name')
    | Attribute attribute_selector ->
      matches_attribute_selector node attribute_selector
    | Pseudo_class pseudo_class_selector ->
      matches_pseudo_class_selector node pseudo_class_selector

  let matches_simple_selectors node selectors =
    List.for_all (matches_simple_selector node) selectors

  let up_to node sequence =
    {eliminate = fun f init ->
      with_stop (fun stop ->
        sequence.eliminate (fun v node' ->
          let v' = f v node' in
          if node' == node then stop.throw v' else v') init)}

  let one sequence =
    {eliminate = fun f init ->
      with_stop (fun stop ->
        sequence.eliminate (fun v node -> f v node |> stop.throw) init)}

  let select root_node selector =
    let root_node = forget_type root_node in

    let matches_selector at_node =
      with_stop (fun stop ->
        let rec backwards_traversal at_node = function
          | [] -> if at_node == root_node then stop.throw true else ()
          | (combinator, simple_selectors)::rest ->
            if not (is_element at_node) then ()
            else
              if not (matches_simple_selectors at_node simple_selectors) then ()
              else
                let next_nodes =
                  match combinator with
                  | Descendant ->
                    at_node |> simple_ancestors |> up_to root_node
                  | Child -> at_node |> ancestors |> one
                  | IndirectSibling ->
                    at_node |> previous_siblings |> elements |> up_to root_node
                  | AdjacentSibling ->
                    at_node |> previous_siblings |> elements |> one
                in
                next_nodes |> iter (fun node -> backwards_traversal node rest)
        in
        backwards_traversal at_node (List.rev selector);
        false)
    in

    let candidates =
      match simple_parent root_node with
      | None -> descendants root_node
      | Some parent -> descendants parent
    in

    candidates
    |> elements
    |> filter matches_selector

  let is_decimal_char c =
    ((Char.code c) >= (Char.code '0')) && ((Char.code c) <= (Char.code '9'))

  let is_hexadecimal_char c =
    (is_decimal_char c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

  let is_identifier_char c =
    let c = Char.lowercase_ascii c in
    ((Char.code c) >= (Char.code 'a') && (Char.code c) <= (Char.code 'z')) ||
    (is_decimal_char c) || (c == '-') || (c == '_')

  let is_whitespace_char c =
    c = ' ' || c = '\t' || c = '\n' || c = '\r'

  let is_continuation_simple_selector_start_char c =
    (c == '.') || (c == '#') || (c == '[') || (c == ':')

  let hexadecimal_value = function
    | 'A' | 'a' -> 0xA
    | 'B' | 'b' -> 0xB
    | 'C' | 'c' -> 0xC
    | 'D' | 'd' -> 0xD
    | 'E' | 'e' -> 0xE
    | 'F' | 'f' -> 0xF
    | c -> Char.code c - Char.code '0'

  let parse_error msg = raise (Parse_error msg)

  let rec parse_hexadecimal_escape value count stream =
    if count >= 6 then
      value
    else
      match Stream.peek stream with
      | Some (' ' | '\t' | '\n') ->
        Stream.junk stream;
        value
      | Some '\r' ->
        begin match Stream.npeek 2 stream with
        | ['\r'; '\n'] ->
          Stream.junk stream;
          Stream.junk stream
        | _ -> ()
        end;
        value
      | Some c when is_hexadecimal_char c ->
        Stream.junk stream;
        let value = value * 0x10 + hexadecimal_value c in
        parse_hexadecimal_escape value (count + 1) stream
      | _ -> value

  let parse_escape_sequence stream =
    Stream.junk stream;
    match Stream.peek stream with
    | None -> '\\'
    | Some c when is_hexadecimal_char c ->
      begin match parse_hexadecimal_escape 0 0 stream with
      | n when n > 0xFF -> '\x1A'
      | n -> Char.chr n
      end
    | Some c ->
      Stream.junk stream;
      c

  let parse_identifier stream =
    let buffer = Buffer.create 32 in
    begin match Stream.peek stream with
    | Some '\\' -> ()
    | Some c when is_identifier_char c -> ()
    | _ -> parse_error "expected an identifier"
    end;
    let rec loop () =
      match Stream.peek stream with
      | Some '\\' ->
        Buffer.add_char buffer (parse_escape_sequence stream); loop ()
      | Some c when is_identifier_char c ->
        Buffer.add_char buffer c; Stream.junk stream; loop ()
      | _ -> Buffer.contents buffer
    in
    loop ()

  let parse_type_selector stream =
    match Stream.peek stream with
    | Some '*' -> Stream.junk stream; Universal
    | _ ->
      try
        let name = parse_identifier stream in
        Name name
      with _ -> parse_error "expected tag name or '*'"

  let parse_attribute_operator stream =
    match Stream.npeek 2 stream with
    | ['='; _] -> Stream.junk stream; "="
    | [c; '='] ->
      Stream.junk stream; Stream.junk stream; Printf.sprintf "%c=" c
    | _ -> parse_error "expected attribute operator"

  let parse_quoted_string stream =
    match Stream.peek stream with
    | Some ('"' as delim) | Some ('\'' as delim) ->
      Stream.junk stream;
      let buffer = Buffer.create 64 in
      let rec loop () =
        match Stream.peek stream with
        | Some c when c = delim ->
          Stream.junk stream; Buffer.contents buffer
        | Some '\\' ->
          Stream.junk stream;
          (match Stream.peek stream with
          | Some c when c = delim ->
            Buffer.add_char buffer delim; Stream.junk stream
          | _ ->
            Buffer.add_char buffer '\\');
          loop ()
        | Some c ->
          Buffer.add_char buffer c; Stream.junk stream; loop ()
        | None -> parse_error "unterminated string"
      in
      loop ()
    | _ -> parse_error "expected quoted string"

  let parse_string stream =
    match Stream.peek stream with
    | Some '"' | Some '\'' -> parse_quoted_string stream
    | _ ->
      let buffer = Buffer.create 32 in
      let rec loop () =
        match Stream.peek stream with
        | Some ']' | None -> Buffer.contents buffer
        | Some c -> Buffer.add_char buffer c; Stream.junk stream; loop ()
      in
      loop ()

  let consume_whitespace stream =
    let rec loop () =
      match Stream.peek stream with
      | Some c when is_whitespace_char c -> Stream.junk stream; loop ()
      | _ -> ()
    in
    loop ()

  let parse_attribute_selector stream =
    Stream.junk stream;
    consume_whitespace stream;
    let name = parse_identifier stream in
    consume_whitespace stream;
    (match Stream.peek stream with
    | None -> parse_error "unterminated attribute selector"
    | Some ']' -> Stream.junk stream; Present name
    | Some _ ->
      let operator = parse_attribute_operator stream in
      consume_whitespace stream;
      (match Stream.peek stream with
      | None ->
        parse_error "unterminated attribute selector"
      | Some ']' ->
        parse_error "expected value in attribute selector"
      | Some _ ->
        let value = parse_string stream in
        consume_whitespace stream;
        (match Stream.peek stream with
        | None ->
          parse_error "unterminated attribute selector"
        | Some ']' ->
          Stream.junk stream;
          (match operator with
          | "=" -> Exactly (name, value)
          | "~=" -> Member (name, value)
          | "|=" -> HasDashSeparatedPrefix (name, value)
          | "^=" -> Prefix (name, value)
          | "$=" -> Suffix (name, value)
          | "*=" -> Substring (name, value)
          | _ ->
            Printf.ksprintf parse_error
              "invalid attribute operator '%s'" operator)
        | Some _ -> parse_error "expected end of attribute selector (']')")))

  let parse_class_selector stream =
    Stream.junk stream;
    let value = parse_identifier stream in
    Member ("class", value)

  let parse_id_selector stream =
    Stream.junk stream;
    let value = parse_identifier stream in
    Exactly ("id", value)

  let parse_number stream =
    let buffer = Buffer.create 16 in
    let rec loop () =
      match Stream.peek stream with
      | Some c when is_decimal_char c ->
        Buffer.add_char buffer c; Stream.junk stream; loop ()
      | _ -> Buffer.contents buffer
    in
    loop () |> int_of_string

  let parse_modular_pattern_tail a stream =
    Stream.junk stream;
    match Stream.peek stream with
    | Some ('+' as c) | Some ('-' as c) ->
      Stream.junk stream;
      (match Stream.peek stream with
      | Some c' when is_decimal_char c' ->
        let b = parse_number stream in
        let b =
          if c = '+' then b mod a
          else a - (b mod a)
        in
        a, b
      | _ -> parse_error "expected number after '+' or '-'")
    | _ -> a, 0

  let parse_modular_pattern stream =
    match Stream.peek stream with
    | Some 'e' | Some 'o' ->
      (match parse_identifier stream with
      | "even" -> (2, 0)
      | "odd" -> (2, 1)
      | _ -> parse_error "expected 'n', 'even', or 'odd'")
    | Some 'n' -> parse_modular_pattern_tail 1 stream
    | Some c when is_decimal_char c ->
      let a = parse_number stream in
      (match Stream.peek stream with
      | Some 'n' -> parse_modular_pattern_tail a stream
      | _ -> (0, a))
    | _ -> parse_error "expected expression"

  let parse_parenthesized_value f stream =
    match Stream.peek stream with
    | Some '(' ->
      Stream.junk stream;
      consume_whitespace stream;
      let value = f stream in
      consume_whitespace stream;
      (match Stream.peek stream with
      | Some ')' -> Stream.junk stream; value
      | _ -> parse_error "unterminated '('")
    | _ -> parse_error "expected parenthesized expression"

  let rec parse_pseudo_class_selector stream =
    Stream.junk stream;
    let function_ = parse_identifier stream in
    (match function_ with
    | "root" -> Root
    | "first-child" -> NthChild (0, 1)
    | "last-child" -> NthLastChild (0, 1)
    | "first-of-type" -> NthOfType (0, 1)
    | "last-of-type" -> NthLastOfType (0, 1)
    | "only-child" -> OnlyChild
    | "only-of-type" -> OnlyOfType
    | "nth-child" ->
      let a, b = parse_parenthesized_value parse_modular_pattern stream in
      NthChild (a, b)
    | "nth-of-type" ->
      let a, b = parse_parenthesized_value parse_modular_pattern stream in
      NthOfType (a, b)
    | "nth-last-child" ->
      let a, b = parse_parenthesized_value parse_modular_pattern stream in
      NthLastChild (a, b)
    | "nth-last-of-type" ->
      let a, b = parse_parenthesized_value parse_modular_pattern stream in
      NthLastOfType (a, b)
    | "contains" ->
      let s = parse_parenthesized_value parse_quoted_string stream in
      Content s
    | "empty" -> Empty
    | "not" ->
      let selector = parse_parenthesized_value parse_simple_selector stream in
      Not selector
    | _ ->
      Printf.ksprintf parse_error
        "unknown pseudo-class or pseudo-element ':%s'" function_)

  and parse_simple_selector stream =
    match Stream.peek stream with
    | Some '[' -> Attribute (parse_attribute_selector stream)
    | Some ':' -> Pseudo_class (parse_pseudo_class_selector stream)
    | Some '.' -> Attribute (parse_class_selector stream)
    | Some '#' -> Attribute (parse_id_selector stream)
    | Some _ -> Type (parse_type_selector stream)
    | None -> parse_error "expected simple selector"

  let parse_simple_selector_list stream =
    let first = parse_simple_selector stream in
    let rec loop selectors =
      match Stream.peek stream with
      | Some c when is_continuation_simple_selector_start_char c ->
        (parse_simple_selector stream)::selectors |> loop
      | _ -> List.rev selectors
    in
    loop [first]

  let parse s =
    let stream = Stream.of_string s in
    let rec loop selectors =
      consume_whitespace stream;
      match Stream.peek stream with
      | None -> List.rev selectors
      | _ ->
        let combinator =
          match Stream.peek stream with
          | Some '>' -> Stream.junk stream; Child
          | Some '+' -> Stream.junk stream; AdjacentSibling
          | Some '~' -> Stream.junk stream; IndirectSibling
          | _ -> Descendant
        in
        consume_whitespace stream;
        (combinator, parse_simple_selector_list stream)::selectors |> loop
    in
    loop []
end

let select selector node =
  selector |> Selector.parse |> Selector.select node

let select_one selector node = select selector node |> first

let ($) node selector =
  node |> select_one selector
  |> require_internal
    (Printf.sprintf "Soup.($): '%s' not found.\n%s"
      selector
      "Try Soup.($?) if you'd prefer returning None instead of an exception.")

let ($?) node selector = node |> select_one selector

let ($$) node selector = node |> select selector

module Infix =
struct
  let ($) = ($)
  let ($?) = ($?)
  let ($$) = ($$)
end

let signals root =
  let root = forget_type root in

  let rec traverse acc = function
    | {values = `Element {name; attributes; children}; _} ->
      let start_signal =
        `Start_element
          (("http://www.w3.org/1999/xhtml", name),
           List.map (fun (n, v) -> ("", n), v) attributes)
      in
      `End_element::(traverse_list (start_signal::acc) children)

    | {values = `Document {roots}; _} -> traverse_list acc roots
    | {values = `Text s; _} -> (`Text [s])::acc

  and traverse_list acc l = List.fold_left traverse acc l

  in

  let signals = List.rev (traverse [] root) |> Markup.of_list in
  match root with
  | {values =
    `Document {roots = {values = `Element {name = "html"; _}; _}::_}; _}
  | {values =
    `Element {name = "html"; _}; _} ->
    Markup.html5 signals
  | _ ->
    signals

let pretty_print root =
  signals root
  |> Markup.pretty_print |> (fun s -> Markup.write_html s) |> Markup.to_string

let to_string root =
  signals root |> (fun s -> Markup.write_html s) |> Markup.to_string

let rec equal_general normalize_children n n' =
  let equal_text s s' = s = s' in

  let equal_children children children' =
    let children = normalize_children children in
    let children' = normalize_children children' in

    try
      List.iter2 (fun c c' ->
        if not (equal_general normalize_children c c') then
          raise_notrace (Invalid_argument "not equal"))
        children children';
      true
    with Invalid_argument _ -> false
  in

  let equal_element values values' =
      (values.name = values'.name)
    &&
      begin
        let sort =
          List.sort (fun attr attr' -> compare (fst attr) (fst attr')) in
        (sort values.attributes) = (sort values'.attributes)
      end
    &&
      equal_children values.children values'.children
  in

  let equal_document values values' =
    equal_children values.roots values'.roots
  in

  match n, n' with
  | {values = `Text s; _}, {values = `Text s'; _} -> equal_text s s'
  | {values = `Element v; _}, {values = `Element v'; _} -> equal_element v v'
  | {values = `Document v; _}, {values = `Document v'; _} -> equal_document v v'
  | _ -> false

let equal n n' =
  equal_general
    (normalize_children (fun s -> s)) (forget_type n) (forget_type n')

let equal_modulo_whitespace n n' =
  equal_general
    (normalize_children String.trim) (forget_type n) (forget_type n')

let mutate_child_list f node =
  match node.values with
  | `Element values -> values.children <- f values.children
  | `Document values -> values.roots <- f values.roots
  | `Text _ -> failwith "Soup.mutate_child_list: node has no children"

let strip_document node =
  if is_document node then
    let children = node |> children |> to_list in
    (children |> List.iter (fun child -> child.parent <- None);
    mutate_child_list (fun _ -> []) node);
    children
  else
    [node]

let delete node =
  match node.parent with
  | None -> ()
  | Some parent ->
    mutate_child_list
      (List.filter (fun child -> child != (forget_type node))) parent;
    node.parent <- None

let insert_at_index k element node =
  let element = forget_type element in
  let node = forget_type node in

  delete node;

  let nodes = strip_document node in

  mutate_child_list (fun l ->
    let rec loop prefix index = function
      | [] -> (List.rev prefix) @ nodes
      | x::l' ->
        if k <= index then (List.rev prefix) @ nodes @ (x::l')
        else loop (x::prefix) (index + 1) l'
    in
    loop [] 1 l) element;

  nodes |> List.iter (fun node -> node.parent <- Some element)

let append_child element node =
  insert_at_index ((element |> children |> count) + 1) element node

let prepend_child element node =
  insert_at_index 1 element node

let insert_before target node =
  insert_at_index
    (index_of target)
    (parent target
     |> require_internal "Soup.insert_before: target node has no parent")
    node

let insert_after target node =
  insert_at_index
    ((index_of target) + 1)
    (parent target
     |> require_internal "Soup.insert_after: target node has no parent")
    node

let clear node =
  mutate_child_list (fun children ->
    children |> List.iter (fun child -> child.parent <- None); []) node

let replace target node =
  delete node;
  let parent =
    parent target
    |> require_internal "Soup.replace: target node has no parent"
  in
  let index = index_of target in
  delete target;
  insert_at_index index parent node

let swap target element =
  let internal = "Soup.swap: internal error: non-element node given" in
  let target_children = child_list target |> require_internal internal in
  let element_children = child_list element |> require_internal internal in
  target_children |> List.iter (fun child -> child.parent <- Some element);
  element_children |> List.iter (fun child -> child.parent <- Some target);
  mutate_child_list (fun _ -> element_children) target;
  mutate_child_list (fun _ -> target_children) element;
  replace target element

let wrap target element =
  delete element;
  clear element;
  replace target element;
  append_child element target

let unwrap node =
  let parent =
    parent node |> require_internal "Soup.unwrap: node has no parent" in
  let index = index_of node in
  delete node;
  let children =
    match child_list node with
    | None -> []
    | Some l -> l
  in
  (try clear node
  with Failure _ -> ());
  List.rev children |> List.iter (insert_at_index index parent)

let append_root document node =
  delete node;
  mutate_child_list (fun f -> f @ [forget_type node]) document;
  node.parent <- Some document

let set_name new_name = function
  | {values = `Element e; _} ->
    e.name <- new_name |> String.trim |> String.lowercase_ascii
  | _ ->
    failwith "Soup.set_name: internal error: not an element" [@coverage off]

let delete_attribute name = function
  | {values = `Element e; _} ->
    e.attributes <-
      e.attributes |> List.filter (fun (name', _) -> name' <> name)
  | _ ->
    failwith "Soup.delete_attribute: internal error: not an element"
      [@coverage off]

let set_attribute name value = function
  | {values = `Element e; _} ->
    e.attributes <-
      e.attributes
      |> List.filter (fun (name', _) -> name' <> name)
      |> fun attributes -> (name, value)::attributes
  | _ ->
    failwith "Soup.set_attribute: internal error: not an element"
      [@coverage off]

let set_classes classes element =
  classes |> String.concat " " |> fun v -> set_attribute "class" v element

let add_class class_ element =
  let classes = classes element in
  if List.mem class_ classes then ()
  else set_classes (class_::classes) element

let remove_class class_ element =
  classes element
  |> List.filter (fun c -> c <> class_)
  |> function
    | [] -> delete_attribute "class" element
    | v -> set_classes v element

let create_element ?id ?class_ ?classes ?(attributes = []) ?inner_text name =
  let children =
    match inner_text with
    | None -> []
    | Some s -> [create_text s]
  in

  let element = create_element name [] children in

  attributes |> List.iter (fun (n, v) -> set_attribute n v element);

  (match classes with
  | None -> ()
  | Some classes ->
    classes |> String.concat " " |> fun v -> set_attribute "class" v element);

  (match class_ with
  | None -> ()
  | Some class_ -> set_attribute "class" class_ element);

  (match id with
  | None -> ()
  | Some id -> set_attribute "id" id element);

  element

module R =
struct
  let select_one s n =
    select_one s n |> require_internal "Soup.R.select_one: None"

  let attribute s n =
    attribute s n |> require_internal "Soup.R.attribute: None"

  let id n =
    id n |> require_internal "Soup.R.id: None"

  let element n =
    element n |> require_internal "Soup.R.element: None"

  let leaf_text n =
    leaf_text n |> require_internal "Soup.R.leaf_text: None"

  let nth n t =
    nth n t |> require_internal "Soup.R.nth: None"

  let first t =
    first t |> require_internal "Soup.R.first: None"

  let last t =
    last t |> require_internal "Soup.R.last: None"

  let tag s n =
    tag s n |> require_internal "Soup.R.tag: None"

  let parent n =
    parent n |> require_internal "Soup.R.parent: None"

  let child n =
    child n |> require_internal "Soup.R.child: None"

  let child_element n =
    child_element n |> require_internal "Soup.R.child_element: None"

  let next_sibling n =
    next_sibling n |> require_internal "Soup.R.next_sibling: None"

  let previous_sibling n =
    previous_sibling n |> require_internal "Soup.R.previous_sibling: None"

  let next_element n =
    next_element n |> require_internal "Soup.R.next_element: None"

  let previous_element n =
    previous_element n |> require_internal "Soup.R.previous_element: None"
end

let read_channel channel = Markup.channel channel |> Markup.to_string

let read_file path = Markup.file path |> fst |> Markup.to_string

let write_channel = output_string

let write_file path data =
  Markup.string data |> Markup.to_file path
  [@@coverage off]
