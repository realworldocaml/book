let read_lexbuf
    ?(expand = false) ?keep_poly ?(xdebug = false)
    ?(inherit_fields = false)
    ?(inherit_variants = false)
    ?(pos_fname = "")
    ?(pos_lnum = 1)
    lexbuf =

  Lexer.init_fname lexbuf pos_fname pos_lnum;
  let head, body = Parser.full_module Lexer.token lexbuf in
  Check.check body;
  let body =
    if inherit_fields || inherit_variants then
      Inherit.expand_module_body ~inherit_fields ~inherit_variants body
    else
      body
  in
  let (body, original_types) =
    if expand then Expand.expand_module_body ?keep_poly ~debug: xdebug body
    else (body, Hashtbl.create 0)
  in
  ((head, body), original_types)

let read_channel
    ?expand ?keep_poly ?xdebug ?inherit_fields ?inherit_variants
    ?pos_fname ?pos_lnum
    ic =
  let lexbuf = Lexing.from_channel ic in
  let pos_fname =
    if pos_fname = None && ic == stdin then
      Some "<stdin>"
    else
      pos_fname
  in
  read_lexbuf ?expand ?keep_poly ?xdebug
    ?inherit_fields ?inherit_variants ?pos_fname ?pos_lnum lexbuf

let load_file
    ?expand ?keep_poly ?xdebug ?inherit_fields ?inherit_variants
    ?pos_fname ?pos_lnum
    file =
  let ic = open_in file in
  let finally () = close_in_noerr ic in
  try
    let pos_fname =
      match pos_fname with
          None -> Some file
        | Some _ -> pos_fname
    in
    let ast =
      read_channel ?expand ?keep_poly ?xdebug ?inherit_fields ?inherit_variants
        ?pos_fname ?pos_lnum ic
    in
    finally ();
    ast
  with e ->
    finally ();
    raise e

let load_string
    ?expand ?keep_poly ?xdebug ?inherit_fields ?inherit_variants
    ?pos_fname ?pos_lnum
    s =
  let lexbuf = Lexing.from_string s in
  read_lexbuf ?expand ?keep_poly ?xdebug
    ?inherit_fields ?inherit_variants ?pos_fname ?pos_lnum lexbuf

module Tsort = Sort.Make (
  struct
    type t = Ast.module_item
    type id = string (* type name *)

    let id def =
      let Ast.Type (_, (name, _, _), _) = def in
      name

    let to_string name = name
  end
)

let tsort l0 =
  let ignorable = [ "unit"; "bool"; "int"; "float"; "string"; "abstract" ] in
  let l =
    List.map (
      fun def ->
        let Ast.Type (_, (_, _, _), x) = def in
        let deps = Ast.extract_type_names ~ignorable x in
        (def, deps)
    ) l0
  in
  List.rev (Tsort.sort l)
