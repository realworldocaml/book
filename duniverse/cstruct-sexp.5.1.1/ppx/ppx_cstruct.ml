(*
 * Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Migrate_parsetree
open Printf

open Ast_404
open Longident
open Asttypes
open Parsetree
open Ast_helper
open Ast_mapper
module Loc = Location
module Ast = Ast_convenience_404

type mode = Big_endian | Little_endian | Host_endian | Bi_endian

type prim =
  | Char
  | UInt8
  | UInt16
  | UInt32
  | UInt64

type ty =
  | Prim of prim
  | Buffer of prim * int

type raw_field = {
  name: string;
  ty: ty;
  definition_loc: loc;
}

type named_field = {
  name: string;
  ty: ty;
  definition_loc: loc;
  off: int;
}

type field =
  | Named_field of named_field
  | Ignored_field

let field_is_ignored name =
  String.get name 0 = '_'

type t = {
  name: string;
  fields: field list;
  len: int;
  endian: mode;
}

let ty_of_string =
  function
  |"char_t"  |"char" -> Some Char
  |"uint8_t" |"uint8" |"int8" |"int8_t"  -> Some UInt8
  |"uint16_t"|"uint16"|"int16"|"int16_t" -> Some UInt16
  |"uint32_t"|"uint32"|"int32"|"int32_t" -> Some UInt32
  |"uint64_t"|"uint64"|"int64"|"int64_t" -> Some UInt64
  |_ -> None

let width_of_prim = function
  | Char -> 1
  | UInt8 -> 1
  | UInt16 -> 2
  | UInt32 -> 4
  | UInt64 -> 8

let width_of_ty = function
  | Prim p -> width_of_prim p
  | Buffer (p, len) -> width_of_prim p * len

let width_of_field f =
  width_of_ty f.ty

let field_to_string f =
  let rec string = function
    |Prim Char -> "char_t"
    |Prim UInt8 -> "uint8_t"
    |Prim UInt16 -> "uint16_t"
    |Prim UInt32 -> "uint32_t"
    |Prim UInt64 -> "uint64_t"
    |Buffer (prim, len) -> sprintf "%s[%d]" (string (Prim prim)) len
  in
  sprintf "%s %s" (string f.ty) f.name

let loc_err loc fmt = Location.raise_errorf ~loc ("ppx_cstruct: " ^^ fmt)

let parse_field loc name field_type sz =
  match ty_of_string field_type with
  |None -> loc_err loc "Unknown type %s" field_type
  |Some ty -> begin
    let ty = match ty,sz with
      |_,None -> Prim ty
      |prim,Some sz -> Buffer (prim, sz)
    in
    { name; ty; definition_loc = loc }
  end

let check_for_duplicates fields =
  let module StringSet = Set.Make(String) in
  let _ : StringSet.t =
    List.fold_left (fun seen f ->
        match f with
        | Ignored_field -> seen
        | Named_field {name; definition_loc; _} ->
          if StringSet.mem name seen then
            loc_err definition_loc "field %s is present several times in this type" name
          else
            StringSet.add name seen
      )
      StringSet.empty
      fields
  in
  ()

let create_struct loc endian name fields =
  let endian = match endian with
    |"little_endian" -> Little_endian
    |"big_endian" -> Big_endian
    |"host_endian" -> Host_endian
    |"bi_endian" -> Bi_endian
    |_ -> loc_err loc "unknown endian %s, should be little_endian, big_endian, host_endian or bi_endian" endian
  in
  let len, fields =
    List.fold_left (fun (off,acc) ({name; ty; definition_loc}:raw_field) ->
        let field =
          if field_is_ignored name then
            Ignored_field
          else
            Named_field { name; ty; off; definition_loc }
        in
        let off = width_of_ty ty + off in
        let acc = acc @ [field] in
        (off, acc)
      ) (0,[]) fields
  in
  check_for_duplicates fields;
  { fields; name = name.txt; len; endian }

let ($.) l x = Longident.Ldot (l, x)
let cstruct_id = Longident.Lident "Cstruct"
let mode_mod s = function
  |Big_endian -> cstruct_id$."BE"$.s
  |Little_endian -> cstruct_id$."LE"$.s
  |Host_endian -> cstruct_id$."HE"$.s
  |Bi_endian -> cstruct_id$."BL"$.s

let mode_mod loc x s =
  Exp.ident ~loc {loc ; txt = mode_mod s x}

type op =
  | Op_get of named_field
  | Op_set of named_field
  | Op_copy of named_field
  | Op_blit of named_field
  | Op_sizeof
  | Op_hexdump
  | Op_hexdump_to_buffer

let op_name s op =
  let parts =
    match op with
    | Op_get f -> ["get"; s.name; f.name]
    | Op_set f -> ["set"; s.name; f.name]
    | Op_copy f -> ["copy"; s.name; f.name]
    | Op_blit f -> ["blit"; s.name; f.name]
    | Op_sizeof -> ["sizeof"; s.name]
    | Op_hexdump -> ["hexdump"; s.name]
    | Op_hexdump_to_buffer -> ["hexdump"; s.name; "to_buffer"]
  in
  String.concat "_" parts

let op_pvar s op = Ast.pvar (op_name s op)
let op_evar s op = Ast.evar (op_name s op)

let get_expr loc s f =
  let m = mode_mod loc s.endian in
  let num x = Ast.int x in
  match f.ty with
  |Buffer (_, _) ->
    let len = width_of_field f in
    [%expr
      fun src -> Cstruct.sub src [%e num f.off] [%e num len]
    ]
  |Prim prim ->
    [%expr
      fun v ->
        [%e match prim with
            |Char -> [%expr Cstruct.get_char v [%e num f.off]]
            |UInt8 -> [%expr Cstruct.get_uint8 v [%e num f.off]]
            |UInt16 -> [%expr [%e m "get_uint16"] v [%e num f.off]]
            |UInt32 -> [%expr [%e m "get_uint32"] v [%e num f.off]]
            |UInt64 -> [%expr [%e m "get_uint64"] v [%e num f.off]]]]

let type_of_int_field = function
  |Char -> [%type: char]
  |UInt8 -> [%type: Cstruct.uint8]
  |UInt16 -> [%type: Cstruct.uint16]
  |UInt32 -> [%type: Cstruct.uint32]
  |UInt64 -> [%type: Cstruct.uint64]

let set_expr loc s f =
  let m = mode_mod loc s.endian in
  let num x = Ast.int x in
  match f.ty with
  |Buffer (_,_) ->
    let len = width_of_field f in
    [%expr
      fun src srcoff dst ->
        Cstruct.blit_from_string src srcoff dst [%e num f.off] [%e num len]]
  |Prim prim ->
    [%expr fun v x ->
        [%e match prim with
            |Char -> [%expr Cstruct.set_char v [%e num f.off] x]
            |UInt8 -> [%expr Cstruct.set_uint8 v [%e num f.off] x]
            |UInt16 -> [%expr [%e m "set_uint16"] v [%e num f.off] x]
            |UInt32 -> [%expr [%e m "set_uint32"] v [%e num f.off] x]
            |UInt64 -> [%expr [%e m "set_uint64"] v [%e num f.off] x]]]

let type_of_set f =
  match f.ty with
  |Buffer (_,_) ->
    [%type: string -> int -> Cstruct.t -> unit]
  |Prim prim ->
    let retf = type_of_int_field prim in
    [%type: Cstruct.t -> [%t retf] -> unit]

let hexdump_expr s =
  [%expr fun v ->
    let buf = Buffer.create 128 in
    Buffer.add_string buf [%e Ast.str (s.name ^ " = {\n")];
    [%e op_evar s Op_hexdump_to_buffer] buf v;
    print_endline (Buffer.contents buf);
    print_endline "}"
  ]

let hexdump_to_buffer_expr s =
  let prim_format_string = function
    | Char -> [%expr "%c\n"]
    | UInt8 | UInt16 -> [%expr "0x%x\n"]
    | UInt32 -> [%expr "0x%lx\n"]
    | UInt64 -> [%expr "0x%Lx\n"]
  in
  let hexdump_field = function
    | Ignored_field ->
      [%expr ()]
    | Named_field f ->
      let get_f = op_evar s (Op_get f) in
      let expr =
        match f.ty with
        |Prim p ->
          [%expr Printf.bprintf buf [%e prim_format_string p] ([%e get_f] v)]
        |Buffer (_,_) ->
          [%expr Printf.bprintf buf "<buffer %s>" [%e Ast.str (field_to_string f)];
            Cstruct.hexdump_to_buffer buf ([%e get_f] v)]
    in
    [%expr
      Printf.bprintf buf "  %s = " [%e Ast.str f.name];
      [%e expr]]
  in
  [%expr fun buf v -> [%e Ast.sequence (List.map hexdump_field s.fields)]]

let op_expr loc s = function
  | Op_sizeof -> Ast.int s.len
  | Op_hexdump -> hexdump_expr s
  | Op_hexdump_to_buffer -> hexdump_to_buffer_expr s
  | Op_get f -> get_expr loc s f
  | Op_set f -> set_expr loc s f
  | Op_copy f ->
    let len = width_of_field f in
    [%expr fun src -> Cstruct.copy src [%e Ast.int f.off] [%e Ast.int len] ]
  | Op_blit f ->
    let len = width_of_field f in
    [%expr fun src srcoff dst ->
      Cstruct.blit src srcoff dst [%e Ast.int f.off] [%e Ast.int len]]

let field_ops_for =
  function
  | Ignored_field ->
    []
  | Named_field f ->
    let if_buffer x =
      match f.ty with
      |Buffer (_,_) -> [x]
      |Prim _ -> []
    in
    List.concat
      [ [Op_get f]
      ; if_buffer (Op_copy f)
      ; [Op_set f]
      ; if_buffer (Op_blit f)
      ]

let ops_for s =
  ( [Op_sizeof]
  @ List.concat (List.map field_ops_for s.fields)
  @ [Op_hexdump_to_buffer;
     Op_hexdump;
    ])

(** Generate functions of the form {get/set}_<struct>_<field> *)
let output_struct_one_endian loc s =
  List.map
    (fun op ->
       [%stri let[@ocaml.warning "-32"] [%p op_pvar s op] =
                [%e op_expr loc s op]])
    (ops_for s)

let output_struct _loc s =
  match s.endian with
  | Bi_endian ->
    (* In case of Bi-endian, create two modules - one for BE and one for LE *)
    let expr_be = Mod.structure (output_struct_one_endian _loc {s with endian = Big_endian})
    and expr_le = Mod.structure (output_struct_one_endian _loc {s with endian = Little_endian})

    in [{pstr_desc = Pstr_module
        {pmb_name = {txt = "BE"; loc = _loc}; pmb_expr = expr_be;
        pmb_attributes = []; pmb_loc = _loc;}; pstr_loc = _loc;};
        {pstr_desc = Pstr_module
        {pmb_name = {txt = "LE"; loc = _loc}; pmb_expr = expr_le;
        pmb_attributes = []; pmb_loc = _loc;}; pstr_loc = _loc;}
        ]
  | _ -> output_struct_one_endian _loc s

let type_of_get f =
  match f.ty with
  |Buffer (_,_) ->
    [%type: Cstruct.t -> Cstruct.t]
  |Prim prim ->
    let retf = type_of_int_field prim in
    [%type: Cstruct.t -> [%t retf]]

let op_typ = function
  | Op_sizeof -> [%type: int]
  | Op_hexdump_to_buffer -> [%type: Buffer.t -> Cstruct.t -> unit]
  | Op_hexdump -> [%type: Cstruct.t -> unit]
  | Op_get f -> type_of_get f
  | Op_set f -> type_of_set f
  | Op_copy _ -> [%type: Cstruct.t -> string]
  | Op_blit _ -> [%type: Cstruct.t -> int -> Cstruct.t -> unit]

(** Generate signatures of the form {get/set}_<struct>_<field> *)
let output_struct_sig loc s =
  List.map
    (fun op ->
       Sig.value
         (Val.mk
            (Loc.mkloc (op_name s op) loc)
            (op_typ op)))
    (ops_for s)

type enum_op =
  | Enum_to_sexp
  | Enum_of_sexp
  | Enum_get
  | Enum_set
  | Enum_print
  | Enum_parse
  | Enum_compare

type cenum =
  { name : string Loc.loc;
    fields : (string Loc.loc * int64) list;
    prim : prim;
    sexp : bool;
  }

let enum_op_name cenum =
  let s = cenum.name.txt in
  function
  | Enum_to_sexp -> sprintf "sexp_of_%s" s
  | Enum_of_sexp -> sprintf "%s_of_sexp" s
  | Enum_get -> sprintf "int_to_%s" s
  | Enum_set -> sprintf "%s_to_int" s
  | Enum_print -> sprintf "%s_to_string" s
  | Enum_parse -> sprintf "string_to_%s" s
  | Enum_compare -> sprintf "compare_%s" s

let enum_pattern {prim; _} =
  let pat_integer f suffix i =
    Pat.constant (Pconst_integer(f i, suffix))
  in
  match prim with
  | Char ->
    (fun i -> Ast.pchar (Char.chr (Int64.to_int i)))
  | (UInt8 | UInt16) -> pat_integer Int64.to_string None
  | UInt32 -> pat_integer (fun i -> Int32.to_string (Int64.to_int32 i)) (Some 'l')
  | UInt64 -> pat_integer Int64.to_string (Some 'L')

let enum_integer {prim; _} =
  let expr_integer f suffix i =
    Exp.constant (Pconst_integer(f i, suffix))
  in
  match prim with
    | Char -> (fun i -> Ast.char (Char.chr (Int64.to_int i)))
    | (UInt8 | UInt16) -> expr_integer Int64.to_string None
    | UInt32 -> expr_integer (fun i -> Int32.to_string (Int64.to_int32 i)) (Some 'l')
    | UInt64 -> expr_integer Int64.to_string (Some 'L')

let declare_enum_expr ({fields; _} as cenum) = function
  | Enum_to_sexp ->
    [%expr fun x -> Sexplib.Sexp.Atom ([%e Ast.evar (enum_op_name cenum Enum_print)] x) ]
  | Enum_of_sexp ->
    [%expr
      fun x ->
        match x with
        | Sexplib.Sexp.List _ ->
          raise (Sexplib.Pre_sexp.Of_sexp_error (Failure "expected Atom, got List", x))
        | Sexplib.Sexp.Atom v ->
          match [%e Ast.evar (enum_op_name cenum Enum_parse)] v with
          | None ->
            raise (Sexplib.Pre_sexp.Of_sexp_error (Failure "unable to parse enum string", x))
          | Some r -> r
    ]
  | Enum_get ->
    let getters = (List.map (fun ({txt = f; _},i) ->
        Exp.case (enum_pattern cenum i) [%expr Some [%e Ast.constr f []]]
      ) fields) @ [Exp.case [%pat? _] [%expr None]]
    in
    Exp.function_ getters
  | Enum_set ->
    let setters = List.map (fun ({txt = f; _},i) ->
        Exp.case (Ast.pconstr f []) (enum_integer cenum i)
      ) fields in
    Exp.function_ setters
  | Enum_print ->
    let printers = List.map (fun ({txt = f; _},_) ->
        Exp.case (Ast.pconstr f []) (Ast.str f)
      ) fields in
    Exp.function_ printers
  | Enum_parse ->
    let parsers = List.map (fun ({txt = f; _},_) ->
        Exp.case (Ast.pstr f) [%expr Some [%e Ast.constr f []]]
      ) fields in
    Exp.function_ (parsers @ [Exp.case [%pat? _] [%expr None]])
  | Enum_compare -> [%expr fun x y ->
    let to_int = [%e Ast.evar (enum_op_name cenum Enum_set)] in
    Stdlib.compare (to_int x) (to_int y)
  ]

let enum_ops_for {sexp; _} =
  Enum_get ::
  Enum_set ::
  Enum_compare ::
  Enum_print ::
  Enum_parse ::
  if sexp then
    [ Enum_to_sexp
    ; Enum_of_sexp
    ]
  else
    []

let enum_type_decl {name; fields; _} =
  let decls = List.map (fun (f,_) -> Type.constructor f) fields in
  Type.mk ~kind:(Ptype_variant decls) name

let output_enum cenum =
  Str.type_ Recursive [enum_type_decl cenum] ::
  List.map
    (fun op ->
       [%stri
         let[@ocaml.warning "-32"] [%p Ast.pvar (enum_op_name cenum op)] =
           [%e declare_enum_expr cenum op]
       ])
    (enum_ops_for cenum)

let enum_op_type {name; prim; _} =
  let cty = Ast.tconstr name.txt [] in
  let oty = match prim with
    | Char -> [%type: char]
    | (UInt8|UInt16) -> [%type: int]
    | UInt32 -> [%type: int32]
    | UInt64 -> [%type: int64]
  in
  function
  | Enum_get -> [%type: [%t oty] -> [%t cty] option]
  | Enum_set -> [%type: [%t cty] -> [%t oty]]
  | Enum_print -> [%type: [%t cty] -> string]
  | Enum_parse -> [%type: string -> [%t cty] option]
  | Enum_to_sexp -> [%type: [%t cty] -> Sexplib.Sexp.t]
  | Enum_of_sexp -> [%type: Sexplib.Sexp.t -> [%t cty]]
  | Enum_compare -> [%type: [%t cty] -> [%t cty] -> int]

let output_enum_sig loc (cenum:cenum) =
  Sig.type_ Recursive [enum_type_decl cenum] ::
  List.map
    (fun op ->
       let name = enum_op_name cenum op in
       let typ = enum_op_type cenum op in
       Sig.value (Val.mk (Loc.mkloc name loc) typ))
    (enum_ops_for cenum)

let constr_enum = function
  | {pcd_name = f; pcd_args = Pcstr_tuple []; pcd_attributes = attrs; _} ->
    let id = match attrs with
      | [{txt = "id"; _}, PStr
           [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant cst; pexp_loc = loc; _}, _); _}]] ->
        let cst = match cst with
          | Pconst_integer(i, _) -> Int64.of_string i
          | _ ->
            loc_err loc "invalid id"
        in
        Some cst
      | _ ->
        None
    in
    (f, id)
  | {pcd_loc = loc; _} ->
    loc_err loc "invalid cenum variant"

let get_len = function
  | [ ({txt = "len"; loc},
       PStr
         [{pstr_desc =
             Pstr_eval ({pexp_desc = Pexp_constant (Pconst_integer (sz, None)); _}, _)
          ; _}])]
    ->
    let n = int_of_string sz in
    if n > 0 then
      Some n
    else
      loc_err loc "[@len] argument should be > 0"
  | [{txt = "len"; loc}, _ ] ->
    loc_err loc "[@len] argument should be an integer"
  | _ ->
    None

let constr_field {pld_name = fname; pld_type = fty; pld_loc = loc; pld_attributes = att; _} =
  let sz = match get_len fty.ptyp_attributes, get_len att with
  | Some sz, None
  | None, Some sz -> Some sz
  | Some _, Some _ -> loc_err loc "multiple field length attribute"
  | None, None -> None
  in
  let fty = match fty.ptyp_desc with
    | Ptyp_constr ({txt = Lident fty; _}, []) -> fty
    | _ ->
      loc_err fty.ptyp_loc "type identifier expected"
  in
  parse_field loc fname.txt fty sz

let cstruct decl =
  let {ptype_name = name; ptype_kind = kind;
       ptype_attributes = attrs; ptype_loc = loc; _} = decl in
  let fields = match kind with
    | Ptype_record fields -> List.map constr_field fields
    | _ -> loc_err loc "record type declaration expected"
  in
  let endian = match attrs with
    | [{txt = endian; _}, PStr []] -> endian
    | [_, _] -> loc_err loc "no attribute payload expected"
    | _ -> loc_err loc "too many attributes"
  in
  create_struct loc endian name fields

let cenum decl =
  let {ptype_name = name; ptype_kind = kind;
       ptype_attributes = attrs; ptype_loc = loc; _} = decl in
  let fields = match kind with
    | Ptype_variant fields -> fields
    | _ ->
      loc_err loc "expected variant type"
  in
  let width, sexp =
    match attrs with
    | ({txt = width; _}, PStr []) :: ({txt = "sexp"; _}, PStr []) :: [] ->
      width, true
    | ({txt = width; _}, PStr []) :: [] ->
      width, false
    | _ ->
      loc_err loc "invalid cenum attributes"
  in
  let n = ref Int64.minus_one in
  let incr_n () = n := Int64.succ !n in
  let fields = List.map constr_enum fields in
  let fields =
    List.map (function
        | (f, None)   -> incr_n (); (f, !n)
        | (f, Some i) -> n := i; (f, i)
      ) fields in
  let prim = match ty_of_string width with
    | None -> loc_err loc "enum: unknown width specifier %s" width
    | Some p -> p
  in
  { name;
    fields;
    prim;
    sexp;
  }

let signature_item' mapper = function
  | {psig_desc =
       Psig_extension (({txt = "cstruct"; _}, PStr [{pstr_desc = Pstr_type(_, [decl]); _}]), _);
     psig_loc = loc} ->
    output_struct_sig loc (cstruct decl)
  | {psig_desc =
       Psig_extension (({txt = "cenum"; _}, PStr [{pstr_desc = Pstr_type(_, [decl]); _}]), _);
     psig_loc = loc} ->
    output_enum_sig loc (cenum decl)
  | other ->
    [default_mapper.signature_item mapper other]

let signature mapper s =
  List.concat (List.map (signature_item' mapper) s)

let structure_item' mapper = function
  | {pstr_desc =
       Pstr_extension (({txt = "cstruct"; _}, PStr [{pstr_desc = Pstr_type(_, [decl]); _}]), _);
     pstr_loc = loc} ->
    output_struct loc (cstruct decl)
  | {pstr_desc =
       Pstr_extension (({txt = "cenum"; _}, PStr [{pstr_desc = Pstr_type(_, [decl]); _}]), _);
     _ } ->
    output_enum (cenum decl)
  | other ->
    [default_mapper.structure_item mapper other]

let structure mapper s =
  List.concat (List.map (structure_item' mapper) s)

let () =
  Driver.register ~name:"ppx_cstruct" Versions.ocaml_404
    (fun _config _cookies -> {default_mapper with structure; signature})
