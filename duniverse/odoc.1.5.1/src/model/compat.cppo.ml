(*
 * Copyright (c) 2019 Jon Ludlam <jon@recoil.org>
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
 
(* Compatibility for older versions of OCaml *)

(* This module contains a subset of the types in ocaml.git/typing/types.ml from
   the latest version of the compiler. There is also conditionally compiled code
   for older versions of the compiler to convert from their version of types.ml
   to this version. This simplifies the support for older versions of OCaml.

   This is only done for the subsets of the types that contain the most invasive
   changes. For other simpler changes we use in-line cppo directives *)

type visibility =
  | Exported
  | Hidden

type module_type =
    Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of functor_parameter * module_type
  | Mty_alias of Path.t

and functor_parameter =
  | Unit
  | Named of Ident.t option * module_type

and module_presence =
  | Mp_present
  | Mp_absent

and signature = signature_item list

and signature_item =
    Sig_value of Ident.t * Types.value_description * visibility
  | Sig_type of Ident.t * Types.type_declaration * Types.rec_status * visibility
  | Sig_typext of Ident.t * Types.extension_constructor * Types.ext_status * visibility
  | Sig_module of
      Ident.t * module_presence * module_declaration * Types.rec_status * visibility
  | Sig_modtype of Ident.t * modtype_declaration * visibility
  | Sig_class of Ident.t * Types.class_declaration * Types.rec_status * visibility
  | Sig_class_type of Ident.t * Types.class_type_declaration * Types.rec_status * visibility

and module_declaration =
  {
    md_type: module_type;
    md_attributes: Parsetree.attributes;
    md_loc: Location.t;
  }

and modtype_declaration =
  {
    mtd_type: module_type option;  (* Note: abstract *)
    mtd_attributes: Parsetree.attributes;
    mtd_loc: Location.t;
  }


let opt conv = function | None -> None | Some x -> Some (conv x)

#if OCAML_MAJOR = 4 && OCAML_MINOR >= 10

let rec signature : Types.signature -> signature = fun x -> List.map signature_item x

and signature_item : Types.signature_item -> signature_item = function
  | Types.Sig_value (a,b,c) -> Sig_value (a,b,visibility c)
  | Types.Sig_type (a,b,c,d) -> Sig_type (a,b,c, visibility d)
  | Types.Sig_typext (a,b,c,d) -> Sig_typext (a,b,c,visibility d)
  | Types.Sig_module (a,b,c,d,e) -> Sig_module (a, module_presence b, module_declaration c, d, visibility e)
  | Types.Sig_modtype (a,b,c) -> Sig_modtype (a, modtype_declaration b, visibility c)
  | Types.Sig_class (a,b,c,d) -> Sig_class (a,b,c, visibility d)
  | Types.Sig_class_type (a,b,c,d) -> Sig_class_type (a,b,c, visibility d)

and visibility : Types.visibility -> visibility = function
  | Types.Hidden -> Hidden
  | Types.Exported -> Exported

and module_type : Types.module_type -> module_type = function
  | Types.Mty_ident p -> Mty_ident p
  | Types.Mty_signature s -> Mty_signature (signature s)
  | Types.Mty_functor (a, b) -> Mty_functor(functor_parameter a, module_type b)
  | Types.Mty_alias p -> Mty_alias p

and functor_parameter : Types.functor_parameter -> functor_parameter = function
  | Types.Unit -> Unit
  | Types.Named (a,b) -> Named (a, module_type b)

and module_presence : Types.module_presence -> module_presence = function
  | Types.Mp_present -> Mp_present
  | Types.Mp_absent -> Mp_absent

and module_declaration : Types.module_declaration -> module_declaration = fun x ->
  { md_type = module_type x.Types.md_type;
    md_attributes = x.md_attributes;
    md_loc = x.md_loc }

and modtype_declaration : Types.modtype_declaration -> modtype_declaration = fun x ->
  { mtd_type = opt module_type x.Types.mtd_type;
    mtd_attributes = x.Types.mtd_attributes;
    mtd_loc = x.Types.mtd_loc }

#elif OCAML_MAJOR = 4 && OCAML_MINOR >= 08

let rec signature : Types.signature -> signature = fun x -> List.map signature_item x

and signature_item : Types.signature_item -> signature_item = function
  | Types.Sig_value (a,b,c) -> Sig_value (a,b,visibility c)
  | Types.Sig_type (a,b,c,d) -> Sig_type (a,b,c, visibility d)
  | Types.Sig_typext (a,b,c,d) -> Sig_typext (a,b,c,visibility d)
  | Types.Sig_module (a,b,c,d,e) -> Sig_module (a, module_presence b, module_declaration c, d, visibility e)
  | Types.Sig_modtype (a,b,c) -> Sig_modtype (a, modtype_declaration b, visibility c)
  | Types.Sig_class (a,b,c,d) -> Sig_class (a,b,c, visibility d)
  | Types.Sig_class_type (a,b,c,d) -> Sig_class_type (a,b,c, visibility d)

and visibility : Types.visibility -> visibility = function
  | Types.Hidden -> Hidden
  | Types.Exported -> Exported

and module_type : Types.module_type -> module_type = function
  | Types.Mty_ident p -> Mty_ident p
  | Types.Mty_signature s -> Mty_signature (signature s)
  | Types.Mty_functor (a, b, c) -> begin
    match b with
    | Some m -> Mty_functor(Named(Some a,module_type m),module_type c)
    | None -> Mty_functor(Unit,module_type c)
    end
  | Types.Mty_alias p -> Mty_alias p

and module_presence : Types.module_presence -> module_presence = function
  | Types.Mp_present -> Mp_present
  | Types.Mp_absent -> Mp_absent

and module_declaration : Types.module_declaration -> module_declaration = fun x ->
  { md_type = module_type x.Types.md_type;
    md_attributes = x.md_attributes;
    md_loc = x.md_loc }

and modtype_declaration : Types.modtype_declaration -> modtype_declaration = fun x ->
  { mtd_type = opt module_type x.Types.mtd_type;
    mtd_attributes = x.Types.mtd_attributes;
    mtd_loc = x.Types.mtd_loc }

#elif OCAML_MAJOR = 4 && (OCAML_MINOR = 7 || OCAML_MINOR = 6 || OCAML_MINOR = 5 || OCAML_MINOR=4)

  let rec module_type : Types.module_type -> module_type = function
  | Types.Mty_ident p -> Mty_ident p
  | Types.Mty_signature s -> Mty_signature (signature s)
  | Types.Mty_functor (a, b, c) -> begin
    match b with
    | Some m -> Mty_functor(Named(Some a,module_type m),module_type c)
    | None -> Mty_functor(Unit,module_type c)
    end
  | Types.Mty_alias (_,q) -> Mty_alias q

  and signature_item : Types.signature_item -> signature_item = function
  | Types.Sig_value (id, d) -> Sig_value (id, d, Exported)
  | Types.Sig_type (id, td, rec_status) -> Sig_type (id, td, rec_status, Exported)
  | Types.Sig_typext (id, ec, es) -> Sig_typext (id, ec, es, Exported)
  | Types.Sig_module (id, ({md_type = Types.Mty_alias (Types.Mta_present, _); _} as md), rs) -> Sig_module (id, Mp_present, module_declaration md, rs, Exported)
  | Types.Sig_module (id, ({md_type = Types.Mty_alias (Types.Mta_absent, _); _} as md), rs) -> Sig_module (id, Mp_absent, module_declaration md, rs, Exported)
  | Types.Sig_module (id, md, rs) -> Sig_module (id, Mp_present, module_declaration md, rs, Exported)
  | Types.Sig_modtype (id, mtd) -> Sig_modtype (id, modtype_declaration mtd, Exported)
  | Types.Sig_class (id, cd, rs) -> Sig_class (id, cd, rs, Exported)
  | Types.Sig_class_type (id, ctd, rs) -> Sig_class_type (id, ctd, rs, Exported)

  and signature : Types.signature -> signature = fun x -> List.map signature_item x

  and module_declaration : Types.module_declaration -> module_declaration = fun x -> 
    { md_type = module_type x.Types.md_type;
      md_attributes = x.Types.md_attributes;
      md_loc = x.Types.md_loc }
  
  and modtype_declaration : Types.modtype_declaration -> modtype_declaration = fun x ->
    { mtd_type = opt module_type x.Types.mtd_type;
      mtd_attributes = x.Types.mtd_attributes;
      mtd_loc = x.Types.mtd_loc }

#elif OCAML_MAJOR = 4 && (OCAML_MINOR = 2 || OCAML_MINOR = 3)

  let rec module_type : Types.module_type -> module_type = function
  | Types.Mty_ident p -> Mty_ident p
  | Types.Mty_signature s -> Mty_signature (signature s)
  | Types.Mty_functor (a, b, c) -> begin
    match b with
    | Some m -> Mty_functor(Named(Some a,module_type m),module_type c)
    | None -> Mty_functor(Unit,module_type c)
    end
  | Types.Mty_alias q -> Mty_alias q

  and signature_item : Types.signature_item -> signature_item = function
  | Types.Sig_value (id, d) -> Sig_value (id, d, Exported)
  | Types.Sig_type (id, td, rec_status) -> Sig_type (id, td, rec_status, Exported)
  | Types.Sig_typext (id, ec, es) -> Sig_typext (id, ec, es, Exported)
  | Types.Sig_module (id, md, rs) -> Sig_module (id, Mp_present, module_declaration md, rs, Exported)
  | Types.Sig_modtype (id, mtd) -> Sig_modtype (id, modtype_declaration mtd, Exported)
  | Types.Sig_class (id, cd, rs) -> Sig_class (id, cd, rs, Exported)
  | Types.Sig_class_type (id, ctd, rs) -> Sig_class_type (id, ctd, rs, Exported)

  and signature : Types.signature -> signature = fun x -> List.map signature_item x

  and module_declaration : Types.module_declaration -> module_declaration = fun x -> 
    { md_type = module_type x.Types.md_type;
      md_attributes = x.Types.md_attributes;
      md_loc = x.Types.md_loc }
  
  and modtype_declaration : Types.modtype_declaration -> modtype_declaration = fun x ->
    { mtd_type = opt module_type x.Types.mtd_type;
      mtd_attributes = x.Types.mtd_attributes;
      mtd_loc = x.Types.mtd_loc }


#endif
