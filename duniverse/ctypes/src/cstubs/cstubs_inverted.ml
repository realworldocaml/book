(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* Cstubs_inverted public interface. *)

[@@@warning "-9-27"]

module type INTERNAL =
sig
  val enum : (string * int64) list -> 'a Ctypes.typ -> unit
  val structure : _ Ctypes.structure Ctypes.typ -> unit
  val union : _ Ctypes.union Ctypes.typ -> unit
  val typedef : _ Ctypes.typ -> string -> unit

  val internal : ?runtime_lock:bool -> string -> ('a -> 'b) Ctypes.fn -> ('a -> 'b) -> unit
end

module type BINDINGS = functor (F : INTERNAL) -> sig end

type fn_meta = {
  fn_runtime_lock : bool;
  fn_name         : string;
}
type fn_info = Fn : fn_meta * (_ -> _) Ctypes.fn -> fn_info
type ty = Ty : _ Ctypes.typ -> ty
type typedef = Typedef : _ Ctypes.typ * string -> typedef
type enum = Enum : (string * int64) list * _ Ctypes.typ -> enum
type decl =
    Decl_fn of fn_info
  | Decl_ty of ty
  | Decl_typedef of typedef
  | Decl_enum of enum

let functions decls =
  List.concat (List.map (function Decl_fn fn -> [fn] | _ -> []) decls)

let collector () : (module INTERNAL) * (unit -> decl list) =
  let decls = ref [] in
  let push d = decls := d :: !decls in
  ((module
    struct
      let enum constants typ = push (Decl_enum (Enum (constants, typ)))
      let structure typ = push (Decl_ty (Ty typ))
      let union typ = push (Decl_ty (Ty typ))
      let typedef typ name = push (Decl_typedef (Typedef (typ, name)))
      let internal ?(runtime_lock=false) name fn _ =
        let meta = { fn_runtime_lock = runtime_lock; fn_name = name } in
        push (Decl_fn ((Fn (meta, fn))))
    end),
   (fun () -> List.rev !decls))

let format_enum_values fmt infos =
  List.iter (fun (Fn ({fn_name}, _)) -> Format.fprintf fmt "@[fn_%s,@]@ " fn_name) infos

let c_prologue fmt register infos =
  Format.fprintf fmt "#include <caml/memory.h>@\n";
  Format.fprintf fmt "#include <caml/callback.h>@\n";
  Format.fprintf fmt "#include \"ctypes_cstubs_internals.h\"@\n@\n";
  Format.fprintf fmt "enum functions@\n{@[<v 2>@ %afn_count@]@\n};"
    format_enum_values infos;
  Format.fprintf fmt "@\n
/* A table of OCaml \"callbacks\". */
static value functions[fn_count];

/* Record a value in the callback table. */
value %s(value i, value v)
{
  CAMLparam2(i, v);
  functions[Long_val(i)] = v;
  caml_register_global_root(&functions[Long_val(i)]);
  CAMLreturn (Val_unit);
}@\n" register

let c_function fmt (Fn ({fn_name; fn_runtime_lock}, fn)) : unit =
  Cstubs_generate_c.inverse_fn ~stub_name:fn_name ~runtime_lock:fn_runtime_lock fmt fn

let gen_c fmt register infos =
  begin
    c_prologue fmt register infos;
    List.iter (c_function fmt) infos
  end

let c_declaration fmt (Fn ({fn_name; fn_runtime_lock}, fn)) : unit =
  Cstubs_generate_c.inverse_fn_decl ~stub_name:fn_name fmt fn

let write_structure_declaration fmt (Ty ty) =
  Format.fprintf fmt "@[%a@];@\n@\n" (fun ty -> Ctypes.format_typ ty) ty

let write_enum_declaration fmt (Enum (constants, ty)) =
  Format.fprintf fmt "@[%a@ {@\n@[<v 2>@\n" (fun ty -> Ctypes.format_typ ty) ty;
  let last = List.length constants - 1 in
  List.iteri
    (fun i (name, value) ->
       (* Trailing commas are not allowed. *)
       if i < last
       then Format.fprintf fmt "@[%s@ =@ %Ld,@]@\n" name value
       else Format.fprintf fmt "@[%s@ =@ %Ld@]@\n" name value)
    constants;
  Format.fprintf fmt "@]@]@\n};@\n@\n"

let write_typedef fmt (Typedef (ty, name)) =
  let write_name _ fmt = Format.fprintf fmt "@ %s" name in
  Format.fprintf fmt "@[typedef@ @[";
  Ctypes_type_printing.format_typ' ty write_name `nonarray fmt;
  Format.fprintf fmt "@]@];@\n@\n"

let write_declaration fmt = function
    Decl_fn f -> c_declaration fmt f
  | Decl_ty s -> write_structure_declaration fmt s
  | Decl_typedef t -> write_typedef fmt t
  | Decl_enum e -> write_enum_declaration fmt e

let write_c fmt ~prefix (module B : BINDINGS) : unit =
  let register = prefix ^ "_register" in
  let m, decls = collector () in
  let module M = B((val m)) in
  gen_c fmt register (functions (decls ()));
  Format.fprintf fmt "@."

let write_c_header fmt ~prefix (module B : BINDINGS) : unit =
  let m, decls = collector () in
  let module M = B((val m)) in
  List.iter (write_declaration fmt) (decls ());
  Format.fprintf fmt "@."

let gen_ml fmt register (infos : fn_info list) : unit =
  Format.fprintf fmt
    "type 'a fn = 'a@\n@\n";
  Format.fprintf fmt
    "module CI = Cstubs_internals@\n@\n";
  Format.fprintf fmt "type 'a f = 'a CI.fn =@\n";
  Format.fprintf fmt " | Returns  : 'a CI.typ   -> 'a f@\n";
  Format.fprintf fmt " | Function : 'a CI.typ * 'b f  -> ('a -> 'b) f@\n";
  Format.fprintf fmt
    "type 'a name = @\n";
  ListLabels.iter infos
    ~f:(fun (Fn ({fn_name}, fn)) ->
        Cstubs_generate_ml.constructor_decl ~concurrency:`Sequential
          ~errno:`Ignore_errno
          (Printf.sprintf "Fn_%s" fn_name) fn fmt);
  Format.fprintf fmt
    "@\n";
  Format.fprintf fmt
    "@[<h>external register_value : 'a name -> 'a fn -> unit =@\n@ @ \"%s\"@]@\n@\n"
    register;
  Format.fprintf fmt
    "@[<h>let internal : ";
  Format.fprintf fmt
    "@[type a b.@ @[?runtime_lock:bool -> string -> (a -> b) Ctypes.fn -> (a -> b) -> unit@]@]@ =@\n";
  Format.fprintf fmt
    "fun ?runtime_lock name fn f -> match fn, name with@\n@[";
  ListLabels.iter infos
    ~f:(fun (Fn ({fn_name}, fn)) ->
      Cstubs_generate_ml.inverse_case ~register_name:"register_value"
        ~constructor:(Printf.sprintf "Fn_%s" fn_name) fn_name fmt fn);
  Format.fprintf fmt
    "| _ -> failwith (\"Linking mismatch on name: \" ^ name)@]@]@]@\n@\n";
  Format.fprintf fmt
    "let enum _ _ = () and structure _ = () and union _ = () and typedef _ _ = ()@."

let write_ml fmt ~prefix (module B : BINDINGS) : unit =
  let register = prefix ^ "_register" in
  let m, decls = collector () in
  let module M = B((val m)) in
  gen_ml fmt register (functions (decls ()))
