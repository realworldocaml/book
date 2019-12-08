(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

(* C pretty printing. *)

[@@@warning "-9-27"]

open Ctypes_static
open Cstubs_c_language
open Format

let format_seq lbr fmt_item sep rbr fmt items =
  let open Format in
  fprintf fmt "%s@[@[" lbr;
    ListLabels.iteri items ~f:(fun i item ->
      if i <> 0 then fprintf fmt "@]%s@ @[" sep;
      fmt_item fmt item);
  fprintf fmt "@]%s@]" rbr

let format_ty fmt (Ty ty) = Ctypes.format_typ fmt ty

let cvar_name = function
  | `Local (name, _) | `Global { name } -> name

let cvar fmt v = fprintf fmt "%s" (cvar_name v)

let cconst fmt (`Int i) = fprintf fmt "%s" (Signed.SInt.to_string i)

let rec camlxParam fmt args =
  match args with
    [] -> ()
  | x1 :: [] ->
    fprintf fmt "@[CAMLxparam1@;(%s)@];" x1
  | x1 :: x2 :: [] ->
    fprintf fmt "@[CAMLxparam2@;(%s,@;%s)@];" x1 x2
  | x1 :: x2 :: x3 :: [] ->
    fprintf fmt "@[CAMLxparam3@;(%s,@;%s,@;%s)@];" x1 x2 x3
  | x1 :: x2 :: x3 :: x4 :: [] ->
    fprintf fmt "@[CAMLxparam4@;(%s,@;%s,@;%s,@;%s)@];" x1 x2 x3 x4
  | x1 :: x2 :: x3 :: x4 :: x5 :: rest ->
    fprintf fmt "@[CAMLxparam5@;(%s,@;%s,@;%s,@;%s,@;%s)@];" x1 x2 x3 x4 x5;
    camlxParam fmt rest

let camlParam fmt args =
  match args with
    [] ->
    fprintf fmt "@[CAMLparam0@;()@];"
  | x1 :: [] ->
    fprintf fmt "@[CAMLparam1@;(%s)@];" x1
  | x1 :: x2 :: [] ->
    fprintf fmt "@[CAMLparam2@;(%s,@;%s)@];" x1 x2
  | x1 :: x2 :: x3 :: [] ->
    fprintf fmt "@[CAMLparam3@;(%s,@;%s,@;%s)@];" x1 x2 x3
  | x1 :: x2 :: x3 :: x4 :: [] ->
    fprintf fmt "@[CAMLparam4@;(%s,@;%s,@;%s,@;%s)@];" x1 x2 x3 x4
  | x1 :: x2 :: x3 :: x4 :: x5 :: rest ->
    fprintf fmt "@[CAMLparam5@;(%s,@;%s,@;%s,@;%s,@;%s)@];@ %a" x1 x2 x3 x4 x5
      camlxParam rest

(* Determine whether the C expression [(ty)e] is equivalent to [e] *)
let cast_unnecessary : ty -> cexp -> bool =
  let rec harmless l r = match l, r with
  | Ty (Pointer Void), Ty (Pointer _) -> true
  | Ty (View { ty }), t -> harmless (Ty ty) t
  | t, Ty (View { ty }) -> harmless t (Ty ty)
  | (Ty (Primitive _) as l), (Ty (Primitive _) as r) -> l = r
  | _ -> false
  in
  fun ty e -> harmless ty (Type_C.cexp e)

let rec cexp fmt : cexp -> unit = function
  | #cconst as c -> cconst fmt c
  | `Local _ as x -> cvar fmt x
  | `Cast (ty, e) when cast_unnecessary ty e -> cexp fmt e
  | `Cast (ty, e) -> fprintf fmt "@[@[(%a)@]%a@]" format_ty ty cexp e
  | `Addr (`Global { name })
  | `Addr (`Local (name, _)) -> fprintf fmt "@[&@[%s@]@]" name

let rec clvalue fmt : clvalue -> unit = function
  | #cvar as x -> cvar fmt x
  | `Index (lv, i) ->
    fprintf fmt "@[@[%a@]@[[%a]@]@]" clvalue lv cexp i
  | `Field (lv, f) ->
    fprintf fmt "@[@[%a@]@[.%s@]@]" clvalue lv f
  | `PointerField (lv, f) ->
    fprintf fmt "@[@[%a@]@[->%s@]@]" clvalue lv f


let camlop fmt : camlop -> unit = function
  | `CAMLparam0 -> Format.fprintf fmt "CAMLparam0()"
  | `CAMLlocalN (e, c) -> Format.fprintf fmt "CAMLlocalN(@[%a@],@ @[%a@])"
    cexp e cexp c
  | `CAMLdrop ->
    Format.fprintf fmt "caml_local_roots = caml__frame;"
    (* Format.fprintf fmt "CAMLdrop()" *) (* 4.03+ only *)

let rec ceff fmt : ceff -> unit = function
  | #cexp as e -> cexp fmt e
  | #camlop as o -> camlop fmt o
  | `Global _ as x -> cvar fmt x
  | `App ({fname}, es) ->
    fprintf fmt "@[%s(@[" fname;
    let last_exp = List.length es - 1 in
    List.iteri
      (fun i e ->
        fprintf fmt "@[%a@]%(%)" cexp e
          (if i <> last_exp then ",@ " else "" : (_,_,_) format))
      es;
    fprintf fmt ")@]@]";
  | `Index (e, i) ->
    fprintf fmt "@[@[%a@]@[[%a]@]@]" ceff e cexp i
  | `Deref e -> fprintf fmt "@[*@[%a@]@]" cexp e
  | `DerefField (e, f) -> fprintf fmt "@[@[%a@]->%s@]" cexp e f

let rec ccomp fmt : ccomp -> unit = function
  | #cexp as e when Type_C.cexp e = Ty Void ->
    fprintf fmt "@[return@];"
  | #cexp as e ->
    fprintf fmt "@[<2>return@;@[%a@]@];" cexp e
  | #ceff as e -> fprintf fmt "@[<2>return@;@[%a@]@];" ceff e
  | `CAMLparam (xs, c) ->
    fprintf fmt "@[%a;@]@ %a" camlParam xs ccomp c
  | `Return (Ty Void, _) ->
    fprintf fmt "@[return@];"
  | `Return (Ty ty, e) ->
    fprintf fmt "@[<2>return@;@[%a@]@];" cexp e
  | `CAMLreturnT (Ty Void, _) ->
    fprintf fmt "@[CAMLreturn0@];"
  | `CAMLreturnT (Ty ty, e) ->
    fprintf fmt "@[<2>CAMLreturnT(@[%a@],@;@[%a@])@];"
      (fun t -> Ctypes.format_typ t) ty
      cexp e
  | `Let (xe, `Cast (ty, (#cexp as e'))) when cast_unnecessary ty e' ->
    ccomp fmt (`Let (xe, e'))
  | `Let ((`Local (x, _), e), `Local (y, _)) when x = y ->
    ccomp fmt (e :> ccomp)
  | `Let ((`Local (name, Ty Void), e), s) ->
    fprintf fmt "@[%a;@]@ %a" ceff e ccomp s
  | `Let ((`Local (name, Ty (Struct { tag })), e), s) ->
    fprintf fmt "@[struct@;%s@;%s@;=@;@[%a;@]@]@ %a"
      tag name ceff e ccomp s
  | `Let ((`Local (name, Ty (Union { utag })), e), s) ->
    fprintf fmt "@[union@;%s@;%s@;=@;@[%a;@]@]@ %a"
      utag name ceff e ccomp s
  | `Let ((`Local (name, Ty ty), e), s) ->
    fprintf fmt "@[@[%a@]@;=@;@[%a;@]@]@ %a"
      (Ctypes.format_typ ~name) ty ceff e ccomp s
  | `LetConst (`Local (x, _), `Int c, s) ->
    fprintf fmt "@[enum@ {@[@ %s@ =@ %s@ };@]@]@ %a"
      x (Signed.SInt.to_string c) ccomp s
  | `LetAssign (lv, e, c) ->
    fprintf fmt "@[@[%a@]@;=@;@[%a@];@]@ %a" clvalue lv ceff e ccomp c

let format_parameter_list parameters k fmt =
  let format_arg fmt (name, Ty t) =
      Ctypes_type_printing.format_typ ~name fmt t
  in
  match parameters with
  | [] ->
    Format.fprintf fmt "%t(void)" k
  | [(_, Ty Void)] -> Format.fprintf fmt "@[%t@[(void)@]@]" k
  | _ ->
    Format.fprintf fmt "@[%t@[%a@]@]" k
      (format_seq "(" format_arg "," ")")
      parameters

let cfundec : Format.formatter -> cfundec -> unit =
  fun fmt (`Fundec (name, args, Ty return)) ->
    Ctypes_type_printing.format_typ' return
      (fun context fmt ->
        format_parameter_list args (Ctypes_type_printing.format_name ~name) fmt)
      `nonarray fmt

let storage_class fmt = function
    `Static -> fprintf fmt "static@\n"
  | `Extern -> ()

let cfundef fmt (`Function (dec, body, sc) : cfundef) =
  storage_class fmt sc;
  fprintf fmt "%a@\n{@[<v 2>@\n%a@]@\n}@\n"
    cfundec dec ccomp body
