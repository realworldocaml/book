(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

[@@@warning "-9-27"]

open Ctypes_static
open Ctypes_memory

let rec format : type a. a typ -> Format.formatter -> a -> unit
  = fun typ fmt v -> match typ with
    Void -> Format.pp_print_string fmt ""
  | Primitive p ->
    Format.pp_print_string fmt (Ctypes_value_printing_stubs.string_of_prim p v)
  | Pointer _ -> format_ptr fmt v
  | Funptr _ -> format_funptr fmt v
  | Struct _ -> format_structured fmt v
  | Union _ -> format_structured fmt v
  | Array (a, n) -> format_array fmt v
  | Bigarray ba -> Format.fprintf fmt "<bigarray %a>"
    (fun fmt -> Ctypes_type_printing.format_typ fmt) typ
  | Abstract _ -> format_structured fmt v
  | OCaml _ -> format_ocaml fmt v
  | View {write; ty; format=f} ->
    begin match f with
      | None -> format ty fmt (write v)
      | Some f -> f fmt v
    end
and format_structured : type a b. Format.formatter -> (a, b) structured -> unit
  = fun fmt ({structured = CPointer p} as s) ->
    let open Format in
    match Ctypes_ptr.Fat.reftype p with
    | Struct {fields} ->
      fprintf fmt "{@;<1 2>@[";
      format_fields "," fields fmt s;
      fprintf fmt "@]@;<1 0>}"
    | Union {ufields} ->
      fprintf fmt "{@;<1 2>@[";
      format_fields " |" ufields fmt s;
      fprintf fmt "@]@;<1 0>}"
    | Abstract abs ->
      pp_print_string fmt "<abstract>"
    | _ -> raise (Unsupported "unknown structured type")
and format_array : type a. Format.formatter -> a carray -> unit
  = fun fmt ({astart = CPointer p; alength} as arr) ->
    let open Format in
    fprintf fmt "{@;<1 2>@[";
    for i = 0 to alength - 1 do
      format (Ctypes_ptr.Fat.reftype p) fmt (CArray.get arr i);
      if i <> alength - 1 then
        fprintf fmt ",@;"
    done;
    fprintf fmt "@]@;<1 0>}"
and format_ocaml : type a. Format.formatter -> a ocaml -> unit =
  let offset fmt = function
    | 0 -> ()
    | n -> Format.fprintf fmt "@ @[[offset:%d]@]" n
  and float_array fmt arr =
    Format.fprintf fmt "[|@;<1 2>@[";
    let len = Array.length arr in
    for i = 0 to len - 1 do
      Format.pp_print_float fmt arr.(i);
      if i <> len - 1 then
        Format.fprintf fmt ",@;"
    done;
    Format.fprintf fmt "@]@;<1 0>|]"
  in
  fun fmt (OCamlRef (off, obj, ty)) -> match ty with
  | String -> Format.fprintf fmt "%S%a" obj offset off
  | Bytes -> Format.fprintf fmt "%S%a" (Bytes.to_string obj) offset off
  | FloatArray -> Format.fprintf fmt "%a%a" float_array obj offset off
and format_fields : type a b. string -> (a, b) structured boxed_field list ->
                              Format.formatter -> (a, b) structured -> unit
  = fun sep fields fmt s ->
    let last_field = List.length fields - 1 in
    let open Format in
    List.iteri
      (fun i (BoxedField ({ftype; foffset; fname} as f)) ->
        fprintf fmt "@[%s@] = @[%a@]%s@;" fname (format ftype) (getf s f)
          (if i <> last_field then sep else ""))
      fields
and format_ptr : type a. Format.formatter -> a ptr -> unit
  = fun fmt (CPointer p) ->
    Format.fprintf fmt "%s" (Ctypes_value_printing_stubs.string_of_pointer p)
and format_funptr  : type a. Format.formatter -> a static_funptr -> unit
  = fun fmt (Static_funptr p) ->
    Format.fprintf fmt "%s" (Ctypes_value_printing_stubs.string_of_pointer p)

let string_of typ v = Format.asprintf "%a" (format typ) v
