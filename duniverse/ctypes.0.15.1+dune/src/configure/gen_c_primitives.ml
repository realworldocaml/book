[@@@warning "-9"]

module C = Configurator.V1

let header ="\
(*
 * Copyright (c) 2016 whitequark
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open Ctypes_primitive_types
"

type c_format =
| No_format
| Known_format of string
| Defined_format of string

type c_primitive = {
  constructor : string;
  typ         : string;
  format      : c_format;
  size        : string;
  alignment   : string;
}

let c_primitive constructor typ format =
  { constructor; typ; format;
    size = "sizeof("^typ^")";
    alignment = "alignof("^typ^")"; }

let c_primitives = [
  c_primitive "Char"      "char"                (Known_format "d");
  c_primitive "Schar"     "signed char"         (Known_format "d");
  c_primitive "Uchar"     "unsigned char"       (Known_format "d");
  c_primitive "Bool"      "bool"                (Known_format "d");
  c_primitive "Short"     "short"               (Known_format "hd");
  c_primitive "Int"       "int"                 (Known_format "d");
  c_primitive "Long"      "long"                (Known_format "ld");
  c_primitive "Llong"     "long long"           (Known_format "lld");
  c_primitive "Ushort"    "unsigned short"      (Known_format "hu");
  c_primitive "Sint"      "int"                 (Known_format "d");
  c_primitive "Uint"      "unsigned int"        (Known_format "u");
  c_primitive "Ulong"     "unsigned long"       (Known_format "lu");
  c_primitive "Ullong"    "unsigned long long"  (Known_format "llu");
  c_primitive "Size_t"    "size_t"              (Known_format "zu");
  c_primitive "Int8_t"    "int8_t"              (Defined_format "PRId8");
  c_primitive "Int16_t"   "int16_t"             (Defined_format "PRId16");
  c_primitive "Int32_t"   "int32_t"             (Defined_format "PRId32");
  c_primitive "Int64_t"   "int64_t"             (Defined_format "PRId64");
  c_primitive "Uint8_t"   "uint8_t"             (Defined_format "PRIu8");
  c_primitive "Uint16_t"  "uint16_t"            (Defined_format "PRIu16");
  c_primitive "Uint32_t"  "uint32_t"            (Defined_format "PRIu32");
  c_primitive "Uint64_t"  "uint64_t"            (Defined_format "PRIu64");
  c_primitive "Float"     "float"               (Known_format ".12g");
  c_primitive "Double"    "double"              (Known_format ".12g");
  c_primitive "LDouble"   "long double"         (Known_format ".12Lg");
  c_primitive "Complex32" "float _Complex"      (No_format);
  c_primitive "Complex64" "double _Complex"     (No_format);
  c_primitive "Complexld" "long double _Complex"(No_format);
  c_primitive "Nativeint" "intnat"              (Defined_format "REAL_ARCH_INTNAT_PRINTF_FORMAT \"d\"");
  { constructor = "Camlint";
    typ         = "intnat";
    format      = Defined_format "REAL_ARCH_INTNAT_PRINTF_FORMAT \"d\"";
    size        = "sizeof(intnat)";
    alignment   = "alignof(intnat)" };
]

open Printf

let generate name typ f =
  printf "let %s : type a. a prim -> %s = function\n" name typ;
  List.iter (fun c_primitive ->
    printf " | %s -> " c_primitive.constructor;
    begin try
      f c_primitive
    with Not_found ->
      failwith (name^": "^c_primitive.constructor)
    end;
    printf "\n") c_primitives

let prelude = "\
#if defined(__MINGW32__) || defined(__MINGW64__)
#define __USE_MINGW_ANSI_STDIO 1
#include <stdio.h> /* see: https://sourceforge.net/p/mingw-w64/bugs/627/ */
#endif

#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include <caml/mlvalues.h>

#define alignof(T) (offsetof(struct { char c; T t; }, t))
#define STRINGIFY1(x) #x
#define STRINGIFY(x) STRINGIFY1(x)

#if __USE_MINGW_ANSI_STDIO && defined(__MINGW64__)
#define REAL_ARCH_INTNAT_PRINTF_FORMAT \"ll\"
#else
#define REAL_ARCH_INTNAT_PRINTF_FORMAT ARCH_INTNAT_PRINTF_FORMAT
#endif
"
let includes = []

let () =
  C.main ~name:"ctypes" (fun c ->
    let import_int l =
      match C.C_define.(import c ~prelude ~includes [l,Type.Int]) with
      |[_,C.C_define.Value.Int i] -> i
      |_ -> failwith ("unable to find integer definition for " ^ l) in
    let import_string l  =
      match C.C_define.(import c ~prelude ~includes [l,Type.String]) with
      |[_,C.C_define.Value.String s] -> s
      |_ -> failwith ("unable to find string definition for " ^ l) in
    print_string header;
    generate "sizeof" "int" (fun { size } ->
      printf "%d" (import_int size));
    generate "alignment" "int" (fun { alignment } ->
      printf "%d" (import_int alignment));
    generate "name" "string" (fun { typ } ->
      printf "%S" (import_string ("STRINGIFY("^typ^")")));
    generate "format_string" "string option" (fun { format } ->
      match format with
      | Known_format str ->
        printf "Some %S" ("%"^str)
      | Defined_format str ->
        printf "Some %S" ("%"^(import_string str))
      | No_format ->
        printf "None");
    printf "let pointer_size = %d\n" (import_int "sizeof(void*)");
    printf "let pointer_alignment = %d\n" (import_int "alignof(void*)");
  )
