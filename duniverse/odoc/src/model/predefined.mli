(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
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

open Paths

(** {3 Identifiers} *)

val bool_identifier : Identifier.Type.t
val int_identifier : Identifier.Type.t
val char_identifier : Identifier.Type.t
val bytes_identifier : Identifier.Type.t
val string_identifier : Identifier.Type.t
val float_identifier : Identifier.Type.t
val unit_identifier : Identifier.Type.t
val exn_identifier : Identifier.Type.t
val array_identifier : Identifier.Type.t
val list_identifier : Identifier.Type.t
val option_identifier : Identifier.Type.t
val int32_identifier : Identifier.Type.t
val int64_identifier : Identifier.Type.t
val nativeint_identifier : Identifier.Type.t
val lazy_t_identifier : Identifier.Type.t
val extension_constructor_identifier : Identifier.Type.t

val false_identifier : Identifier.Constructor.t
val true_identifier : Identifier.Constructor.t
val void_identifier : Identifier.Constructor.t
val nil_identifier : Identifier.Constructor.t
val cons_identifier : Identifier.Constructor.t
val none_identifier : Identifier.Constructor.t
val some_identifier : Identifier.Constructor.t

val match_failure_identifier : Identifier.Exception.t
val assert_failure_identifier : Identifier.Exception.t
val invalid_argument_identifier : Identifier.Exception.t
val failure_identifier : Identifier.Exception.t
val not_found_identifier : Identifier.Exception.t
val out_of_memory_identifier : Identifier.Exception.t
val stack_overflow_identifier : Identifier.Exception.t
val sys_error_identifier : Identifier.Exception.t
val end_of_file_identifier : Identifier.Exception.t
val division_by_zero_identifier : Identifier.Exception.t
val sys_blocked_io_identifier : Identifier.Exception.t
val undefined_recursive_module_identifier : Identifier.Exception.t

val core_type_identifier : string -> Identifier.Type.t option
val core_exception_identifier : string -> Identifier.Exception.t option
val core_constructor_identifier : string -> Identifier.Constructor.t option

(** {3 Paths} *)

val bool_path : Path.Type.t
val int_path : Path.Type.t
val char_path : Path.Type.t
val bytes_path : Path.Type.t
val string_path : Path.Type.t
val float_path : Path.Type.t
val unit_path : Path.Type.t
val exn_path : Path.Type.t
val array_path : Path.Type.t
val list_path : Path.Type.t
val option_path : Path.Type.t
val int32_path : Path.Type.t
val int64_path : Path.Type.t
val nativeint_path : Path.Type.t
val lazy_t_path : Path.Type.t
val extension_constructor_path : Path.Type.t

(** {3 References} *)

val bool_reference : Reference.Type.t
val int_reference : Reference.Type.t
val char_reference : Reference.Type.t
val bytes_reference : Reference.Type.t
val string_reference : Reference.Type.t
val float_reference : Reference.Type.t
val unit_reference : Reference.Type.t
val exn_reference : Reference.Type.t
val array_reference : Reference.Type.t
val list_reference : Reference.Type.t
val option_reference : Reference.Type.t
val int32_reference : Reference.Type.t
val int64_reference : Reference.Type.t
val nativeint_reference : Reference.Type.t
val lazy_t_reference : Reference.Type.t
val extension_constructor_reference : Reference.Type.t

val false_reference : Reference.Constructor.t
val true_reference : Reference.Constructor.t
val void_reference : Reference.Constructor.t
val nil_reference : Reference.Constructor.t
val cons_reference : Reference.Constructor.t
val none_reference : Reference.Constructor.t
val some_reference : Reference.Constructor.t

val match_failure_reference : Reference.Exception.t
val assert_failure_reference : Reference.Exception.t
val invalid_argument_reference : Reference.Exception.t
val failure_reference : Reference.Exception.t
val not_found_reference : Reference.Exception.t
val out_of_memory_reference : Reference.Exception.t
val stack_overflow_reference : Reference.Exception.t
val sys_error_reference : Reference.Exception.t
val end_of_file_reference : Reference.Exception.t
val division_by_zero_reference : Reference.Exception.t
val sys_blocked_io_reference : Reference.Exception.t
val undefined_recursive_module_reference : Reference.Exception.t

(** {3 Declarations} *)

val int_decl : Lang.TypeDecl.t
val char_decl : Lang.TypeDecl.t
val bytes_decl : Lang.TypeDecl.t
val string_decl : Lang.TypeDecl.t
val float_decl : Lang.TypeDecl.t
val bool_decl : Lang.TypeDecl.t
val unit_decl : Lang.TypeDecl.t
val exn_decl : Lang.TypeDecl.t
val array_decl : Lang.TypeDecl.t
val list_decl : Lang.TypeDecl.t
val option_decl : Lang.TypeDecl.t
val int32_decl : Lang.TypeDecl.t
val int64_decl : Lang.TypeDecl.t
val nativeint_decl : Lang.TypeDecl.t
val lazy_t_decl : Lang.TypeDecl.t
val extension_constructor_decl : Lang.TypeDecl.t

val match_failure_decl : Lang.Exception.t
val assert_failure_decl : Lang.Exception.t
val invalid_argument_decl : Lang.Exception.t
val failure_decl : Lang.Exception.t
val not_found_decl : Lang.Exception.t
val out_of_memory_decl : Lang.Exception.t
val stack_overflow_decl : Lang.Exception.t
val sys_error_decl : Lang.Exception.t
val end_of_file_decl : Lang.Exception.t
val division_by_zero_decl : Lang.Exception.t
val sys_blocked_io_decl : Lang.Exception.t
val undefined_recursive_module_decl : Lang.Exception.t

val core_types : Lang.TypeDecl.t list
val core_exceptions : Lang.Exception.t list
