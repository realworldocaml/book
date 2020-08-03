{
open StdLabels
open Printf

module String = struct
  [@@@warning "-32-3"]
  let capitalize_ascii   = String.capitalize
  let uncapitalize_ascii = String.uncapitalize
  let uppercase_ascii    = String.uppercase
  let lowercase_ascii    = String.lowercase
  include String
end

let deprecated_msg ~is_exn what =
  sprintf
    "[%sdeprecated \"\\\n\
     [2016-09] this element comes from the stdlib distributed with OCaml.\n\
     Referring to the stdlib directly is discouraged by Base. You should either\n\
     use the equivalent functionality offered by Base, or if you really want to\n\
     refer to the stdlib, use Caml.%s instead\"]"
    (if is_exn then "@" else "@@")
    what

let deprecated_msg_no_equivalent ~is_exn what =
  sprintf
    "[%sdeprecated \"\\\n\
     [2016-09] this element comes from the stdlib distributed with OCaml.\n\
     There is not equivalent functionality in Base or Stdio at the moment,\n\
     so you need to use [Caml.%s] instead\"]"
    (if is_exn then "@" else "@@")
    what

let deprecated_msg_with_repl_text ~is_exn text =
  sprintf
    "[%sdeprecated \"\\\n\
     [2016-09] this element comes from the stdlib distributed with OCaml.\n\
     %s.\"]"
    (if is_exn then "@" else "@@")
    text

let deprecated_msg_with_repl ~is_exn repl =
  deprecated_msg_with_repl_text ~is_exn (sprintf "Use [%s] instead" repl)

let deprecated_msg_with_approx_repl ~is_exn ~id repl =
  sprintf
    "[%sdeprecated \"\\\n\
     [2016-09] this element comes from the stdlib distributed with OCaml.\n\
     There is no equivalent functionality in Base or Stdio but you can use\n\
     [%s] instead.\n\
     Alternatively, if you really want to refer the stdlib you can use\n\
     [Caml.%s].\"]"
    (if is_exn then "@" else "@@")
    repl id

type replacement =
  | No_equivalent
  | Repl of string
  | Repl_text of string
  | Approx of string

let val_replacement = function
  | "( != )"              -> Repl "not (phys_equal ...)"
  | "( == )"              -> Repl "phys_equal"
  | "( ** )"              -> Repl "**."
  | "( mod )"             -> Repl_text "Use (%), which has slightly different \
                                        semantics, or Int.rem which is equivalent"
  | "acos"                -> Repl "Float.acos"
  | "asin"                -> Repl "Float.asin"
  | "atan"                -> Repl "Float.atan"
  | "atan2"               -> Repl "Float.atan2"
  | "bool_of_string"      -> Repl "Bool.of_string"
  | "ceil"                -> Repl "Float.round_up"
  | "char_of_int"         -> Repl "Char.of_int_exn"
  | "classify_float"      -> Repl "Float.classify"
  | "close_in"            -> Repl "Stdio.In_channel.close"
  | "close_in_noerr"      -> Repl "Stdio.In_channel.close"
  | "close_out"           -> Repl "Stdio.Out_channel.close"
  | "close_out_noerr"     -> Repl "Stdio.Out_channel.close"
  | "copysign"            -> Repl "Float.copysign"
  | "cos"                 -> Repl "Float.cos"
  | "cosh"                -> Repl "Float.cosh"
  | "decr"                -> Repl "Int.decr"
  | "epsilon_float"       -> Repl "Float.epsilon_float"
  | "exp"                 -> Repl "Float.exp"
  | "expm1"               -> Repl "Float.expm1"
  | "float"               -> Repl "Float.of_int"
  | "float_of_int"        -> Repl "Float.of_int"
  | "float_of_string"     -> Repl "Float.of_string"
  | "floor"               -> Repl "Float.round_down"
  | "flush"               -> Repl "Stdio.Out_channel.flush"
  | "flush_all"           -> No_equivalent
  | "frexp"               -> Repl "Float.frexp"
  | "hypot"               -> Repl "Float.hypot"
  | "in_channel_length"   -> Repl "Stdio.In_channel.length"
  | "infinity"            -> Repl "Float.infinity"
  | "incr"                -> Repl "Int.incr"
  | "input"               -> Repl "Stdio.In_channel.input"
  | "input_binary_int"    -> Repl "Stdio.In_channel.input_binary_int"
  | "input_byte"          -> Repl "Stdio.In_channel.input_byte"
  | "input_char"          -> Repl "Stdio.In_channel.input_char"
  | "input_line"          -> Repl "Stdio.In_channel.input_line"
  | "input_value"         -> Repl "Stdio.In_channel.unsafe_input_value"
  | "int_of_char"         -> Repl "Char.to_int"
  | "int_of_float"        -> Repl "Int.of_float"
  | "int_of_string"       -> Repl "Int.of_string"
  | "ldexp"               -> Repl "Float.ldexp"
  | "log"                 -> Repl "Float.log"
  | "log10"               -> Repl "Float.log10"
  | "log1p"               -> Repl "Float.log1p"
  | "max_float"           -> Repl "Float.max_finite_value"
  | "max_int"             -> Repl "Int.max_value"
  | "min_float"           -> Repl "Float.min_positive_normal_value"
  | "min_int"             -> Repl "Int.min_value"
  | "mod_float"           -> Repl "Float.mod_float"
  | "modf"                -> Repl "Float.modf"
  | "nan"                 -> Repl "Float.nan"
  | "neg_infinity"        -> Repl "Float.neg_infinity"
  | "open_in"             -> Repl "Stdio.In_channel.create"
  | "open_in_bin"         -> Repl "Stdio.In_channel.create"
  | "open_in_gen"         -> No_equivalent
  | "open_out"            -> Repl "Stdio.Out_channel.create"
  | "open_out_bin"        -> Repl "Stdio.Out_channel.create"
  | "open_out_gen"        -> No_equivalent
  | "out_channel_length"  -> Repl "Stdio.Out_channel.length"
  | "output"              -> Repl "Stdio.Out_channel.output"
  | "output_binary_int"   -> Repl "Stdio.Out_channel.output_binary_int"
  | "output_byte"         -> Repl "Stdio.Out_channel.output_byte"
  | "output_bytes"        -> Repl "Stdio.Out_channel.output_bytes"
  | "output_char"         -> Repl "Stdio.Out_channel.output_char"
  | "output_string"       -> Repl "Stdio.Out_channel.output_string"
  | "output_substring"    -> Repl "Stdio.Out_channel.output"
  | "output_value"        -> Repl "Stdio.Out_channel.output_value"
  | "pos_in"              -> Repl "Stdio.In_channel.pos"
  | "pos_out"             -> Repl "Stdio.Out_channel.pos"
  | "pred"                -> Repl "Int.pred"
  | "prerr_bytes"         -> Repl "Stdio.Out_channel.output_bytes Stdio.stderr"
  | "prerr_char"          -> Repl "Stdio.Out_channel.output_char Stdio.stderr"
  | "prerr_endline"       -> Repl "Stdio.prerr_endline"
  | "prerr_float"         -> Repl "Stdio.eprintf \"%f\""
  | "prerr_int"           -> Repl "Stdio.eprintf \"%d\""
  | "prerr_newline"       -> Repl "Stdio.eprintf \"\n%!\""
  | "prerr_string"        -> Repl "Stdio.Out_channel.output_string Stdio.stderr"
  | "print_bytes"         -> Repl "Stdio.Out_channel.output_bytes Stdio.stdout"
  | "print_char"          -> Repl "Stdio.Out_channel.output_char Stdio.stdout"
  | "print_endline"       -> Repl "Stdio.print_endline"
  | "print_float"         -> Repl "Stdio.eprintf \"%f\""
  | "print_int"           -> Repl "Stdio.eprintf \"%d\""
  | "print_newline"       -> Repl "Stdio.eprintf \"\n%!\""
  | "print_string"        -> Repl "Stdio.Out_channel.output_string Stdio.stdout"
  | "read_float"          -> No_equivalent
  | "read_int"            -> No_equivalent
  | "read_line"           -> Repl "Stdio.In_channel.input_line"
  | "really_input"        -> Repl "Stdio.In_channel.really_input"
  | "really_input_string" -> Approx "Stdio.In_channel"
  | "seek_in"             -> Repl "Stdio.In_channel.seek"
  | "seek_out"            -> Repl "Stdio.Out_channel.seek"
  | "set_binary_mode_in"  -> Repl "Stdio.In_channel.set_binary_mode"
  | "set_binary_mode_out" -> Repl "Stdio.Out_channel.set_binary_mode"
  | "sin"                 -> Repl "Float.sin"
  | "sinh"                -> Repl "Float.sinh"
  | "sqrt"                -> Repl "Float.sqrt"
  | "stderr"              -> Repl "Stdio.stderr"
  | "stdin"               -> Repl "Stdio.stdin"
  | "stdout"              -> Repl "Stdio.stdout"
  | "string_of_bool"      -> Repl "Bool.to_string"
  | "string_of_float"     -> Repl "Float.to_string"
  | "string_of_int"       -> Repl "Int.to_string"
  | "succ"                -> Repl "Int.succ"
  | "tan"                 -> Repl "Float.tan"
  | "tanh"                -> Repl "Float.tanh"
  | "truncate"            -> Repl "Int.of_float"
  (* This is documented as DO-NOT-USE in the stdlib *)
  | "unsafe_really_input" -> No_equivalent
  | _                     -> No_equivalent
;;

let exception_replacement = function
  | "Not_found" ->
    Some (Repl_text "\
Instead of raising [Not_found], consider using [raise_s] with an informative error\n\
message.  If code needs to distinguish [Not_found] from other exceptions, please change\n\
it to handle both [Not_found] and [Not_found_s].  Then, instead of raising [Not_found],\n\
raise [Not_found_s] with an informative error message")
  | _ -> None

let type_replacement = function
  | "result" -> Some (Repl "Result.t")
  | "in_channel"  -> Some (Repl "Stdio.In_channel.t")
  | "out_channel" -> Some (Repl "Stdio.Out_channel.t")
  | _ -> None
;;

let module_replacement = function
  | "Printexc" -> Some (Repl_text "Use [Exn] or [Backtrace] instead")
  | "Format" ->
    let repl_text =
      "[Base] doesn't export a [Format] module, although the \n\
       [Caml.Format.formatter] type is available (as [Formatter.t])\n\
       for interaction with other libraries"
    in
    Some (Repl_text repl_text)
  | "Fun" -> Some (Repl "Fn")
  | "Gc" -> Some No_equivalent
  | "Seq" -> Some (Approx "Sequence")
  | _ -> None

let replace ~is_exn id replacement line =
  let msg =
    match replacement with
    | No_equivalent  -> deprecated_msg_no_equivalent ~is_exn id
    | Repl repl      -> deprecated_msg_with_repl ~is_exn repl
    | Repl_text text -> deprecated_msg_with_repl_text ~is_exn text
    | Approx repl    -> deprecated_msg_with_approx_repl ~is_exn repl ~id
  in
  sprintf "%s\n%s" line msg
;;
}

let id_trail = ['a'-'z' 'A'-'Z' '_' '0'-'9']*
               let id = ['a'-'z' 'A'-'Z' '_' '0'-'9'] id_trail
let val_id = id | '(' [^ ')']* ')'
let params = ('(' [^')']* ')' | ['+' '-']? '\'' id) " "

let val_ = "val " | "external "

rule line = parse
  | "module Camlinternal" _*
    { "" (* We can't deprecate these *) }
  | "module Bigarray" _* { "" (* Don't deprecate it yet *) }
  | "type " (params? (id as id) _* as def)
      { sprintf "type nonrec %s"
          (match type_replacement id with
           | Some replacement -> replace ~is_exn:false id replacement def
           | None -> sprintf "%s\n%s" def (deprecated_msg ~is_exn:false id)) }

  | val_ (val_id as id) _* as line { replace ~is_exn:false id (val_replacement id) line }

  | "module " (id as id) " = Stdlib__" (id as id2) (_* as line)
      {
        let line =
          Printf.sprintf "module %s = Stdlib.%s %s"
            id (String.capitalize_ascii id2) line in
        match module_replacement id with
        | Some replacement -> replace ~is_exn:false id replacement line
        | None -> sprintf "%s\n%s" line (deprecated_msg ~is_exn:false id) }

  | "exception " (id as id) _* as line
    { match exception_replacement id with
      | Some replacement -> replace ~is_exn:true id replacement line
      | None ->
        let predefined_exceptions =
          [ "Out_of_memory"
          ; "Sys_error"
          ; "Failure"
          ; "Invalid_argument"
          ; "End_of_file"
          ; "Division_by_zero"
          ; "Not_found"
          ; "Match_failure"
          ; "Stack_overflow"
          ; "Sys_blocked_io"
          ; "Assert_failure"
          ; "Undefined_recursive_module" ]
        in
        if List.mem id ~set:predefined_exceptions
        then ""
        else sprintf "%s\n%s" line (deprecated_msg ~is_exn:true id)
    }
  | "module " (id as id) _* as line
    { match module_replacement id with
      | Some replacement -> replace ~is_exn:false id replacement line
      | None -> sprintf "%s\n%s" line (deprecated_msg ~is_exn:false id) }
  | _* as line
    { ksprintf failwith "cannot parse this: %s" line }
