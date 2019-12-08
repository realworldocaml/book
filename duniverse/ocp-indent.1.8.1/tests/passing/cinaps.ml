(*$ open Bin_prot_cinaps $*)

let bin_read_nat0 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00'..'\x7f' as ch ->
    pos_ref := pos + 1;
    Nat0.unsafe_of_int (Char.code ch)
  | (*$ Code.char INT16 *)'\xfe'(*$*) ->
    safe_bin_read_nat0_16 buf ~pos_ref ~pos:(pos + 1)
  | (*$ Code.char INT32 *)'\xfd'(*$*) ->
    safe_bin_read_nat0_32 buf ~pos_ref ~pos:(pos + 1)
  | (*$ Code.char INT64 *)'\xfc'(*$*) ->
    if arch_sixtyfour then
      safe_bin_read_nat0_64 buf ~pos_ref ~pos:(pos + 1)
    else
      raise_read_error ReadError.Nat0_overflow pos
  | _ ->
    raise_read_error ReadError.Nat0_code pos
[@@ocamlformat "disable"]

let bin_read_int buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00'..'\x7f' as ch ->
    pos_ref := pos + 1;
    Char.code ch
  | (*$ Code.char NEG_INT8 *)'\xff'(*$*) ->
    safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1)
  | (*$ Code.char INT16 *)'\xfe'(*$*) ->
    safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1)
  | (*$ Code.char INT32 *)'\xfd'(*$*) ->
    safe_bin_read_int32_as_int buf ~pos_ref ~pos:(pos + 1)
  | (*$ Code.char INT64 *)'\xfc'(*$*) ->
    if arch_sixtyfour then
      safe_bin_read_int64_as_int buf ~pos_ref ~pos:(pos + 1)
    else
      raise_read_error ReadError.Int_overflow pos
  | _ ->
    raise_read_error ReadError.Int_code pos
[@@ocamlformat "disable"]

let bin_read_float buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  let next = pos + 8 in
  check_next buf next;
  pos_ref := next;
  (* No error possible either. *)
  Int64.float_of_bits (unsafe_get64le buf pos)
;;

let bin_read_int32 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00'..'\x7f' as ch ->
    pos_ref := pos + 1;
    Int32.of_int (Char.code ch)
  | (*$ Code.char NEG_INT8 *)'\xff'(*$*) ->
    Int32.of_int (safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1))
  | (*$ Code.char INT16 *)'\xfe'(*$*) ->
    Int32.of_int (safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1))
  | (*$ Code.char INT32 *)'\xfd'(*$*) ->
    safe_bin_read_int32 buf ~pos_ref ~pos:(pos + 1)
  | _ ->
    raise_read_error ReadError.Int32_code pos
[@@ocamlformat "disable"]

let bin_read_int64 buf ~pos_ref =
  let pos = safe_get_pos buf pos_ref in
  assert_pos pos;
  match unsafe_get buf pos with
  | '\x00'..'\x7f' as ch ->
    pos_ref := pos + 1;
    Int64.of_int (Char.code ch)
  | (*$ Code.char NEG_INT8 *)'\xff'(*$*) ->
    Int64.of_int (safe_bin_read_neg_int8 buf ~pos_ref ~pos:(pos + 1))
  | (*$ Code.char INT16 *)'\xfe'(*$*) ->
    Int64.of_int (safe_bin_read_int16 buf ~pos_ref ~pos:(pos + 1))
  | (*$ Code.char INT32 *)'\xfd'(*$*) ->
    safe_bin_read_int32_as_int64 buf ~pos_ref ~pos:(pos + 1)
  | (*$ Code.char INT64 *)'\xfc'(*$*) ->
    safe_bin_read_int64 buf ~pos_ref ~pos:(pos + 1)
  | _ ->
    raise_read_error ReadError.Int64_code pos
[@@ocamlformat "disable"]
