(* using the hash_variant of pa_type_conv at compile time *)
let repr_of_poly_variant : [> ] -> int = fun variant ->
  let obj = Obj.repr variant in
  if Obj.is_int obj then Obj.obj obj
  else
    let size = Obj.size obj in
    assert (size = 2);
    let repr = Obj.field obj 0 in
    (assert (Obj.is_int repr));
    Obj.obj repr

let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

(* a few unit tests of cases that have triggered diffs in the past of this
   lib *)
let () = assert (repr_of_poly_variant `Latency_stats = hash_variant "Latency_stats")
let () = assert (repr_of_poly_variant `zero = hash_variant "zero")

let double_array_value = Obj.magic 0.
let has_double_array_tag a = Obj.double_array_tag = (Obj.tag (Obj.repr a))
let () =
  let module M = struct
    type double = { a : float ; b : float }
    type simple = { c : float ; d : int }
    let double = { a = double_array_value; b = double_array_value; }
    let simple = { c = double_array_value; d = double_array_value; }
  end in
  assert (has_double_array_tag M.double);
  assert (not (has_double_array_tag M.simple));
;;
