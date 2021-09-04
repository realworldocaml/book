open Crowbar

let () =
  add_test ~name:"bool_of_int" [ int ]
  @@ fun n ->
  let result = Eqaf.bool_of_int n in
  check_eq ~eq:(=) (if n = 0 then false else true) result

let () =
  add_test ~name:"select_a_if_in_range"
    [ range max_int ; range max_int ; int; range 10000; range 10000 ]
  @@ fun low high n a b ->
  let low, high = min low high, max low high in
  let choice = Eqaf.select_a_if_in_range ~low ~high ~n a b in
  check_eq ~eq:(=) (if low <= n && n <= high then a else b) choice

let () =
  add_test ~name:"uppercase_ascii" [ bytes ]
  @@ fun raw_str ->
  check_eq ~eq:String.equal
    (String.uppercase_ascii raw_str)
    (Eqaf.uppercase_ascii raw_str)

let () =
  add_test ~name:"lowercase_ascii" [ bytes ]
  @@ fun raw_str ->
  check_eq ~eq:String.equal
    (String.lowercase_ascii raw_str)
    (Eqaf.lowercase_ascii raw_str)

let () =
  (* These are here for compat with OCaml <= 4.09
     from >= they can be replaced by
     Int32.unsigned_div
     Int32.unsigned_rem
  *)
  let int32_div_unsigned n d =
    let sub,min_int = Int32.(sub,min_int)in
    let int32_unsigned_compare n m =
      Int32.compare (sub n min_int) (sub m min_int)
    in
    if d < 0_l then
      if int32_unsigned_compare n d < 0 then 0_l else 1_l
    else
      let q =
        let open Int32 in
        shift_left (Int32.div (Int32.shift_right_logical n 1) d) 1 in
      let r = sub n (Int32.mul q d) in
      if int32_unsigned_compare r d >= 0 then Int32.succ q else q
  in
  let int32_rem_unsigned n d =
    Int32.sub n (Int32.mul (int32_div_unsigned n d) d)
  in
  add_test ~name:"divmod" [ int32 ; int32 ]
  @@ fun x m ->
  try
    let result = Eqaf.divmod ~x ~m in
    let expect = int32_div_unsigned x m,
                 int32_rem_unsigned x m in
    check_eq ~eq:(=) expect result
  with
  | Invalid_argument desc ->
    (* we expect this for negative m: *)
    assert (desc = "m <= 0" || desc = "m >= 16348 not supported")

let () =
  add_test ~name:"hex_of_string |> string_of_hex" [ bytes ]
  @@ fun raw ->
  let enc = Eqaf.hex_of_string raw in
  let dec = Eqaf.string_of_hex enc in
  check_eq ~pp:(fun fmt (s,err) -> Format.fprintf fmt "(%S,%d)" s err)
    ~eq:(=) (raw,0) dec ;
  String.iter (function (* check for invalid encoding: *)
      | '0'..'9'
      | 'a'..'z'
      | 'A'..'Z' -> ()
      | _ -> assert false) enc

let () =
  add_test ~name:"string_of_hex |> hex_of_string" [ bytes ]
  @@ fun hex ->
  begin
    match Eqaf.string_of_hex hex with
    | dec, 0 ->
      let enc = Eqaf.hex_of_string dec in
      check_eq ~pp:(fun fmt s -> Format.pp_print_string fmt @@ String.escaped s)
        ~eq:(=) (String.lowercase_ascii hex) enc ;
    | _ ->
      let invalid = ref false in
      begin
        if (String.length hex mod 2 = 1)
        then invalid := true
        else String.iter (function
            | 'a'..'f'
            | '0'..'9'
            | 'A'..'F' -> ()
            | _ -> invalid := true
          ) hex
      end ; assert !invalid (* we expect it to be invalid since it raised *)
  end

let () =
  add_test ~name:"equal" [ bytes; bytes; ] @@ fun a b ->
  let expect = String.equal a b in
  let result = Eqaf.equal a b in
  check_eq ~pp:Format.pp_print_bool ~eq:(=) expect result

let rev str =
  let len = String.length str in
  let res = Bytes.create len in
  for i = 0 to len - 1 do Bytes.set res (len - 1 - i) str.[i] done ;
  Bytes.unsafe_to_string res

type order = Zero | Neg | Pos

let of_int = function 0 -> Zero | n -> if n < 0 then Neg else Pos

let pf = Format.fprintf
let pp_order ppf = function Zero -> pf ppf "Zero" | Neg -> pf ppf "Neg" | Pos -> pf ppf "Pos"

let () =
  add_test ~name:"compare_le" [ bytes; bytes ] @@ fun a b ->
  if String.length a <> String.length b then bad_test () ;
  let expect = String.compare a b in
  let result = Eqaf.compare_be a b in
  check_eq ~pp:pp_order ~eq:(=) (of_int expect) (of_int result)

let () =
  add_test ~name:"compare_be" [ bytes; bytes ] @@ fun a b ->
  if String.length a <> String.length b then bad_test () ;
  let expect = String.compare (rev a) (rev b) in
  let result = Eqaf.compare_le a b in
  check_eq ~pp:pp_order ~eq:(=) (of_int expect) (of_int result)
