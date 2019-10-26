exception Error of string

let error s = raise (Error s)


(*
  Debugging utilities.
*)

let string8_of_int x =
  let s = Bytes.create 8 in
  for i = 0 to 7 do
    Bytes.set s (7-i) (Char.chr (0xff land (x lsr (8 * i))))
  done;
  Bytes.to_string s

let string4_of_int x =
  let s = Bytes.create 4 in
  for i = 0 to 3 do
    Bytes.set s (3-i) (Char.chr (0xff land (x lsr (8 * i))))
  done;
  Bytes.to_string s

let print_bits ?(pos = 0) ?len s =
  let slen = String.length s in
  if pos < 0 || (pos > 0 && pos >= slen) then
    invalid_arg "Bi_util.print_bits";
  let len =
    match len with
        None -> slen - pos
      | Some len ->
          if len > slen - pos then invalid_arg "Bi_util.print_bits"
          else len
  in

  let r = Bytes.create (len * 9) in
  for i = 0 to len - 1 do
    let k = i * 9 in
    let x = Char.code s.[pos+i] in
    for j = 0 to 7 do
      Bytes.set r (k+j) (if (x lsr (7 - j)) land 1 = 0 then '0' else '1')
    done;
    Bytes.set r (k+8) (if (i + 1) mod 8 = 0 then '\n' else ' ')
  done;
  Bytes.to_string r

(* int size in bits *)
let int_size =
  let c = ref 0 in
  let r = ref (-1) in
  while !r <> 0 do
    r := !r lsr 1;
    incr c
  done;
  !c
