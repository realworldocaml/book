open Core_kernel

include Bin_prot.Std

let hex_char n =
  if n < 10 then
    Char.of_int_exn (n + Char.to_int '0')
  else
    Char.of_int_exn (n - 10 + Char.to_int 'a')

let to_hex buf max_len =
  let len = String.length buf in
  assert (len <= max_len);
  let str = Bytes.make (max_len * 3 - 1) ' ' in
  for column = 0 to max_len - 1 do
    let ofs = (max_len - 1 - column) in
    if ofs >= len then begin
      Bytes.set str (column * 3 + 0) '.';
      Bytes.set str (column * 3 + 1) '.';
    end else begin
      let byte = Char.to_int buf.[ofs] in
      Bytes.set str (column * 3 + 0) (hex_char (byte lsr 4));
      Bytes.set str (column * 3 + 1) (hex_char (byte land 0xf));
    end
  done;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:str
