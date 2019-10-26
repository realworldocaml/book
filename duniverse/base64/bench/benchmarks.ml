module Old_version = struct
  let default_alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  let uri_safe_alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

  let padding = '='

  let of_char ?(alphabet = default_alphabet) x =
    if x = padding then 0 else String.index alphabet x

  let to_char ?(alphabet = default_alphabet) x = alphabet.[x]

  let decode ?alphabet input =
    let length = String.length input in
    let input =
      if length mod 4 = 0 then input
      else input ^ String.make (4 - (length mod 4)) padding
    in
    let length = String.length input in
    let words = length / 4 in
    let padding =
      match length with
      | 0 -> 0
      | _ when input.[length - 2] = padding -> 2
      | _ when input.[length - 1] = padding -> 1
      | _ -> 0
    in
    let output = Bytes.make ((words * 3) - padding) '\000' in
    for i = 0 to words - 1 do
      let a = of_char ?alphabet input.[(4 * i) + 0]
      and b = of_char ?alphabet input.[(4 * i) + 1]
      and c = of_char ?alphabet input.[(4 * i) + 2]
      and d = of_char ?alphabet input.[(4 * i) + 3] in
      let n = (a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
      let x = (n lsr 16) land 255
      and y = (n lsr 8) land 255
      and z = n land 255 in
      Bytes.set output ((3 * i) + 0) (char_of_int x) ;
      if i <> words - 1 || padding < 2 then
        Bytes.set output ((3 * i) + 1) (char_of_int y) ;
      if i <> words - 1 || padding < 1 then
        Bytes.set output ((3 * i) + 2) (char_of_int z)
    done ;
    Bytes.unsafe_to_string output

  let decode_opt ?alphabet input =
    try Some (decode ?alphabet input) with Not_found -> None

  let encode ?(pad = true) ?alphabet input =
    let length = String.length input in
    let words = (length + 2) / 3 (* rounded up *) in
    let padding_len = if length mod 3 = 0 then 0 else 3 - (length mod 3) in
    let output = Bytes.make (words * 4) '\000' in
    let get i = if i >= length then 0 else int_of_char input.[i] in
    for i = 0 to words - 1 do
      let x = get ((3 * i) + 0)
      and y = get ((3 * i) + 1)
      and z = get ((3 * i) + 2) in
      let n = (x lsl 16) lor (y lsl 8) lor z in
      let a = (n lsr 18) land 63
      and b = (n lsr 12) land 63
      and c = (n lsr 6) land 63
      and d = n land 63 in
      Bytes.set output ((4 * i) + 0) (to_char ?alphabet a) ;
      Bytes.set output ((4 * i) + 1) (to_char ?alphabet b) ;
      Bytes.set output ((4 * i) + 2) (to_char ?alphabet c) ;
      Bytes.set output ((4 * i) + 3) (to_char ?alphabet d)
    done ;
    for i = 1 to padding_len do
      Bytes.set output (Bytes.length output - i) padding
    done ;
    if pad then Bytes.unsafe_to_string output
    else Bytes.sub_string output 0 (Bytes.length output - padding_len)
end

let random len =
  let ic = open_in "/dev/urandom" in
  let rs = Bytes.create len in
  really_input ic rs 0 len ;
  close_in ic ;
  Bytes.unsafe_to_string rs

open Core
open Core_bench

let b64_encode_and_decode len =
  let input = random len in
  Staged.stage @@ fun () ->
  let encoded = Base64.encode_exn input in
  let _decoded = Base64.decode_exn encoded in
  ()

let old_encode_and_decode len =
  let input = random len in
  Staged.stage @@ fun () ->
  let encoded = Old_version.encode input in
  let _decoded = Old_version.decode encoded in
  ()

let args = [ 0; 10; 50; 100; 500; 1000; 2500; 5000 ]

let test_b64 =
  Test.create_indexed ~name:"Base64"
    ~args b64_encode_and_decode

let test_old =
  Test.create_indexed ~name:"Old"
    ~args old_encode_and_decode

let command =
  Bench.make_command [ test_b64; test_old ]

let () = Command.run command
