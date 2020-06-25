let () = Random.self_init ()

let random_string () =
  let s = Bytes.create (Random.int 20483) in
  for i = 0 to Bytes.length s - 1 do
    Bytes.set s i (Char.chr (Random.int 256))
  done;
  Bytes.to_string s

let check msg x y =
  if x = y then ()
  else (
    Printf.eprintf "\n---\n%S\n---\n%S\n%s: \027[31mERROR!\027[m\n" x y msg;
    exit 1
  )

let success = ref 0

let test s =
  let h1 = Hex.of_string s in
  check "to/from string" s (Hex.to_string h1);
  incr success

let test_cs s =
  let cs = Cstruct.of_string s in
  let `Hex s = Hex.of_cstruct cs in
  let `Hex s' = (Hex.of_string (Cstruct.to_string cs)) in
  check "of_cstruct = of_string" s s';
  incr success

let test_suite test =
  test "";
  test "deadbeef";
  for _ = 0 to 100 do
    test (random_string ())
  done

let test_cs_array () =
  let open Bigarray_compat in
  let arr = Array1.of_array char c_layout
      [|'0'; '1'; '2'; '3'; '4'; '5';
        '6'; '7'; '8'; '9'; 'a'; 'b';
        'c'; 'd'; 'e'; 'f'|] in
  let cs = Cstruct.of_bigarray arr in
  let hex = Hex.of_cstruct cs in
  let s = Hex.to_string hex in
  check "cstruct array" s "0123456789abcdef";
  incr success

let test_ignore () =
  let s = "...   aJjf...c 1" in
  let h = Hex.of_string ~ignore:[' '; '.'; 'j'; 'J'] s in
  check "string ignore" "afc1" (Hex.to_string h);
  let cs = Cstruct.of_string s in
  let h = Hex.of_cstruct ~ignore:[' '; '.'; 'j'; 'J'] cs in
  check "cstruct ignore" "afc1" (Cstruct.to_string (Hex.to_cstruct h))

let test_hexdump () =
  let test input answer =
    let hex = Hex.of_string input in
    let s = Hex.hexdump_s hex in
    check "hexdump" s answer
  in
  (* Test out the various padding situations we need to get right *)
  (* Less than 1 row *)
  test "i am less" "00000000: 6920 616d 206c 6573 73                   i am less\n";
  (* Exactly 2 rows long *)
  test "i am a 32 character string......" "00000000: 6920 616d 2061 2033 3220 6368 6172 6163  i am a 32 charac\n00000001: 7465 7220 7374 7269 6e67 2e2e 2e2e 2e2e  ter string......\n";
  (* 8 rows with some remainder *)
  let input = "# OASIS_START
# DO NOT EDIT (digest: 3a6a404057e98b471c7d0eb9f9ea243c)
version = \"0.1.0\"
description = \"Hexadecimal converter\"" in
  let hex = Hex.of_string input in
  let s = Hex.hexdump_s hex in
  check "hexdump" s "00000000: 2320 4f41 5349 535f 5354 4152 540a 2320  # OASIS_START.# 
00000001: 444f 204e 4f54 2045 4449 5420 2864 6967  DO NOT EDIT (dig
00000002: 6573 743a 2033 6136 6134 3034 3035 3765  est: 3a6a404057e
00000003: 3938 6234 3731 6337 6430 6562 3966 3965  98b471c7d0eb9f9e
00000004: 6132 3433 6329 0a76 6572 7369 6f6e 203d  a243c).version =
00000005: 2022 302e 312e 3022 0a64 6573 6372 6970   \"0.1.0\".descrip
00000006: 7469 6f6e 203d 2022 4865 7861 6465 6369  tion = \"Hexadeci
00000007: 6d61 6c20 636f 6e76 6572 7465 7222       mal converter\"\n";
  incr success

let () =
  test_suite test;
  test_suite test_cs;
  test_cs_array ();
  test_ignore ();
  test_hexdump ();
  Printf.printf "\027[32mSUCCESS!\027[m (%d/%d tests pass)\n" !success !success
