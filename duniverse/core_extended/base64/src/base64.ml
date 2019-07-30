open Core_kernel

module type T = sig
  val encode : string -> string
  val decode : string -> string * [`Unconsumed_data of string] option
  val decode_exn : string -> string
end

module Make (D : sig
    val char62 : char
    val char63 : char
    val pad_char : char
    val pad_when_encoding : bool
    val ignore_char : char -> bool
  end) =
struct
  let encode_char i =
    if 0 <= i && i <= 25
    then Char.of_int_exn (Char.to_int 'A' + i)
    else if 26 <= i && i <= 51
    then Char.of_int_exn (Char.to_int 'a' + i - 26)
    else if 52 <= i && i <= 61
    then Char.of_int_exn (Char.to_int '0' + i - 52)
    else if i = 62
    then D.char62
    else if i = 63
    then D.char63
    else
      failwithf
        "Impossible Base64 encoder bug: not representable as a base64 character: %d"
        i
        ()
  ;;

  type base64_char =
    | Data of int
    | Skip
    | Other

  let decode_char c =
    if 'A' <= c && c <= 'Z'
    then Data (Char.to_int c - Char.to_int 'A')
    else if 'a' <= c && c <= 'z'
    then Data (Char.to_int c - Char.to_int 'a' + 26)
    else if '0' <= c && c <= '9'
    then Data (Char.to_int c - Char.to_int '0' + 52)
    else if c = D.char62
    then Data 62
    else if c = D.char63
    then Data 63
    else if D.ignore_char c
    then Skip
    else Other
  ;;

  let encode source =
    let dest = Bytes.create ((String.length source + 2) / 3 * 4) in
    let source_get i =
      if i < String.length source then source.[i] |> Char.to_int else 0
    in
    let rec loop ~i ~j =
      if i < String.length source
      then (
        let a = source_get (i + 0) in
        let b = source_get (i + 1) in
        let c = source_get (i + 2) in
        Bytes.set dest (j + 0) Int.(shift_right a 2 |> bit_and 0x3f |> encode_char);
        Bytes.set
          dest
          (j + 1)
          Int.(bit_or (shift_left a 4) (shift_right b 4) |> bit_and 0x3f |> encode_char);
        Bytes.set
          dest
          (j + 2)
          Int.(bit_or (shift_left b 2) (shift_right c 6) |> bit_and 0x3f |> encode_char);
        Bytes.set dest (j + 3) Int.(c |> bit_and 0x3f |> encode_char);
        loop ~i:(i + 3) ~j:(j + 4))
      else (
        match String.length source % 3, D.pad_when_encoding with
        | 0, _ -> Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dest
        | rest, false ->
          (* Remove extra bytes *)
          Bytes.To_string.sub dest ~pos:0 ~len:(j - 3 + rest)
        | 1, true ->
          (* Set padding *)
          Bytes.set dest (j - 2) D.pad_char;
          Bytes.set dest (j - 1) D.pad_char;
          Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dest
        | 2, true ->
          (* Set padding *)
          Bytes.set dest (j - 1) D.pad_char;
          Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dest
        | _, _ -> failwith "Impossible")
    in
    loop ~i:0 ~j:0
  ;;

  type block =
    | No_chars
    | Two_chars of { a : int; b : int; i : int }
    | Three_chars of { a : int; b : int; c : int; i : int }
    | Four_chars of { a : int; b : int; c : int; d : int; i : int }

  let read_base64_block source i =
    let rec read_char i =
      if i >= String.length source
      then None
      else (
        match source.[i] |> decode_char with
        | Data b -> Some (b, i + 1)
        | Skip -> read_char (i + 1)
        | Other -> None)
    in
    match read_char i with
    | None -> No_chars
    | Some (a, i) ->
      (match read_char i with
       | None -> No_chars
       | Some (b, i) ->
         (match read_char i with
          | None -> Two_chars { a; b; i }
          | Some (c, i) ->
            (match read_char i with
             | None -> Three_chars { a; b; c; i }
             | Some (d, i) -> Four_chars { a; b; c; d; i })))
  ;;

  let decode source =
    (* We need 3 bytes for every 4 bytes of [source]. We round up to handle the
       possibility of no padding *)
    let dest = Bytes.create ((String.length source + 2) / 4 * 3) in
    let set ~a ~b ?(c = 0) ?(d = 0) j =
      Bytes.set
        dest
        (j + 0)
        Int.(
          bit_or (shift_left a 2) (shift_right b 4) |> bit_and 0xff |> Char.of_int_exn);
      Bytes.set
        dest
        (j + 1)
        Int.(
          bit_or (shift_left b 4) (shift_right c 2) |> bit_and 0xff |> Char.of_int_exn);
      Bytes.set
        dest
        (j + 2)
        Int.(bit_or (shift_left c 6) d |> bit_and 0xff |> Char.of_int_exn)
    in
    let finish i j =
      let result = Bytes.To_string.sub dest ~pos:0 ~len:j in
      let rec stop i =
        if i < String.length source
        then (
          let c = source.[i] in
          if D.ignore_char c || c = D.pad_char
          then stop (i + 1)
          else
            Some
              (`Unconsumed_data
                 (String.sub source ~pos:i ~len:(String.length source - i))))
        else None
      in
      let unconsumed_data = stop i in
      result, unconsumed_data
    in
    let rec loop i j =
      match read_base64_block source i with
      | No_chars -> finish i j
      | Two_chars { a; b; i } ->
        set j ~a ~b;
        finish i (j + 1)
      | Three_chars { a; b; c; i } ->
        set j ~a ~b ~c;
        finish i (j + 2)
      | Four_chars { a; b; c; d; i } ->
        set j ~a ~b ~c ~d;
        loop i (j + 3)
    in
    loop 0 0
  ;;

  let decode_exn source =
    match decode source with
    | result, None -> result
    | decoded, Some (`Unconsumed_data unconsumed_data) ->
      raise_s
        [%message
          "[decode_exn] failed with warnings"
            (decoded : string)
            (unconsumed_data : string)]
  ;;
end

include Make (struct
    let char62 = '+'
    let char63 = '/'
    let pad_char = '='
    let pad_when_encoding = true
    let ignore_char = Char.is_whitespace
  end)

module Websafe = Make (struct
    let char62 = '-'
    let char63 = '_'
    let pad_char = '='
    let pad_when_encoding = false
    let ignore_char _ = false
  end)
