let version = "%%VERSION%%"

exception Json_error of string

let json_error s = raise (Json_error s)

exception End_of_array
exception End_of_object
exception End_of_tuple
exception End_of_input

type in_param = {
  string_buf : Buffer.t
}

let create_in_param ?(len = 256) () = {
  string_buf = Buffer.create len
}


let utf8_of_code buf x =
  let add = Bi_outbuf.add_char in

  (* Straight <= doesn't work with signed 31-bit ints *)
  let maxbits n x = x lsr n = 0 in

  if maxbits 7 x then
    (* 7 *)
    add buf (Char.chr x)
  else if maxbits 11 x then (
    (* 5 + 6 *)
    add buf (Char.chr (0b11000000 lor ((x lsr 6) land 0b00011111)));
    add buf (Char.chr (0b10000000 lor (x         land 0b00111111)))
  )
  else if maxbits 16 x then (
    (* 4 + 6 + 6 *)
    add buf (Char.chr (0b11100000 lor ((x lsr 12) land 0b00001111)));
    add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x          land 0b00111111)))
  )
  else if maxbits 21 x then (
    (* 3 + 6 + 6 + 6 *)
    add buf (Char.chr (0b11110000 lor ((x lsr 18) land 0b00000111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
  )
  else if maxbits 26 x then (
    (* 2 + 6 + 6 + 6 + 6 *)
    add buf (Char.chr (0b11111000 lor ((x lsr 24) land 0b00000011)));
    add buf (Char.chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
  )
  else (
    assert (maxbits 31 x);
    (* 1 + 6 + 6 + 6 + 6 + 6 *)
    add buf (Char.chr (0b11111100 lor ((x lsr 30) land 0b00000001)));
    add buf (Char.chr (0b10000000 lor ((x lsr 24) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 18) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr 12) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor ((x lsr  6) land 0b00111111)));
    add buf (Char.chr (0b10000000 lor (x          land 0b00111111)));
  )

let code_of_surrogate_pair i j =
  let high10 = i - 0xD800 in
  let low10 = j - 0xDC00 in
  0x10000 + ((high10 lsl 10) lor low10)

let utf8_of_surrogate_pair buf i j =
  utf8_of_code buf (code_of_surrogate_pair i j)

let is_object_or_array x =
  match x with
      `List _
    | `Assoc _ -> true
    | _ -> false


type lexer_state = {
  buf : Bi_outbuf.t;
    (* Buffer used to accumulate substrings *)

  mutable lnum : int;
    (* Current line number (starting from 1) *)

  mutable bol : int;
    (* Absolute position of the first character of the current line
       (starting from 0) *)

  mutable fname : string option;
    (* Name describing the input file *)
}

module Lexer_state =
struct
  type t = lexer_state = {
    buf : Bi_outbuf.t;
    mutable lnum : int;
    mutable bol : int;
    mutable fname : string option;
  }
end

let init_lexer ?buf ?fname ?(lnum = 1) () =
  let buf =
    match buf with
	None -> Bi_outbuf.create 256
      | Some buf -> buf
  in
  {
    buf = buf;
    lnum = lnum;
    bol = 0;
    fname = fname
  }
