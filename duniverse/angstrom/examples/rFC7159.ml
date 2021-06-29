open Angstrom

type json =
  [ `Null
  | `False
  | `True
  | `String of string
  | `Number of float
  | `Object of (string * json) list
  | `Array of json list ]

let ws = skip_while (function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false)

let lchar c =
  ws *> char c

let rsb = lchar ']'
let rcb = lchar '}'
let ns, vs  = lchar ':', lchar ','
let quo = lchar '"'

let _false : json t = string "false" *> return `False
let _true  : json t = string "true"  *> return `True
let _null  : json t = string "null"  *> return `Null

let num =
  take_while1 (function
    | '\x20' | '\x0a' | '\x0d' | '\x09'
    | '[' | ']' | '{' | '}' | ':' | ',' -> false
    | _               -> true)
  >>= fun s ->
  try return (`Number (float_of_string s))
  with _ -> fail "number"

module S = struct
  type t =
    [ `Unescaped
    | `Escaped
    | `UTF8  of char list
    | `UTF16 of int * [`S | `U | `C of char list]
    | `Error of string
    | `Done ]

  let to_string : [`Terminate | t] -> string = function
    | `Unescaped -> "unescaped"
    | `Escaped   -> "escaped"
    | `UTF8 _    -> "utf-8 _"
    | `UTF16 _   -> "utf-16 _ _"
    | `Error e   -> Printf.sprintf "error %S" e
    | `Terminate -> "terminate"
    | `Done      -> "done"

  let unescaped buf = function
    | '"'  -> `Terminate
    | '\\' -> `Escaped
    | c    ->
      if c <= '\031'
      then `Error (Printf.sprintf "unexpected character '%c'" c)
      else begin Buffer.add_char buf c; `Unescaped end

  let escaped buf = function
    | '\x22' -> Buffer.add_char buf '\x22'; `Unescaped
    | '\x5c' -> Buffer.add_char buf '\x5c'; `Unescaped
    | '\x2f' -> Buffer.add_char buf '\x2f'; `Unescaped
    | '\x62' -> Buffer.add_char buf '\x08'; `Unescaped
    | '\x66' -> Buffer.add_char buf '\x0c'; `Unescaped
    | '\x6e' -> Buffer.add_char buf '\x0a'; `Unescaped
    | '\x72' -> Buffer.add_char buf '\x0d'; `Unescaped
    | '\x74' -> Buffer.add_char buf '\x09'; `Unescaped
    | '\x75' -> `UTF8 []
    | _      -> `Error "invalid escape sequence"

  let hex c =
    match c with
    | '0' .. '9' -> Char.code c - 0x30 (* '0' *)
    | 'a' .. 'f' -> Char.code c - 87
    | 'A' .. 'F' -> Char.code c - 55
    | _          -> 255

  let utf_8 buf d = function
    | [c;b;a] ->
      let a = hex a and b = hex b and c = hex c and d = hex d in
      if a lor b lor c lor d = 255 then
        `Error "invalid hex escape"
      else
        let cp = (a lsl 12) lor (b lsl 8) lor (c lsl 4) lor d in
        if cp >= 0xd800 && cp <= 0xdbff then
          `UTF16(cp, `S)
        else begin
          Buffer.add_char buf (Char.unsafe_chr (0b11100000 lor ((cp lsr 12) land 0b00001111)));
          Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((cp lsr  6) land 0b00111111)));
          Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor (cp          land 0b00111111)));
          `Unescaped
        end
    | cs -> `UTF8 (d::cs)

  let utf_16 buf d x s =
    match s, d with
    | `S        , '\\' -> `UTF16(x, `U)
    | `U        , 'u'  -> `UTF16(x, `C [])
    | `C [c;b;a], _    ->
      let a = hex a and b = hex b and c = hex c and d = hex d in
      if a lor b lor c lor d = 255 then
        `Error "invalid hex escape"
      else
        let y = (a  lsl 12) lor (b lsl 8) lor (c lsl 4) lor d in
        if y >= 0xdc00 && y <= 0xdfff then begin
          let hi = x - 0xd800 in
          let lo = y - 0xdc00 in
          let cp = 0x10000 + ((hi lsl 10) lor lo) in
          Buffer.add_char buf (Char.unsafe_chr (0b11110000 lor ((cp lsr 18) land 0b00000111)));
          Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((cp lsr 12) land 0b00111111)));
          Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor ((cp lsr  6) land 0b00111111)));
          Buffer.add_char buf (Char.unsafe_chr (0b10000000 lor (cp          land 0b00111111)));
          `Unescaped
        end else
          `Error "invalid escape sequence for utf-16 low surrogate"
    | `C cs,      _    -> `UTF16(x, `C (d::cs))
    | _, _             -> `Error "invalid escape sequence for utf-16 low surrogate"

  let str buf =
    let state : t ref = ref `Unescaped in
    skip_while (fun c ->
      match
        begin match !state with
        | `Unescaped    -> unescaped buf c
        | `Escaped      -> escaped   buf c
        | `UTF8 cs      -> utf_8     buf c cs
        | `UTF16(x, cs) -> utf_16    buf c x cs
        | (`Error _ | `Done) as state -> state
        end
      with
        | (`Error _) | `Done -> false
        | `Terminate         -> state := `Done; true
        | #t as state'       -> state := state'; true)
    >>= fun () ->
      match !state with
      | `Done ->
        let result = Buffer.contents buf in
        Buffer.clear buf;
        state := `Unescaped;
        return result
      | `Error msg ->
        Buffer.clear buf; state := `Unescaped; fail msg
      | `Unescaped | `Escaped | `UTF8 _ | `UTF16 _ ->
        Buffer.clear buf; state := `Unescaped; fail "unterminated string"
end

let json =
  let advance1 = advance 1 in
  let pair x y = (x, y) in
  let buf = Buffer.create 0x1000 in
  let str = S.str buf in
  fix (fun json ->
    let mem = lift2 pair (quo *> str <* ns) json in
    let obj = advance1 *> sep_by vs mem  <* rcb >>| fun ms -> `Object ms in
    let arr = advance1 *> sep_by vs json <* rsb >>| fun vs -> `Array  vs in
    let str = advance1 *> str >>| fun s -> `String s in
    ws *> peek_char_fail
    >>= function
      | 'f' -> _false
      | 'n' -> _null
      | 't' -> _true
      | '{' -> obj
      | '[' -> arr
      | '"' -> str
      | _   -> num) <?> "json"
