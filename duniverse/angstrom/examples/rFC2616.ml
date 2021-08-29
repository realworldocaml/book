open Angstrom

module P = struct
  let is_space =
    function | ' ' | '\t' -> true | _ -> false

  let is_eol =
    function | '\r' | '\n' -> true | _ -> false

  let is_hex =
    function | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

  let is_digit =
    function '0' .. '9' -> true | _ -> false

  let is_separator =
    function
      | ')' | '(' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"'
      | '/' | '[' | ']' | '?' | '=' | '{' | '}' | ' ' | '\t' -> true
      | _ -> false

  let is_token =
    (* The commented-out ' ' and '\t' are not necessary because of the range at
     * the top of the match. *)
    function
      | '\000' .. '\031' | '\127'
      | ')' | '(' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"'
      | '/' | '[' | ']' | '?' | '=' | '{' | '}' (* | ' ' | '\t' *) -> false
      | _ -> true
end

let token = take_while1 P.is_token
let digits = take_while1 P.is_digit
let spaces = skip_while P.is_space

let lex p = p <* spaces

let version =
  string "HTTP/" *>
  lift2 (fun major minor -> major, minor)
    (digits <* char '.')
    digits

let uri =
  take_till P.is_space

let meth = token
let eol = string "\r\n"

let request_first_line =
  lift3 (fun meth uri version -> (meth, uri, version))
    (lex meth)
    (lex uri)
    version

let response_first_line =
  lift3 (fun version status msg -> (version, status, msg))
    (lex version)
    (lex (take_till P.is_space))
    (take_till P.is_eol)

let header =
  let colon = char ':' <* spaces in
  lift2 (fun key value -> (key, value))
    token
    (colon *> take_till P.is_eol)

let request =
  lift2 (fun (meth, uri, version) headers -> (meth, uri, version, headers))
    (request_first_line   <* eol)
    (many (header <* eol) <* eol)

let response =
  lift2 (fun (version, status, msg) headers -> (version, status, msg, headers))
    (response_first_line  <* eol)
    (many (header <* eol) <* eol)
