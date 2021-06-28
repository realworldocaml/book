(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Astring

(* Errors *)

let err_malformed_pat s =
  strf "malformed named string pattern: %a" String.dump s

(* Patterns *)

type lexeme = Lit of string | Var of string
type t = lexeme list

let empty = []
let dom p =
  let add acc = function Lit _ -> acc | Var v -> String.Set.add v acc in
  List.fold_left add String.Set.empty p

let equal p p' = p = p'
let compare p p' = Pervasives.compare p p'

type parse_state = S_lit | S_dollar | S_var

let of_string s =
  let b = Buffer.create 255 in
  let flush b = let s = Buffer.contents b in (Buffer.clear b; s) in
  let err () = R.error_msg (err_malformed_pat s) in
  let push_lit b acc =
    if Buffer.length b <> 0 then Lit (flush b) :: acc else acc
  in
  let max_i = String.length s - 1 in
  let rec loop acc state i =
    if i > max_i then
      if state <> S_lit then err () else (Ok (List.rev (push_lit b acc)))
    else match state with
    | S_lit ->
        begin match s.[i] with
        | '$' -> loop acc S_dollar (i + 1)
        | c -> Buffer.add_char b c; loop acc S_lit (i + 1)
        end
    | S_dollar ->
        begin match s.[i] with
        | '$' -> Buffer.add_char b '$'; loop acc S_lit (i + 1)
        | '(' -> loop (push_lit b acc) S_var (i + 1)
        | _ -> err ()
        end
    | S_var ->
        begin match s.[i] with
        | ')' -> loop (Var (flush b) :: acc) S_lit (i + 1);
        | ',' -> err ()
        | c -> Buffer.add_char b c; loop acc S_var (i + 1)
        end
  in
  loop [] S_lit 0

let v s = R.error_msg_to_invalid_arg (of_string s)

let to_string p =
  let b = Buffer.create 255 in
  let add = function
  | Lit l ->
      let max_i = String.length l - 1 in
      let rec loop start i =
        if i > max_i then Buffer.add_substring b l start (i - start) else
        if l.[i] <> '$' then loop start (i + 1) else
        begin
          Buffer.add_substring b l start (i - start + 1);
          Buffer.add_char b '$';
          let next = i + 1 in loop next next
        end
      in
      loop 0 0
  | Var v -> Buffer.(add_string b "$("; add_string b v; add_char b ')')
  in
  List.iter add p;
  Buffer.contents b

let escape_dollar s =
  let len = String.length s in
  let max_idx = len - 1 in
  let rec escaped_len i l =
    if i > max_idx then l else
    match String.unsafe_get s i with
    | '$' -> escaped_len (i + 1) (l + 2)
    | _ -> escaped_len (i + 1) (l + 1)
  in
  let escaped_len = escaped_len 0 0 in
  if escaped_len = len then s else
  let b = Bytes.create escaped_len in
  let rec loop i k =
    if i > max_idx then Bytes.unsafe_to_string b else
    match String.unsafe_get s i with
    | '$' ->
        Bytes.unsafe_set b k '$'; Bytes.unsafe_set b (k + 1) '$';
        loop (i + 1) (k + 2)
    | c ->
        Bytes.unsafe_set b k c;
        loop (i + 1) (k + 1)
  in
  loop 0 0

let rec pp ppf = function
| [] -> ()
| Lit l :: p -> Fmt.string ppf (escape_dollar l); pp ppf p
| Var v :: p -> Fmt.pf ppf "$(%s)" v; pp ppf p

let dump ppf p =
  let rec dump ppf = function
  | [] -> ()
  | Lit l :: p ->
      Fmt.string ppf (String.Ascii.escape_string (escape_dollar l)); pp ppf p
  | Var v :: p ->
      Fmt.pf ppf "$(%s)" v; pp ppf p
  in
  Fmt.pf ppf "\"%a\"" dump p

(* Substitution *)

type defs = string String.map

let subst ?(undef = fun _ -> None) defs p =
  let subst acc = function
  | Lit _ as l -> l :: acc
  | Var v as var ->
      match String.Map.find v defs with
      | Some lit -> (Lit lit) :: acc
      | None ->
          match undef v with
          | Some lit -> (Lit lit) :: acc
          | None -> var :: acc
  in
  List.(rev (fold_left subst [] p))

let format ?(undef = fun _ -> "") defs p =
  let b = Buffer.create 255 in
  let add = function
  | Lit l -> Buffer.add_string b l
  | Var v ->
      match String.Map.find v defs with
      | Some s -> Buffer.add_string b s
      | None -> Buffer.add_string b (undef v)
  in
  List.iter add p;
  Buffer.contents b

(* Matching
   N.B. matching is not t.r. but stack is bounded by number of variables. *)

let match_literal pos s lit =              (* matches [lit] at [pos] in [s]. *)
  let l_len = String.length lit in
  let s_len = String.length s - pos in
  if l_len > s_len then None else
  try
    for i = 0 to l_len - 1 do if lit.[i] <> s.[pos + i] then raise Exit done;
    Some (pos + l_len)
  with Exit -> None

let match_pat ~env pos s pat =
  let init, no_env = match env with
  | None -> Some String.Map.empty, true
  | Some m as init -> init, false
  in
  let rec loop pos = function
  | [] -> if pos = String.length s then init else None
  | Lit lit :: p ->
      begin match (match_literal pos s lit) with
      | None -> None
      | Some pos -> loop pos p
      end
  | Var n :: p ->
      let rec try_match next_pos =
        if next_pos < pos then None else
        match loop next_pos p with
        | None -> try_match (next_pos - 1)
        | Some m as r ->
            if no_env then r else
            Some (String.Map.add n
                  (String.with_index_range s ~first:pos ~last:(next_pos - 1)) m)
      in
      try_match (String.length s) (* Longest match first. *)
  in
  loop pos pat

let matches p s = (match_pat ~env:None 0 s p) <> None
let query ?(init = String.Map.empty) p s = match_pat ~env:(Some init) 0 s p

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
