(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult

(* Command line fragments *)

type t = string list

let empty = []
let is_empty = function [] -> true | _ -> false
let v a = [a]
let ( % ) l a = a :: l
let ( %% ) l0 l1 = List.rev_append (List.rev l1) l0
let add_arg l a = l % a
let add_args l a = l %% a
let on bool l = if bool then l else []
let p = Fpath.to_string

(* Command lines *)

let line_tool l = match List.rev l with [] -> None | t :: _ -> Some t
let get_line_tool l = match List.rev l with
| t :: _ -> t
| [] -> invalid_arg "the command is empty"

let line_args l = match List.rev l with
| _ :: args -> args
| [] -> []

(* Deprecated *)

let line_exec = line_tool
let get_line_exec = get_line_tool

(* Predicates and comparison *)

let equal l l' = l = l'
let compare l l' = Pervasives.compare l l'

(* Conversions and pretty printing *)

(* Parsing is loosely based on
   http://pubs.opengroup.org/onlinepubs/009695399/utilities/\
   xcu_chap02.html#tag_02_03 *)

let parse_cmdline s =
  try
    let err_unclosed kind s =
      failwith @@
      strf "%d: unclosed %s quote delimited string"
        (String.Sub.start_pos s) kind
    in
    let skip_white s = String.Sub.drop ~sat:Char.Ascii.is_white s in
    let tok_sep c = c = '\'' || c = '\"' || Char.Ascii.is_white c in
    let tok_char c = not (tok_sep c) in
    let not_squote c = c <> '\'' in
    let parse_squoted s =
      let tok, rem = String.Sub.span ~sat:not_squote (String.Sub.tail s) in
      if not (String.Sub.is_empty rem) then tok, String.Sub.tail rem else
      err_unclosed "single" s
    in
    let parse_dquoted acc s =
      let is_data = function '\\' | '"' -> false | _ -> true in
      let rec loop acc s =
        let data, rem = String.Sub.span ~sat:is_data s in
        match String.Sub.head rem with
        | Some '"' -> (data :: acc), (String.Sub.tail rem)
        | Some '\\' ->
            let rem = String.Sub.tail rem in
            begin match String.Sub.head rem with
            | Some ('"' | '\\' | '$' | '`' as c) ->
                let acc = String.(sub (of_char c)) :: data :: acc in
                loop acc (String.Sub.tail rem)
            | Some ('\n') -> loop (data :: acc) (String.Sub.tail rem)
            | Some c ->
                let acc = String.Sub.extend ~max:2 data :: acc in
                loop acc (String.Sub.tail rem)
            | None ->
                err_unclosed "double" s
            end
        | None -> err_unclosed "double" s
        | Some _ -> assert false
      in
      loop acc (String.Sub.tail s)
    in
    let parse_token s =
      let ret acc s = String.Sub.(to_string @@ concat (List.rev acc)), s in
      let rec loop acc s = match String.Sub.head s with
      | None -> ret acc s
      | Some c when Char.Ascii.is_white c -> ret acc s
      | Some '\'' ->
          let tok, rem = parse_squoted s in loop (tok :: acc) rem
      | Some '\"' ->
          let acc, rem = parse_dquoted acc s in loop acc rem
      | Some c ->
          let sat = tok_char in
          let tok, rem = String.Sub.span ~sat s in loop (tok :: acc) rem
      in
      loop [] s
    in
    let rec loop acc s =
      if String.Sub.is_empty s then acc else
      let token, s = parse_token s in
      loop (token :: acc) (skip_white s)
    in
    Ok (loop [] (skip_white (String.sub s)))
  with Failure err -> R.error_msgf "command line %a:%s" String.dump s err

let of_string s = parse_cmdline s
let to_string l = String.concat ~sep:" " (List.rev_map Filename.quote l)

let to_list line = List.rev line
let of_list ?slip line = match slip with
| None -> List.rev line
| Some slip -> List.fold_left (fun acc v -> v :: slip :: acc) [] line

let of_values ?slip conv vs = match slip with
| None -> List.rev_map conv vs
| Some slip -> List.fold_left (fun acc v -> conv v :: slip :: acc) [] vs

let pp ppf cmd = match List.rev cmd with
| [] -> ()
| cmd :: [] -> Fmt.(pf ppf "%s" cmd)
| cmd :: args -> Fmt.(pf ppf "@[<2>%s@ %a@]" cmd (list ~sep:sp string) args)

let dump ppf cmd =
  let pp_arg ppf a = Fmt.pf ppf "%s" (Filename.quote a) in
  Fmt.pf ppf "@[<1>[%a]@]" Fmt.(list ~sep:sp pp_arg) (List.rev cmd)

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
