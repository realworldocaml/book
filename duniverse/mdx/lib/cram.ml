(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let src = Logs.Src.create "ocaml-mdx"

module Log = (val Logs.src_log src : Logs.LOG)
open Astring
open Misc

type t = {
  command : string list;
  output : Output.t list;
  exit_code : int;
  vpad : int;
}

let dump_line ppf = function
  | #Output.t as o -> Output.dump ppf o
  | `Exit i -> Fmt.pf ppf "`Exit %d" i
  | `Command c -> Fmt.pf ppf "`Command %S" c
  | `Command_first c -> Fmt.pf ppf "`Command_first %S" c
  | `Command_cont c -> Fmt.pf ppf "`Command_cont %S" c
  | `Command_last c -> Fmt.pf ppf "`Command_last %S" c

let dump ppf (t : t) =
  Fmt.pf ppf "{@[command: %a;@ output: %a;@ exit_code: %d@]}"
    Fmt.(Dump.list dump_string)
    t.command
    Fmt.(Dump.list Output.dump)
    t.output t.exit_code

let pp_vpad ppf t =
  let rec loop = function
    | 0 -> ()
    | n ->
        Fmt.pf ppf "\n";
        loop (Int.pred n)
  in
  loop t.vpad

let pp_command ?(pad = 0) ppf (t : t) =
  match t.command with
  | [] -> ()
  | l ->
      pp_vpad ppf t;
      let sep ppf () = Fmt.pf ppf "\\\n%a> " pp_pad pad in
      Fmt.pf ppf "%a$ %a\n" pp_pad pad Fmt.(list ~sep string) l

let pp_exit_code ?(pad = 0) ppf n =
  if n <> 0 then Fmt.pf ppf "%a[%d]\n" pp_pad pad n

let pp ?pad ppf (t : t) =
  pp_command ?pad ppf t;
  pp_lines (Output.pp ?pad) ppf t.output;
  pp_exit_code ?pad ppf t.exit_code

let hpad_of_lines = function
  | [] -> 0
  | h :: _ ->
      let i = ref 0 in
      while !i < String.length h && h.[!i] = ' ' do
        incr i
      done;
      !i

let of_lines ~syntax ~(loc : Location.t) t =
  let pos = loc.loc_start in
  let hpad =
    match syntax with Syntax.Mli -> pos.pos_cnum + 2 | _ -> hpad_of_lines t
  in
  let unpad line =
    match syntax with
    | Syntax.Mli -> String.trim line
    | _ ->
        if String.is_empty line then line
        else if String.length line < hpad then
          Fmt.failwith "invalid padding: %S" line
        else String.with_index_range line ~first:hpad
  in
  let lines = List.map unpad t in
  let lines =
    Lexer_cram.token (Lexing.from_string (String.concat ~sep:"\n" lines))
  in
  let vpad = match syntax with Syntax.Mli -> 1 | _ -> 0 in
  Log.debug (fun l ->
      l "Cram.of_lines (pad=%d) %a" hpad Fmt.(Dump.list dump_line) lines);
  let mk command output exit_code =
    { command; output = List.rev output; exit_code; vpad }
  in
  let rec command_cont acc = function
    | `Command_cont c :: t -> command_cont (c :: acc) t
    | `Command_last c :: t -> (List.rev (c :: acc), t)
    | _ -> Fmt.failwith "invalid multi-line command"
  in
  let rec aux command output acc = function
    | [] when command = [] -> List.rev acc
    | [] -> List.rev (mk command output 0 :: acc)
    | `Exit i :: t -> aux [] [] (mk command output i :: acc) t
    | (`Ellipsis as o) :: t -> aux command (o :: output) acc t
    | `Command cmd :: t ->
        if command = [] then aux [ cmd ] [] acc t
        else aux [ cmd ] [] (mk command output 0 :: acc) t
    | `Command_first cmd :: t ->
        let cmd, t = command_cont [ cmd ] t in
        aux cmd [] (mk command output 0 :: acc) t
    | (`Output _ as o) :: t -> aux command (o :: output) acc t
    | (`Command_last s | `Command_cont s) :: t ->
        aux command output acc (`Output s :: t)
  in
  match lines with
  | `Command_first cmd :: t ->
      let cmd, t = command_cont [ cmd ] t in
      (hpad, aux cmd [] [] t)
  | `Command cmd :: t -> (hpad, aux [ cmd ] [] [] t)
  | [] -> (0, [])
  | `Output line :: _ ->
      if String.length line > 0 && line.[0] = '$' then
        failwith
          "Blocks must start with a command or similar, not with an output \
           line. To indicate a line as a command, start it with a dollar \
           followed by a space."
      else
        failwith
          "Blocks must start with a command or similar, not with an output \
           line. Please, make sure that there's no spare empty line, \
           particularly between the output and its input."
  | _ -> Fmt.failwith "invalid cram block: %a" Fmt.(Dump.list dump_line) lines

let exit_code t = t.exit_code

(* http://tldp.org/LDP/abs/html/here-docs.html *)
let use_heredoc (t : t) = String.cut (List.hd t.command) ~sep:"<<" <> None

let command_line t =
  if not (use_heredoc t) then String.concat ~sep:" " t.command
  else String.concat ~sep:"\n" t.command
