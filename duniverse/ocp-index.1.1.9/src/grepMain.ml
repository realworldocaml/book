(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the Lesser GNU Public License version 3.0.                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

module Grep : sig

  (* * [ident path, filename, input channel, [line, column, length]]
      Finds occurence of given ident in an opened file.
      Results are in reverse order. *)
  val ident: string list -> string -> in_channel -> (int * int * int) list

  (* * [regex, filename, input channel, [line, column, length]]
      Finds matches of a regexp in the strings of an opened ocaml file.
      Results are in reverse order. *)
  val strings_re: Re.re -> string -> in_channel -> (int * int * int) list

end = struct
  open Approx_tokens

  let curpath_update cp tok =
    let id = function
      | DOT -> `Dot
      | UIDENT s -> `Uid s
      | LIDENT s
      | INFIXOP0 s | INFIXOP1 s | INFIXOP2 s | INFIXOP3 s | INFIXOP4 s -> `Lid s
      | AMPERSAND -> `Lid "&"
      | AMPERAMPER -> `Lid "&&"
      | BARBAR -> `Lid "||"
      | COLONEQUAL -> `Lid ":="
      | MINUS -> `Lid "-"
      | MINUSDOT -> `Lid "-."
      | OR -> `Lid "or"
      | PLUS -> `Lid "+"
      | PLUSDOT -> `Lid "+."
      | QUESTIONQUESTION -> `Lid "??"
      | STAR -> `Lid "*"
      | BANG -> `Lid "!"
      | _ -> `None
    in
    match (id tok, cp) with
    | `None, _ -> []
    | `Dot, `Uid _ :: _ -> `Dot :: cp
    | `Dot, _ -> []
    | (`Uid _ | `Lid _) as c, `Dot :: cp -> c :: cp
    | (`Uid _ | `Lid _) as c, _ -> [c]
    | _ -> []

  let path_of_curpath = function
    | `Dot :: _ -> []
    | cp -> List.rev_map (function `Uid s | `Lid s -> s | `Dot -> assert false) cp

  let rec list_rm_pfx pfx l = match pfx, l with
    | px :: pr, x :: r when px = x -> list_rm_pfx pr r
    | [], l -> Some l
    | _ -> None

  (* Returns the list of paths that could match [path] within an env *)
  let rec possible_paths path envs =
    match envs with
    | IndexScope.Open p :: envs ->
        (match list_rm_pfx p path with
         | Some subpath -> possible_paths subpath envs
         | None -> []) @
        possible_paths path envs
    | IndexScope.Alias (id, p) :: envs ->
        (match list_rm_pfx p path with
         | Some subpath -> possible_paths (id :: subpath) envs
         | None -> []) @
        possible_paths path envs
    | [] -> [path]

  let ident path f ch =
    let modname =
      let s =
        Filename.basename
          (try Filename.chop_extension f with Invalid_argument _ -> f)
      in
#if OCAML_VERSION >= "4.03"
      String.mapi (function 0 -> Char.uppercase_ascii | _ -> fun x -> x) s
#elif OCAML_VERSION >= "4.02"
      String.mapi (function 0 -> Char.uppercase | _ -> fun x -> x) s
#else
      s.[0] <- Char.uppercase s.[0];
      s
#endif
    in
    let f (curpath, lookfor, last_scope, acc) scope tok pos =
      let lookfor =
        if scope == last_scope then lookfor
        else possible_paths path (IndexScope.to_list scope)
      in
      let curpath = curpath_update curpath tok in
      if curpath <> [] && List.hd curpath <> `Dot &&
         List.mem (path_of_curpath curpath) lookfor
      then
        curpath, lookfor, scope, pos :: acc
      else curpath, lookfor, scope, acc
    in
    let _, _, _, matches =
      IndexScope.fold f ([], [], IndexScope.empty, [])
        ~init:[IndexScope.Open ["Pervasives"]; IndexScope.Open [modname]]
        ch
    in
    matches

  let strings_re re _f ch =
    let line_column str ofs =
      let rec aux line i =
        let j = try String.index_from str i '\n' with Not_found -> ofs in
        if j >= ofs then line, ofs - i
        else aux (line + 1) (j + 1)
      in aux 0 0
    in
    let match_re s =
      let rec aux acc pos =
        let ofs =
          try Some (Re.get_ofs (Re.exec ~pos re s) 0) with Not_found -> None
        in
        match ofs with
        | None -> acc
        | Some ((_,pos) as m) -> aux (m :: acc) pos
      in
      aux [] 0
    in
    let ns = Nstream.of_channel ch in
    let rec aux ns matches =
      match Nstream.next ns with
      | Some ({Nstream.token=STRING _} as tok, ns) ->
          let s = Lazy.force (tok.Nstream.substr) in
          let s = String.sub s 1 (String.length s - 2) in (* remove "" *)
          let pos = Pos.Region.fst tok.Nstream.region in
          let orig_line, orig_col =
            Lexing.(pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)
          in
          let s_matches =
            List.rev_map (fun (ofs,ofs_end) ->
                let nl, col = line_column s ofs in
                let col = if nl = 0 then orig_col + ofs else col in
                (orig_line + nl, col, ofs_end - ofs)
              )
              (match_re s)
          in
          aux ns (List.rev_append s_matches matches)
      | Some (_, ns) -> aux ns matches
      | None -> matches
    in
    aux ns []
end

module Args = struct
  open Cmdliner

  let files_of_dir dirs =
    let skip d = match d.[0] with
      | '_' -> true
      | '.' -> d <> Filename.current_dir_name &&
               d <> Filename.parent_dir_name
      | _ -> false
    in
    List.fold_left (fun acc dir ->
        let files = Array.to_list (Sys.readdir dir) in
        List.map (Filename.concat dir)
          (List.filter (fun f ->
               (Filename.check_suffix f ".ml" ||
                Filename.check_suffix f ".mli" ||
                Filename.check_suffix f ".mll" ||
                Filename.check_suffix f ".mly") &&
               let c = f.[0] in c <> '.' && c <> '#')
              files)
        @ acc)
      [] (IndexMisc.unique_subdirs ~skip dirs)

  let pattern =
    let doc = "Fully qualified ident to search for \
               (eg. `List.map', `Set.Make.add', ...), \
               or string with option `-s'." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"ID" ~doc)

  let files =
    let doc = "Files or directories to search into. By default, searches for \
               project root" in
    let arg = Arg.(value & pos_right 0 file [] & info [] ~docv:"FILES" ~doc) in
    let get_files = function
      | [] ->
          let dir = match IndexMisc.project_root () with
            | Some d, _ -> d
            | None, _ -> Filename.current_dir_name
          in files_of_dir [dir]
      | fs ->
          let dirs, files = List.partition Sys.is_directory fs in
          files @ files_of_dir dirs
    in
    Term.(pure get_files $ arg)

  let color =
    let arg =
      let choices = Arg.enum
          [ "always", `Always;
            "never", `Never;
            "auto", `Auto; ]
      in
      let doc =
        "Colorize the output. $(docv) is either `always', `never' or `auto'."
      in
      Arg.(
        last & opt_all choices [`Auto] & info ["c";"color"] ~docv:"WHEN" ~doc
      )
    in
    let to_bool = function
      | `Always -> true
      | `Never -> false
      | `Auto -> Unix.isatty Unix.stdout
    in
    Term.(pure to_bool $ arg)

  let strings =
    let doc = "Search strings within strings in the source, \
               instead of searching code"
    in
    Arg.(value & flag & info ["s";"strings"] ~doc)

  let regexp =
    let doc = "Like `--strings', but search a POSIX regular expression." in
    Arg.(value & flag & info ["e";"regexp-strings"] ~doc)
end

let rec rmdups = function
  | x::(y::_ as l) when x = y -> rmdups l
  | x::r -> x :: rmdups r
  | [] -> []

let lines_of_file ch lines =
  let rec aux curline = function
    | [] -> []
    | l::r when l = curline ->
        let txt = input_line ch in
        (l, txt) :: aux (curline+1) r
    | lines ->
        while input_char ch <> '\n' do () done;
        aux (curline + 1) lines
  in
  seek_in ch 0;
  aux 1 (rmdups lines)

let print_line color =
  if not color then
    fun file _ (l,txt) -> Printf.printf "%s:%d:%s\n" file l txt
  else
    fun file matches (l,txt) ->
      let shortfile = IndexMisc.make_relative file in
      Printf.printf "%s\027[36m:\027[m%d\027[36m:\027[m" shortfile l;
      let m = List.rev (List.filter (fun (l1,_,_) -> l = l1) matches) in
      let len = String.length txt in
      let offs =
        List.fold_left (fun offs (_,col,toklen) ->
            print_string (String.sub txt offs (col - offs));
            print_string "\027[1;31m";
            print_string (String.sub txt col (min (len - col) toklen));
            print_string "\027[m";
            min len (col + toklen)) 0 m
      in
      print_endline (String.sub txt offs (String.length txt - offs))

let grep_file finder color file =
  try
    let ch = open_in file in
    let matches = finder file ch in
    (if matches <> [] then
       let lines = lines_of_file ch (List.rev_map (fun (l,_,_) -> l) matches) in
       List.iter (print_line color file matches) lines);
    close_in ch;
    matches <> []
  with Sys_error _ as e ->
    Printf.eprintf "%s: %s\n%!" file (Printexc.to_string e); false

let grep pattern files color strings regexp =
  if strings || regexp then
    let re =
      Re.Posix.compile
        (if regexp then Re.Posix.re pattern else Re.str pattern)
    in
    List.fold_left
      (fun found f -> grep_file (Grep.strings_re re) color f || found)
      false files
  else
    let path = IndexMisc.(key_to_modpath (string_to_key pattern)) in
    List.fold_left
      (fun found f -> grep_file (Grep.ident path) color f || found)
      false files

let () =
  let open Cmdliner in
  let doc = "Locates instances of a given OCaml ident in source files, \
             handling (local) opens, module, etc." in
  let man = [
    `S "BUGS";
    `P "Current version doesn't handle shadowing and different kinds of idents, \
        therefore you can get false positive if a type and a value have the \
        name.";
    `P "Field records don't currently respect the distributive `{Module.' \
        syntax. Also, if you use record disambiguation, you're on your own \
        for field names since this program doesn't know about typing.";
  ]
  in
  match
    Term.eval
      (Term.(pure grep
             $ Args.pattern $ Args.files $ Args.color $ Args.strings $ Args.regexp),
       Term.info "ocp-grep" ~version:"1.1.5" ~doc ~man)
  with
  | `Ok true -> exit 0
  | `Ok false -> exit 1
  | `Error _ -> exit 2
  | _ -> exit 0

(* idea: single utility to color parts of source with syntactic context:
   pattern, expr, type, topexpr, module, record ...
   Could be used for better completion, analysis etc.*)
