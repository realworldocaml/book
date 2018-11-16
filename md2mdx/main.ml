exception Error of string

let (/) = Filename.concat

let err fmt = Fmt.kstrf (fun str -> raise (Error str)) fmt

type script = [
  | `OCaml of Expect.Raw_script.t
  | `OCaml_toplevel of Expect.Mlt.t
  | `OCaml_rawtoplevel of Expect.Raw_script.t
  | `Shell of Expect.Cram.t
  | `Other of (string * string)
]

type script_part = [
  | `OCaml of Expect.Raw_script.part
  | `OCaml_toplevel of Expect.Chunk.t
  | `OCaml_rawtoplevel of Expect.Raw_script.part
  | `Shell of Expect.Cram.t
  | `Other of (string * string)
]

type link = {
  href: string;
  part: script_part;
}

type item =
  | Lines of string list
  | Link  of link

type t = {
  items  : item list;
  scripts: script list;
}

let exists_ml_file dir =
  let files = Sys.readdir dir in
  Array.exists (fun f -> Filename.extension f = ".ml") files

let read_script filename: script =
  let remove () =
    if not (Filename.basename filename = "dune"
            && exists_ml_file (Filename.dirname filename))
    then Unix.unlink filename
  in
  match Filename.extension filename with
  | ".mlt" ->
    let script = Expect.Mlt.of_file ~filename in
    remove ();
    `OCaml_toplevel script
  | ".ml" | ".mli" | ".mly" | ".mll" ->
    (* Hack: Oloop.Script.of_file intended only for ml files but
       happens to work for mli, mll, and mly files. *)
    let script = Expect.Raw_script.of_file ~filename in
    `OCaml script
  | ".rawscript" ->
    let script = Expect.Raw_script.of_file ~filename in
    remove ();
    `OCaml_rawtoplevel script
  | ".sh" | ".errsh" ->
    let script = Expect.Cram.of_file ~filename in
    remove ();
    if Expect.Cram.is_empty script then
      Fmt.pr "warning: %s is empty\n%!" filename;
    `Shell script
  | _ ->
    let file = File.read filename in
    remove ();
    `Other (filename, file)

type cache = {
  files: (string, script) Hashtbl.t;
}

let empty () = { files = Hashtbl.create 17 }

let read t filename =
  try Hashtbl.find t.files filename
  with Not_found ->
    let s = read_script filename in
    Hashtbl.add t.files filename s;
    s

let is_rawpart ~name p = name = p.Expect.Raw_script.name
let is_part ~name p = name = Expect.Part.name p

let read_part t ?part:(name="") filename: script_part =
  let err_no_part () =
    Fmt.failwith "no data for requested part of file %s:%s" filename name
  in
  match read t filename with
  | `OCaml parts -> (
      match List.find (is_rawpart ~name) parts with
      | exception Not_found -> err_no_part ()
      | x -> `OCaml x
    )
  | `OCaml_toplevel doc -> (
      match List.find (is_part ~name) (Expect.Mlt.parts doc) with
      | exception Not_found -> err_no_part ()
      | x -> `OCaml_toplevel (Expect.Chunk.of_part ~filename x)
    )
  | `OCaml_rawtoplevel parts -> (
      match List.find (is_rawpart ~name) parts with
      | exception Not_found -> err_no_part ()
      | x -> `OCaml_rawtoplevel x
    )
  | `Shell parts -> (
      if name = "" then `Shell parts
      else match Expect.Cram.part name parts with
        | None   -> err_no_part ()
        | Some x -> `Shell x
    )
  | `Other _ as x -> if name = "" then x else err_no_part ()

let link t ?part href =
  let part = read_part t ?part ("examples" / href) in
  { href; part; }

let pp_header ?part ?dir ?filename ?non_det ?env () ppf =
  let dir = match dir with
    | None     -> []
    | Some dir -> ["dir", dir]
  in
  let env = match env with
    | None   -> []
    | Some e -> ["env",e]
  in
  let part = match filename, part with
    | Some file, Some ""   -> ["file", ".." / ".." / file]
    | Some file, Some part -> ["file", ".." / ".." / file; "part", part]
    | _ -> []
  in
  let non_det = match non_det with
    | None -> []
    | Some `Command -> ["non-deterministic", "command"]
    | Some `Output  -> ["non-deterministic", "output"]
  in
  match dir@part@non_det@env with
  | [] -> ()
  | l  ->
    let pp_bindings ppf (k, v) = Fmt.pf ppf "%s=%s" k v in
    Fmt.pf ppf " %a"  Fmt.(list ~sep:(unit ",") pp_bindings) l

let pp_cram ppf c =
  let filename = Expect.Cram.filename c in
  let pp_line ppf = function
    | `Part _ | `Non_det _ -> ()
    | `Output s  -> Fmt.pf ppf "%s\n" s
    | `Command s -> Fmt.pf ppf "$ %s\n" s
    | `Ellipsis  -> Fmt.pf ppf "...\n"
    | `Comment s -> Fmt.pf ppf "# %s\n" s
    | `Exit_code n -> Fmt.pf ppf "[%d]\n" n
  in
  let rec aux (non_det, blocks) = function
    | [] -> non_det, List.rev blocks
    | Cram.Test t :: rest ->
      let non_det = match non_det, t.non_deterministic with
        | Some `Command, _ | _ , `Command -> Some `Command
        | Some `Output , _ | _ , `Output  -> Some `Output
        | None, `False -> None
      in
      let s = Fmt.strf "%a" Fmt.(list ~sep:(unit "") pp_line) t.lines in
      aux (non_det, s :: blocks) rest
    | Cram.Line l :: rest ->
      let s = Fmt.strf "%a" pp_line l in
      aux (non_det, s :: blocks) rest
  in
  let non_det, blocks = aux (None, []) (Expect.Cram.v c) in
  let dir = ".." / ".." / Filename.dirname filename in
  Fmt.pf ppf "```sh%t\n%a```\n"
    (pp_header ~filename ~dir ?non_det ())
    Fmt.(list ~sep:(unit "") string) blocks

let pp_mlt ppf c =
  let env =
    Filename.remove_extension (Filename.basename (Expect.Chunk.filename c))
  in
  Fmt.pf ppf "```ocaml%t\n%a```\n" (pp_header ~env ()) Expect.Chunk.pp c

let pp_part ppf (c:script_part) = match c with
  | `OCaml { Expect.Raw_script.name; filename; content } ->
    Fmt.pf ppf "```ocaml%t\n%s\n```\n"
      (pp_header ~part:name ~filename ())
      (String.trim content)
  | `OCaml_toplevel c -> pp_mlt ppf c
  | `OCaml_rawtoplevel c ->
    Fmt.pf ppf "```ocaml\n%s\n```\n" (String.trim c.Expect.Raw_script.content)
  | `Shell s -> pp_cram ppf s
  | `Other (f, s) ->
    match Filename.basename f with
    | "dune" -> Fmt.pf ppf "```scheme%t\n%s\n```\n" (pp_header ~filename:f ()) s
    | _ ->
      let kind = match Filename.extension f with
        | ".java" -> "java"
        | ".cpp"  -> "cpp"
        | _ -> ""
      in
      Fmt.pf ppf "```%s\n%s\n```\n" kind s

let pp_lines ppf l =
  let str = Fmt.to_to_string Fmt.(list ~sep:(unit "\n") string) l in
  Fmt.string ppf (String.trim str)

let pp_item ppf = function
  | Lines t -> Fmt.pf ppf "%a\n" pp_lines t
  | Link t  -> pp_part ppf t.part

let pp ppf t = Fmt.pf ppf "%a\n" Fmt.(list ~sep:(unit "\n") pp_item) t.items

module Html = struct

  type tree = E of Xmlm.tag * tree list | D of string

  let parse s =
    let el tag childs = E (tag, childs)  in
    let data d = D d in
    let i = Xmlm.make_input (`String (0, s)) in
    try  let _, d = Xmlm.input_doc_tree ~el ~data i in d
    with Xmlm.Error (_, _) -> D s

  let rec dump ppf = function
    | E (t, tr) -> Fmt.pf ppf "@[E (%a,@ %a)@]" Xmlm.pp_tag t Fmt.(Dump.list dump) tr
    | D s -> Fmt.pf ppf "%S" s

  let name: tree -> string option = function
    | E ((n, _), _) -> Some (snd n)
    | _ -> None

  let attrs = function
    | D _ -> []
    | E ((_, a), _) ->
      let compare (a, _) (b, _) = String.compare a b in
      let s = List.map (fun ((_, k), v) -> k, v) a in
      List.sort compare s

end

module Parse = struct

  open Astring

  let text l = Lines [l]

  let link t e =
    match Html.attrs e with
    | ["href", href; "rel", "import"] -> link t href
    | ["href", href; "part", p; "rel", "import"] -> link t ~part:p href
    | _ -> err "invalid link: %a" Html.dump e

  let slugify s =
    let wrote_something = ref false in
    let last_char_is_dash = ref false in
    let in_tag = ref false in
    let s = String.Ascii.lowercase s in
    let buf = Buffer.create (String.length s) in
    String.iter
      (function
        | ('a' .. 'z' | '0' .. '9' | '-' | '.') as c ->
          if !in_tag then ()
          else (
            if !wrote_something && !last_char_is_dash then (
              Buffer.add_char buf '-';
              last_char_is_dash := false;
            );
            wrote_something := true;
            Buffer.add_char buf c
          )
        | '<' -> in_tag := true
        | '>' -> in_tag := false
        | '/' -> ()
        | _   -> if !wrote_something then last_char_is_dash := true)
      s;
    Buffer.contents buf

  let item ~filename t s =
    let n = Html.parse s in
    match Html.name n with
    | Some "link" -> Link (link t n)
    | _ ->
      if String.length s > 1 && s.[0] = '#' then
        match String.cuts ~sep:" " s with
        | level::title ->
          let id, title = match List.rev title with
            | []   -> Fmt.failwith "%s: invalid title: %s" filename s
            | h::t ->
              if String.length h > 3
              && h.[0] = '{'
              && h.[1] = '#' && h.[String.length h - 1] = '}'
              then
                let id =
                  String.with_range h ~first:2 ~len:(String.length h - 3)
                in
                Some id, List.rev t
              else
                None, title
          in
          let id = match id with
            | None    -> []
            | Some id ->
              let id' = slugify (String.concat ~sep:" " title) in
              if id=id' then []
              else (
                Fmt.pr "%s: non consistent title:\n  %s\n  %s\n%!"
                  filename id id';
                [Fmt.strf "{#%s}"id]
              )
          in
          Lines [String.concat ~sep:" " (level::title @ id)]
        | _ -> text s
      else
        text s

  let is_code_block x =
    match String.cut x ~sep:"```" with
    | Some (x, _) -> String.trim x = ""
    | _ -> false

  let rec code_block acc = function
    | []   -> failwith "non terminated code block"
    | h::t ->
      if is_code_block h then (
        let block = String.concat ~sep:"\n" (List.rev (h::acc)) in
        block, t
      ) else code_block (h::acc) t

  let merge lines =
    let rec aux acc = function
      | []  -> List.rev acc
      | [x] -> List.rev (x :: acc)
      | x::(y::t as t') ->
        if is_code_block x then
          let block, rest = code_block [x] t' in
          aux (block :: acc) rest
        else if String.length x > 1
             && String.length y > 1
             && x.[0] = '<' && y.[0] = '"'
        then
          aux ((x ^ " " ^ y) :: acc) t
        else
          aux (x :: acc) t'
    in
    aux [] lines

  let normalize l =
    let return lines acc = match lines with
      | [] -> acc
      | _  -> List.rev (Lines (List.flatten (List.rev lines)) :: acc)
    in
    let rec aux lines acc = function
      | []           -> return lines acc
      | Lines l :: t -> aux (l :: lines) acc t
      | Link l  :: t  ->
        aux [] (Link l :: Lines (List.flatten (List.rev lines)) :: acc) t
    in
    aux [] [] l

  let file f =
    let t = empty () in
    let lines = File.read_lines f in
    let lines = merge lines in
    let items = List.map (item ~filename:f t) lines in
    let items = normalize items in
    let scripts = Hashtbl.fold (fun _ l acc -> l :: acc) t.files [] in
    { items; scripts }

end

let run input =
  let t = Parse.file input in
  Unix.unlink input;
  let dir = Filename.remove_extension input in
  let output = dir / "README.md" in
  Fmt.pr "Generating %s\n%!" output;
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o755;
  try
    let oc = open_out output in
    let ppf = Format.formatter_of_out_channel oc in
    Fmt.pf ppf "%a%!" pp t;
    close_out oc;
  with Error e ->
    Fmt.epr "%s: %s\n%!" input e;
    exit 1

let () =
  let input =
    if Array.length Sys.argv = 2 then Sys.argv.(1)
    else (
      Fmt.epr "usage: md2mdx <file>";
      exit 1
    ) in
  match input with
  | "--all" | "-a" ->
    let dir = Unix.opendir "book" in
    let files = ref [] in
    let (/) = Filename.concat in
    let rec loop () =
      try
        let file = Unix.readdir dir in
        if Filename.extension file = ".md"
        && Filename.(extension (remove_extension file)) = ""
        then
          files := ("book" / file) :: !files;
        loop ()
      with End_of_file ->
        List.sort String.compare !files
    in
    List.iter run (loop ())
  | _ -> run input
