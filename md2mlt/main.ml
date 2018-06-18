exception Error of string

let (/) = Filename.concat

let err fmt = Fmt.kstrf (fun str -> raise (Error str)) fmt

type script = [
  | `OCaml of Expect.Raw_script.t
  | `OCaml_toplevel of Expect.Mlt.t
  | `OCaml_rawtoplevel of Expect.Raw_script.t
  | `Shell of Expect.Cram.t
  | `Other of string
]

type script_part = [
  | `OCaml of Expect.Raw_script.part
  | `OCaml_toplevel of Expect.Chunk.t list
  | `OCaml_rawtoplevel of Expect.Raw_script.part
  | `Shell of Expect.Cram.t
  | `Other of string
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

let read_script filename: script =
  match Filename.extension filename with
  | ".mlt" ->
    let script = Expect.Mlt.of_file ~filename in
    `OCaml_toplevel script
  | ".ml" | ".mli" | ".mly" | ".mll" ->
    (* Hack: Oloop.Script.of_file intended only for ml files but
       happens to work for mli, mll, and mly files. *)
    let script = Expect.Raw_script.of_file ~filename in
    `OCaml script
  | ".rawscript" ->
    let script = Expect.Raw_script.of_file ~filename in
    `OCaml_rawtoplevel script
  | ".sh" | ".errsh" ->
    let script = Expect.Cram.of_file ~filename in
    if Expect.Cram.is_empty script then
      Fmt.pr "warning: %s is empty\n%!" filename;
    `Shell script
  | ".jbuild" -> `Other (File.read filename)
  | _ -> `Other (File.read filename)

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
      | x -> `OCaml_toplevel (Expect.Part.chunks x)
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
  | `Other s as x -> if name = "" then x else err_no_part ()

let link t ?part href =
  let part = read_part t ?part ("examples" / href) in
  { href; part; }

let dump_part ppf = function
  | `OCaml p -> Expect.Raw_script.dump_part ppf p
  | `OCaml_toplevel e -> Fmt.Dump.list Expect.Chunk.dump ppf e
  | `OCaml_rawtoplevel p -> Expect.Raw_script.dump_part ppf p
  | `Shell s -> Expect.Cram.dump ppf s
  | `Other s -> Fmt.string ppf s

let pp_part ppf = function
  | `OCaml c -> Fmt.string ppf c.Expect.Raw_script.content
  | `OCaml_toplevel c -> Fmt.(list ~sep:(unit "\n") Expect.Chunk.pp) ppf c
  | `OCaml_rawtoplevel c -> Fmt.string ppf c.Expect.Raw_script.content
  | `Shell s -> Expect.Cram.pp ppf s
  | `Other s -> Fmt.string ppf s

let dump_item ppf = function
  | Lines t -> Fmt.pf ppf "Line@ (@[<2>%a@])" Fmt.(Dump.list string) t
  | Link l ->
    Fmt.pf ppf "{@[<2>@ href: %S;@ part: %a@]}" l.href dump_part l.part

let pp_lines ppf l =
  let str = Fmt.to_to_string Fmt.(list ~sep:(unit "\n") string) l in
  Fmt.string ppf (String.trim str)

let pp_item ppf = function
  | Lines t -> Fmt.pf ppf "[%%md {|\n%a\n|}];;" pp_lines t
  | Link t  -> pp_part ppf t.part

let dump ppf t = Fmt.Dump.list pp_item ppf t.items
let pp ppf t = Fmt.pf ppf "%a\n" Fmt.(list ~sep:(unit "\n") pp_item) t.items

module Html = struct

  type tree = E of Xmlm.tag * tree list | D of string

  let parse s =
    let el tag childs = E (tag, childs)  in
    let data d = D d in
    let i = Xmlm.make_input (`String (0, s)) in
    try  let _, d = Xmlm.input_doc_tree ~el ~data i in d
    with Xmlm.Error (_, e) -> D s

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

  let text l = Lines [l]

  let link t e =
    match Html.attrs e with
    | ["href", href; "rel", "import"] -> link t href
    | ["href", href; "part", p; "rel", "import"] -> link t ~part:p href
    | _ -> err "invalid link: %a" Html.dump e

  let item t s =
    let n = Html.parse s in
    match Html.name n with
    | Some "link" -> Link (link t n)
    | _ -> text s

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
    let items = List.map (item t) lines in
    let items = normalize items in
    let scripts = Hashtbl.fold (fun _ l acc -> l :: acc) t.files [] in
    { items; scripts }

end

let run input =
  let output = (Filename.remove_extension input) ^ ".mlt" in
  Fmt.pr "Generating %s\n%!" output;
  try
    let oc = open_out output in
    let ppf = Format.formatter_of_out_channel oc in
    let t = Parse.file input in
    Fmt.pf ppf "%a%!" pp t;
    close_out oc;
  with Error e ->
    Fmt.epr "%s: %s\n%!" input e;
    exit 1

let () =
  let input =
    if Array.length Sys.argv = 2 then Sys.argv.(1)
    else (
      Fmt.epr "usage: md2mlt <file>";
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
