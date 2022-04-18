(* This script create the file [sentences/dune], which contains instructions
   for generating random sentences, and the file [dune], which contains
   instructions for running the benchmarks. *)

open Printf

let header f =
  fprintf f
    ";; This file has been auto-generated. Please do not edit it.\n\
     ;; Instead, edit [dune.ml].\n"

let ls dir =
  Sys.readdir dir
  |> Array.to_list

let filter_suffix files suffix =
  List.filter (fun f -> Filename.check_suffix f suffix) files

let remove_filter_suffix suffix files =
  filter_suffix files suffix
  |> List.map Filename.remove_extension

(* -------------------------------------------------------------------------- *)

(* Instructions for generating sentences are printed in sentences/dune. *)

type sentence =
  { name : string; seed : int option; size : int }

let generate n size =
  List.init n @@ fun i ->
    { name = sprintf "sentence_%d_%d" size i; seed = Some i; size }

let small : sentence list =
  let n, size = 500, 1000 in
  generate n size

let large : sentence list =
  let n, size = 5, 100000 in
  generate n size

let huge : sentence list =
  let n, size = 2, 1000000 in
  generate n size

let sentences =
  small @ large @ huge

let rule f { name; seed; size } =
  let init =
    match seed with
    | None ->
        "--random-self-init"
    | Some i ->
        sprintf "--random-seed %d" i
  in
  fprintf f {|
(rule
  (deps
    (:parser ../src/parser.mly))
  (target %s.tokens)
  (action
    (with-stdout-to %%{target}
      (run menhir %%{parser}
        --random-sentence main
        %s
        --random-sentence-length %d
))))
|}
    name
    init
    size

let () =
  let f = open_out "sentences/dune" in
  header f;
  List.iter (rule f) sentences;
  close_out f

(* -------------------------------------------------------------------------- *)

(* For each file matching [backends/*.flags], we create a backend. Each backend
   contains an executable file named [executable backend]. The main executable
   file [speed.exe] drives the whole thing. *)

let backends =
  ls "backends"
  |> remove_filter_suffix ".flags"

let filename sentence =
  sprintf "sentences/%s.tokens" sentence.name

let executable backend =
  sprintf "backends/%s.backend/main.exe" backend

let concat strings =
  strings
  |> List.map (fun s -> "    " ^ s ^ "\n")
  |> String.concat ""

let rule f backends sentences =
  let sentences = List.map filename sentences |> concat in
  let backends = List.map executable backends |> concat in
  fprintf f {|
(executable
  (name speed)
  (libraries unix)
)

(alias
  (name executables)
  (deps
%s    speed.exe)
)

(alias
  (name sentences)
  (deps
%s)
)

(rule
  (alias benchmark)
  (deps
    (alias executables)
    (alias sentences)
    speed.sh
    speed.exe)
  (action (run bash speed.sh))
)
|}
    backends
    sentences

let () =
  let f = open_out "dune" in
  header f;
  rule f backends sentences;
  close_out f
