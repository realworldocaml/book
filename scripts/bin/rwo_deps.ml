open Core
open Async
let (/) = Filename.concat

module Html : sig
  type item = Nethtml.document
  type t = item list
  val of_string : string -> t
  val of_file : string -> t Deferred.t

  val chapter_files : string -> string list Deferred.t
end = struct
  type item = Nethtml.document
  type t = item list

  let of_string s =
    Netchannels.with_in_obj_channel
      (new Netchannels.input_string s)
      (Nethtml.parse ~dtd:[])

  let of_file file =
    Reader.file_contents file >>| of_string

  let is_chapter_file file : bool =
    Filename.basename file |>
    String.split ~on:'-' |>
    List.hd_exn |> fun x ->
    try ignore (Int.of_string x); true
    with _ -> false

  let chapter_files repo_root =
    let dir = repo_root/"book" in
    Sys.readdir dir >>| fun a ->
    Array.to_list a |>
    List.filter ~f:is_chapter_file

end

module Deps : sig

  (** Return dependencies of given chapter file. *)
  val of_chapter : string -> string list Deferred.t

end = struct

  let of_chapter_html (html:Html.t) =
    let rec loop accum = function
      | [] -> accum
      | (Nethtml.Data _)::html -> loop accum html
      | (Nethtml.Element ("link", attrs, childs))::html ->
        (
          match List.Assoc.find ~equal:String.equal attrs "href" with
          | Some x -> loop (loop (x::accum) childs) html
          | None -> loop (loop accum childs) html
        )
      | (Nethtml.Element (_,_,childs))::html ->
        loop (loop accum childs) html
    in
    loop [] html |>
    List.dedup_and_sort ~compare:(Pervasives.compare)

  let of_chapter file =
    Html.of_file file >>| of_chapter_html

end


(******************************************************************************)
(* `deps` command                                                             *)
(******************************************************************************)
let deps_site : Command.t = 
    let open Async.Command.Let_syntax in
    Command.async
    ~summary:"print build dependencies for site files"
    [%map_open
      let repo_root =
        Async.Command.Param.(let default = "./" in
        let doc = sprintf "dir Root of repository. Default: \"%s\"." default in
        flag "-repo-root" (optional_with_default default file) ~doc)
        in
        (fun () ->
           Html.chapter_files repo_root >>=
           Deferred.List.iter ~f:(fun chapter_file ->
               (
                 Deps.of_chapter (repo_root/"book"/chapter_file) >>| fun l ->
                 List.map l ~f:(fun x -> "examples"/x) |> fun l ->
                 String.concat l ~sep:" "
               ) >>| fun dependencies ->
               printf "site/%s: %s\n\n" chapter_file dependencies
             )
        )
    ]

let deps : Command.t = Command.group
    ~summary:"print build dependencies"
    [
      "site", deps_site;
    ]

(******************************************************************************)
(* `main` command                                                             *)
(******************************************************************************)
let main = Command.group
  ~summary:"Real World OCaml authoring and publication tools"
  [
    "deps", deps;
  ]

;;
try Command.run main
with e -> eprintf "%s\n" (Exn.to_string e)
