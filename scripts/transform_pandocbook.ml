(* This script rewrites the RWO Docbook file (originally generated
   by Pandoc, and adds <part> tags.  It can also filter public or
   private chapters to hide them for certain builds (such as the website) *)

open Core.Std
open Chapter

let part_to_string = function
  |Basic -> "I", "Basic Concepts"
  |Practical -> "II", "Practical Examples"
  |Advanced -> "III", "Advanced Topics"

let all_parts = [ Basic; Practical; Advanced ]

open Xml_tree
module Transform = struct

  (* Turn a <linkend> tag from Pandoc into an <xref> tag that OReilly want *)
  let rewrite_linkend it =
    let rec aux = function
    | Element ( (("","link"),[("","linkend"),v]), [Data "xref"]) ->
        Element ( (("","xref"),[("","linkend"),v]), [])
    | Element (tag, children) ->
        Element (tag, List.map ~f:aux children)
    | x -> x
    in aux it

  (* Assemble all the chapter tags separately from other tags.
   * Returns a tuple of the (id*chapter) and other tags. *)
  let split_chapters i =
    match i with
    |Element (((_,"book"),_), children) ->
      List.partition_map ~f:(function
        |Element ((("","chapter"),[("","id"),id]), _) as chapter ->
           `Fst (id,chapter)
        |other -> `Snd other
      ) children
    |_ -> failwith "<book> tag not found"

  (* Given a list of input chapters and the book XML, output
   * a book with <part> tags and the desired chapters *)
  let add_parts parts i =
    let chapters, other_tags = split_chapters i in
    let mk_chapter id =
      List.Assoc.find chapters id |! fun x ->
      Option.value_exn ~message:("Unable to find " ^ id) x
    in
    (* Construct a <part> tag *)
    let mk_part part =
      let label_num,label_name = part_to_string part in
      let title = mk_tag "title" [Data label_name] in
      let chapters = List.filter_map parts ~f:(fun c ->
        if c.part = part then Some (mk_chapter c.name) else None) in
      mk_tag ~attrs:["label",label_num] "part" (title :: chapters)
    in
    other_tags @ (List.map ~f:mk_part all_parts) |!
    mk_tag "book"
end

let apply_transform parts_file book public =
  let parts = Sexp.load_sexps_conv_exn parts_file chapter_of_sexp in
  let parts = if public then List.filter parts ~f:(fun c -> c.public) else parts in
  let o = Xmlm.make_output (`Channel stdout) in
  Xmlm.make_input (`Channel (open_in book)) |!
  in_tree |!
  fun (dtd, t) -> Transform.rewrite_linkend t |!
  Transform.add_parts parts |!
  fun t -> out_tree o (dtd, t)

open Cmdliner
let _ = 
  let parts = Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"CHAPTERS"
     ~doc:"Sexp list of chapters and the parts that they map onto. See $(b,type chapter) in $(i,add_parts.ml) for the format of this file.") in
  let book = Arg.(required & pos 1 (some non_dir_file) None & info [] ~docv:"DOCBOOK"
    ~doc:"Docbook source to transform with the $(i,<part>) tags. This file should have been output from $(b,pandoc).") in
  let public = Arg.(value & flag & info ["public"] ~doc:"Filter the chapter list to only output the ones marked as $(i,public) in the $(i,CHAPTERS) file.") in
  let info = Term.info "transform_pandocbook" ~version:"1.0.0" ~doc:"customise the Real World OCaml Docbook" in
  let cmd_t = Term.(pure apply_transform $ parts $ book $ public) in
  match Term.eval (cmd_t, info) with `Ok x -> x |_ -> exit 1

