(*
 Add <part> tags to organise the <chapter> tags better
*)

let basic = [
 "prologue"; 
 "a-guided-tour";
 "variables-and-functions"; 
 "lists-options-and-patterns";
 "records";
 "variants" ]

let practical = [
 "data-serialization-with-json-xml-and-s-expressions";
 "error-handling";
 "imperative-programming-1";
 "files-modules-and-programs";
 "functors-and-first-class-modules";
 "input-and-output";
 "concurrent-programming-with-async";
 "object-oriented-programming"
]

let advanced = [
 "understanding-the-runtime-system";
 "managing-external-memory-with-bigarrays";
 "inside-the-runtime";
 "performance-tuning-and-profiling";
 "packaging-and-build-systems";
 "parsing-with-ocamllex-and-ocamlyacc";
 "installation"
]

let parts = [
  "basic-concepts","Basic Concepts", basic;
  "practical-examples","Practical Examples", practical;
  "advanced-topics","Advanced Topics", advanced
]

open Xml_tree

let transform it =
  let rec aux = function
    | Element ( (("","link"),[("","linkend"),v]), [Data "xref"]) ->
        Element ( (("","xref"),[("","linkend"),v]), [])
    | Element (tag, children) ->
        Element (tag, List.map aux children)
    | x -> x
  in
  aux it

let get_chapters i =
  let chapters = Hashtbl.create 13 in
  match i with
  |Element (((_,"book"),_), children) ->
    let others = List.filter (
      function
      |Element ((("","chapter"),[("","id"),id]), contents) as chapter ->
         Hashtbl.add chapters id chapter;
         false
      |_ -> true
    ) children in
    chapters, others
  |_ -> failwith "<book> tag not found"

(* Output the new XML with the <part> tags added *)
let output_book chapters others =
  (* Consume a chapter tag from the list of chapters *)
  let mk_chapter id =
    let chapter = Hashtbl.find chapters id in
    Hashtbl.remove chapters id;
    chapter
  in
  (* Construct a <part> tag *)
  let mk_part (label,title,ids) = 
    let title = mk_tag "title" [Data title] in
    mk_tag ~attrs:["label",label] "part"  
    (title :: (List.map mk_chapter ids))
  in
  (* Build the book and include the misc tags from the original parse *)
  let contents = others @ (List.map mk_part parts) in
  (* Verify that the chapters list is empty, or something is left over *)
  if Hashtbl.length chapters > 0 then begin
    prerr_endline "WARNING! We found some chapters that aren't in the add_parts.ml scripts";
    Hashtbl.iter (fun k v -> prerr_endline k) chapters;
    exit 1;
  end;
  mk_tag "book" contents

let t it =
  let chapters, others = get_chapters it in
  output_book chapters others
