(* This script rewrites the RWO Docbook file (originally generated
   by Pandoc, and adds <part> tags.  It can also filter public or
   private chapters to hide them for certain builds (such as the website) *)

open Core.Std
open Chapter

let string_of_data d =
  let b = Buffer.create 100 in
  List.iter ~f:(function
    |`Data x -> Buffer.add_string b x
    |_ -> failwith "unexpected tag in data"
  ) d;
  Buffer.contents b

let mk_tag ?(attrs=[]) tag_name contents =
  let attrs : Xmlm.attribute list = List.map ~f:(fun (k,v) -> ("",k),v) attrs in
  let tag = ("", tag_name), attrs in
  `El (tag, contents)

let part_to_string = function
  | Basic     -> "I"   , "Language Concepts"
  | Practical -> "II"  , "Tools and Techniques"
  | Advanced  -> "III" , "The Runtime System"
  | Appendix | Preface -> assert false

let part_to_intro =
  let map_paras = List.map ~f:(fun p -> mk_tag "para" [`Data p]) in
  function
  | Basic     -> map_paras [
    "Part I covers the basic language concepts you'll need to know when building
     OCaml programs.  You won't need to memorise all of this (objects, for
     example, are used rarely in practice) but understanding the concepts and
     examples is important.";
    "This part opens up with a guided tour to give you a quick overview of the
     language using an interactive command-line interface. It then moves onto
     covering language features such as records, algebraic data types and the
     module system.";
    "The final portion covers more advanced features such as functors, objects
     and first-class modules, which may all take some time to digest. Persevere
     though; even though these concepts may be difficult at first, they will put
     you in good stead even when switching to other languages, many of which
     have drawn inspiration from ML."
  ]
  | Practical  -> map_paras [
    "Part II builds on the basics by working through higher-level tools and
     techniques for using OCaml effectively.  Here you'll learn about useful
     libraries for building command-line interfaces and networked applications,
     as well as functional design patterns that help combine different features
     of the language to good effect.";
    "The focus throughout this section is on networked systems, and among other
     examples we'll build a simple tool that will perform Internet queries using
     the DuckDuckGo search engine."
  ]
  | Advanced -> map_paras [
    "Part III is all about understanding the compiler toolchain and runtime
     system in OCaml.  It's a remarkably simple system in comparison to other
     language runtimes (such as Java or the .NET CLR).";
    "You'll need to read this to build very high performance systems that have
     to minimise resource usage or interface to C libraries. This is also where
     we talk about profiling and debugging techniques using tools such as GNU
     gdb.";
  ]
  | Appendix | Preface -> assert false

let all_parts = [ Basic; Practical; Advanced ]

module Transform = struct

  (* Turn a <linkend> tag from Pandoc into an <xref> tag that OReilly want *)
  let rewrite_linkend it =
    let rec aux = function
      | `El ( (("","link"),[("","linkend"),v]), [`Data "xref"]) ->
        `El ( (("","xref"),[("","linkend"),v]), [])
      | `El (tag, children) ->
        `El (tag, List.map ~f:aux children)
      | x -> x
    in aux it

  (* Turn a <chapter> tag into a new (e.g. <appendix> or <preface> tag *)
  let rewrite_chapter_to_other_tag new_tag it =
    let rec aux = function
      | `El ((("","chapter"),i), c) -> `El ((("",new_tag),i),c)
      | `El (tag, children) ->
        `El (tag, List.map ~f:aux children)
      | x -> x
    in aux it

  (* Assemble all the chapter tags separately from other tags.
   * Returns a tuple of the (id*chapter) and other tags. *)
  let split_chapters i =
    match i with
    |`El (((_,"book"),_), children) ->
      List.partition_map ~f:(function
          |`El ((("","chapter"),[("","id"),id]), _) as chapter ->
            `Fst (id,chapter)
          |other -> `Snd other
        ) children
    |_ -> failwith "<book> tag not found"

  (* Substitute a <programlisting> tag with the appropriate code block *)
  let rewrite_programlisting (it:Cow.Xml.t) =
    let rec aux = function
      | `El ( (("","programlisting"),_), contents) ->
        let open Code_frag in
        let cf = of_string (string_of_data contents) in
        Cow.Xml.of_string (read ~ext:"xml" cf)
      | `El (tag, children) ->
        let cs = List.concat (List.map ~f:aux children) in
        [`El (tag, cs)]
      | `Data x-> [`Data x]
    in List.concat (List.map ~f:aux it)

  (* Given a list of input chapters and the book XML, output
   * a book with <part> tags and the desired chapters.
   * If [public] is true then only show public chapters and stub out rest *)
  let add_parts public parts i =
    let chapters, other_tags = split_chapters i in
    let mk_chapter id =
      List.Assoc.find chapters id |! fun x ->
      Option.value_exn ~message:("Unable to find " ^ id) x
    in
    let mk_appendix id = mk_chapter id |! rewrite_chapter_to_other_tag "appendix" in
    let mk_preface id = mk_chapter id |! rewrite_chapter_to_other_tag "preface" in
    (* Construct a <part> tag *)
    let mk_part part =
      let label_num,label_name = part_to_string part in
      let intro_elements = part_to_intro part in
      let title = mk_tag "title" [`Data label_name] in
      let intro = mk_tag "partintro" intro_elements in
      let chapters =
        List.filter_map parts ~f:(fun c ->
            if c.part = part then begin
              match c.public, public with
              |false, true ->
                let title = mk_tag "title" [`Data (Option.value ~default:"???" c.title)] in
                let para = mk_tag "para" [`Data "This chapter is under construction and not yet ready for public review, and so omitted from this milestone. This chaper is present as a placeholder so that cross-references from other chapters resolve correctly in the print version."] in
                Some (mk_tag ~attrs:["id",c.name] "chapter" [ title; para ])
              |_ -> Some (mk_chapter c.name)
            end
            else None
          )
      in
      mk_tag ~attrs:["label",label_num] "part" (title :: intro :: chapters)
    in
    let appendices =
      List.filter_map parts ~f:(fun c ->
          if c.part = Appendix then Some (mk_appendix c.name) else None) in
    let prefaces =
      List.filter_map parts ~f:(fun c ->
          if c.part = Preface then Some (mk_preface c.name) else None) in
    other_tags @ prefaces @ (List.map ~f:mk_part all_parts) @ appendices |!
    mk_tag "book"
end

let apply_transform parts_file book public plsubst =
  try
    let parts = Sexp.load_sexps_conv_exn parts_file chapter_of_sexp in
    Xmlm.make_input (`Channel (open_in book))
    |> Xml_tree.in_tree
    |> fun (dtd, t) ->    Transform.rewrite_linkend t
                       |> Transform.add_parts public parts
                       |> fun d ->
                          (if plsubst then Transform.rewrite_programlisting [d]
                           else [d])
                       |> fun t -> print_endline (Option.value_exn dtd); print_endline (Cow.Xml.to_string t)
  with Xmlm.Error ((line,col),e) -> (
      Printf.eprintf "ERROR: [%d,%d] %s\n%!" line col (Xmlm.error_message e);
      exit 1)

open Cmdliner
let _ =
  let parts = Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"CHAPTERS"
                     ~doc:"Sexp list of chapters and the parts that they map onto. See $(b,type chapter) in $(i,add_parts.ml) for the format of this file.") in
  let book = Arg.(required & pos 1 (some non_dir_file) None & info [] ~docv:"DOCBOOK"
                    ~doc:"Docbook source to transform with the $(i,<part>) tags. This file should have been output from $(b,pandoc).") in
  let public = Arg.(value & flag & info ["public"] ~doc:"Filter the chapter list to only output the ones marked as $(i,public) in the $(i,CHAPTERS) file.") in
  let plsubst = Arg.(value & flag & info ["subst"] ~doc:"Substitute $(i,<programlisting>) tags with their frag values. For OReilly backend only.") in
  let info = Term.info "transform_pandocbook" ~version:"1.0.0" ~doc:"customise the Real World OCaml Docbook" in
  let cmd_t = Term.(pure apply_transform $ parts $ book $ public $ plsubst) in
  match Term.eval (cmd_t, info) with `Ok x -> x |_ -> exit 1
