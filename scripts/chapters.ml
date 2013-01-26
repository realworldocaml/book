open Core.Std
module Ascii_table = Core_extended.Std.Ascii_table
module Column = Ascii_table.Column

let load_chapters filename =
  Sexp.load_sexps_conv_exn filename
  <:of_sexp<Chapter.chapter>>
;;

let chapter_name = Column.create "Name" (fun x -> x.Chapter.name)
let public       = Column.create "Pub?" (fun x -> Bool.to_string x.Chapter.public)
let note         = Column.create "Note" (function x -> x.Chapter.note)

let print_chapter_summary filename =
  let chapters = load_chapters filename in
  Ascii_table.output  [chapter_name;public;note] chapters
    ~limit_width_to:150
    ~display:Ascii_table.Display.line
    ~oc:stdout


let summary =
  Command.basic
    ~summary:"Print out a chapter summary"
    Command.Spec.(empty +> anon ("filename" %: string))
    (fun filename () ->
      print_chapter_summary filename)

let command =
  Command.group ~summary:"Tools for working with the chapter summary"
    ["summary", summary]

let () =
  Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
