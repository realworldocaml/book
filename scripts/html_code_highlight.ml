(* Figure out all the paragraph identifiers and comment on github issues with them (for context) *)
open Core.Std
open Xml_tree

let rec filter i =
  match i with
  |Element ((("","pre"),attr),c) as e -> begin
     match List.Assoc.find attr ("","id") with
     |None -> e
     |Some pid -> begin
        match List.Assoc.find attr ("","language") with
        |None -> e
        |Some lang ->
          (* The contents of <pre> are just Data tags, so filter them through Pygments *)
          let ic,oc = Unix.open_process (Printf.sprintf "pygmentize -l %s -f html" lang) in
          List.iter c ~f:(function Data x -> Out_channel.output_string oc x |_ -> assert false);
          Out_channel.close oc;
          let html = In_channel.input_all ic in
          let htmli = Xmlm.make_input (`String (0,html)) in
          let _,preit = in_tree htmli in
          preit
     end
  end
  |Element (p,c) -> Element (p, List.map ~f:filter c)
  |Data _ as d -> d

let _ =
  try
    let i = Xmlm.make_input ~entity:Xhtml.entity (`Channel stdin) in
    let o = Xmlm.make_output (`Channel stdout) in
    let (dtd,it) = in_tree i in
    let ot = filter it in
    out_tree o (dtd,ot)
  with Xmlm.Error (p,e) -> print_endline (Xmlm.error_message e)

