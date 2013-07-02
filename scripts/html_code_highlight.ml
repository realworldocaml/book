open Core.Std
open Xml_tree

let subst_with_code_frag c =
  let open Code_frag in
  try
    let t = of_string c in
    let frag = In_channel.read_all (sprintf "code/_build/%s.%d.html" t.name t.part) in
    let i = Xmlm.make_input ~entity:Xhtml.entity (`String (0,frag)) in
    let (_dtd,it) = in_tree i in
    Some it
  with exn -> prerr_endline (Exn.to_string exn); None

let rec filter i =
  match i with
  | Element ((("","pre"),attr),c) as e -> begin
      match List.Assoc.find attr ("","id") with
      | None -> e
      | Some _pid -> begin
          (* If there is a language attr, then it isn't a frag, so don't try to parse it *)
          match List.Assoc.find attr ("","language") with
          | Some _ -> e
          | None -> begin
            let buf = String.concat ~sep:"" (List.map ~f:Xml_tree.to_string c) in
            match subst_with_code_frag buf with
            | None -> e
            | Some it -> it
          end
        end
    end
  | Element (p,c) -> Element (p, List.map ~f:filter c)
  | Data _ as d -> d

let () =
  try
    let i = Xmlm.make_input ~entity:Xhtml.entity (`Channel stdin) in
    let o = Xmlm.make_output ~decl:false (`Channel stdout) in
    let (_dtd,it) = in_tree i in
    let ot = filter it in
    out_tree o (None,ot)
  with Xmlm.Error (_p,e) -> print_endline (Xmlm.error_message e)
