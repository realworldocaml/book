open Core.Std

type tree = 
  | Element of Xmlm.tag * tree list
  | Data of string

let in_tree i = 
  let el tag children = Element (tag, children) in
  let data d = Data d in
  Xmlm.input_doc_tree ~el ~data i

let name ((_,n),_) = n

let filter_tag n =                                                       
  List.fold_left ~init:[] ~f:(fun acc ->                                                
    function
    |Element (tag, ts) when name tag = n -> ts @ acc                           
    |_ -> acc
  )
     
let concat_data =                                                        
  List.fold_left ~init:"" ~f:(fun acc ->
    function
    |Data s -> acc ^ s
    |_ -> acc                           
  )

let topics trees =
  filter_tag "DuckDuckGoResponse" trees |!
  filter_tag "RelatedTopics" |!
  filter_tag "RelatedTopic" |!
  filter_tag "Text" |!
  List.iter ~f:(fun x -> concat_data [x] |! print_endline)

let _ =
  let i = Xmlm.make_input (`Channel (open_in "ddg.xml")) in
  let o = Xmlm.make_output (`Channel stdout) in
  let (_,it) = in_tree i in
  topics [it]
