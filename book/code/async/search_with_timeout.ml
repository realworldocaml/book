open Core
open Async

(* Extract the "Definition" or "Abstract" field from the DuckDuckGo results *)
let get_definition_from_json json =
  match Yojson.Safe.from_string json with
  | `Assoc kv_list ->
    let find key =
      begin match List.Assoc.find ~equal:String.equal kv_list key with
      | None | Some (`String "") -> None
      | Some s -> Some (Yojson.Safe.to_string s)
      end
    in
    begin match find "Abstract" with
    | Some _ as x -> x
    | None -> find "Definition"
    end
  | _ -> None

(* Execute the DuckDuckGo search *)
let get_definition ~server word =
  try_with (fun () ->
    Cohttp_async.Client.get (query_uri ~server word)
    >>= fun  (_, body) ->
    Pipe.to_list body
    >>| fun strings ->
    (word, get_definition_from_json (String.concat strings)))
  >>| function
  | Ok (word,result) -> (word, Ok result)
  | Error _          -> (word, Error "Unexpected failure")

[@@@part "1"];;
let get_definition_with_timeout ~server ~timeout word =
  Deferred.any
    [ (after timeout >>| fun () -> (word,Error "Timed out"))
    ; (get_definition ~server word
       >>| fun (word,result) ->
       let result' = match result with
         | Ok _ as x -> x
         | Error _ -> Error "Unexpected failure"
       in
       (word,result')
      )
    ]

[@@@part "2"];;
(* Print out a word/definition pair *)
let print_result (word,definition) =
  printf "%s\n%s\n\n%s\n\n"
    word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    (match definition with
     | Error s -> "DuckDuckGo query failed: " ^ s
     | Ok None -> "No definition found"
     | Ok (Some def) ->
       String.concat ~sep:"\n"
         (Wrapper.wrap (Wrapper.make 70) def))

(* Run many searches in parallel, printing out the results after they're all
   done. *)
let search_and_print ~servers ~timeout words =
  let servers = Array.of_list servers in
  Deferred.all (List.mapi words ~f:(fun i word ->
    let server = servers.(i mod Array.length servers) in
    get_definition_with_timeout ~server ~timeout word))
  >>| fun results ->
  List.iter results ~f:print_result

let () =
  Command.async
    ~summary:"Retrieve definitions from duckduckgo search engine"
    Command.Spec.(
      empty
      +> anon (sequence ("word" %: string))
      +> flag "-servers"
           (optional_with_default ["api.duckduckgo.com"] string_list)
           ~doc:" Specify server to connect to"
      +> flag "-timeout" (optional_with_default (sec 5.) time_span)
           ~doc:" Abandon queries that take longer than this time"
    )
    (fun words servers timeout () -> search_and_print ~servers ~timeout words)
  |> Command.run
