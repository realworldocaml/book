open Core
open Async

(* Generate a DuckDuckGo search URI from a query string *)
let query_uri ~server query =
  let base_uri =
    Uri.of_string
      (String.concat [ "http://"; server; "/?format=json" ])
  in
  Uri.add_query_param base_uri ("q", [ query ])

(* Extract the "Definition" or "Abstract" field from the DuckDuckGo
   results *)
let get_definition_from_json json =
  match Yojson.Safe.from_string json with
  | `Assoc kv_list ->
    let find key =
      match List.Assoc.find ~equal:String.equal kv_list key with
      | None | Some (`String "") -> None
      | Some s -> Some (Yojson.Safe.to_string s)
    in
    (match find "Abstract" with
    | Some _ as x -> x
    | None -> find "Definition")
  | _ -> None

[@@@part "1"]

(* Execute the DuckDuckGo search *)
let get_definition ~server ~interrupt word =
  match%map
    try_with (fun () ->
        let%bind _, body =
          Cohttp_async.Client.get ~interrupt (query_uri ~server word)
        in
        let%map string = Cohttp_async.Body.to_string body in
        word, get_definition_from_json string)
  with
  | Ok (word, result) -> word, Ok result
  | Error _ -> word, Error "Unexpected failure"

[@@@part "2"]

let get_definition_with_timeout ~server ~timeout word =
  let interrupt = Ivar.create () in
  choose
    [ choice (after timeout) (fun () ->
          Ivar.fill interrupt ();
          word, Error "Timed out")
    ; choice
        (get_definition ~server ~interrupt:(Ivar.read interrupt) word)
        (fun (word, result) ->
          let result' =
            match result with
            | Ok _ as x -> x
            | Error _ -> Error "Unexpected failure"
          in
          word, result')
    ]

[@@@part "3"]

(* Print out a word/definition pair *)
let print_result (word, definition) =
  printf
    "%s\n%s\n\n%s\n\n"
    word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    (match definition with
    | Error s -> "DuckDuckGo query failed: " ^ s
    | Ok None -> "No definition found"
    | Ok (Some def) ->
      String.concat ~sep:"\n" (Wrapper.wrap (Wrapper.make 70) def))

(* Run many searches in parallel, printing out the results after
   they're all done. *)
let search_and_print ~servers ~timeout words =
  let servers = Array.of_list servers in
  let%map results =
    Deferred.all
      (List.mapi words ~f:(fun i word ->
           let server = servers.(i mod Array.length servers) in
           get_definition_with_timeout ~server ~timeout word))
  in
  List.iter results ~f:print_result

let () =
  Command.async
    ~summary:"Retrieve definitions from duckduckgo search engine"
    (let%map_open.Command words = anon (sequence ("word" %: string))
     and servers =
       let string_list = Arg_type.create (String.split ~on:',') in
       flag
         "-servers"
         (optional_with_default [ "api.duckduckgo.com" ] string_list)
         ~doc:" Specify server to connect to"
     and timeout =
       flag
         "-timeout"
         (optional_with_default (sec 5.) Time_unix.Span.arg_type)
         ~doc:" Abandon queries that take longer than this time"
     in
     fun () -> search_and_print ~servers ~timeout words)
  |> Command_unix.run
