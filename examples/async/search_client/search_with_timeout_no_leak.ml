open Core.Std
open Async.Std

(* Generate a DuckDuckGo search URI from a query string *)
let query_uri ~server query =
  let base_uri =
    Uri.of_string (String.concat ["http://";server;"/?format=json"])
  in
  Uri.add_query_param base_uri ("q", [query])

(* Extract the "Definition" or "Abstract" field from the DuckDuckGo results *)
let get_definition_from_json json =
  match Yojson.Safe.from_string json with
  | `Assoc kv_list ->
    let find key =
      begin match List.Assoc.find kv_list key with
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
let get_definition ~stop ~server word =
  ignore stop;
  try_with (fun () ->
    Cohttp_async.Client.call `GET (query_uri ~server word)
    >>= function
    | None | Some (_, None) -> return (word, None)
    | Some (_, Some body) ->
      Pipe.to_list body >>| fun strings ->
      (word, get_definition_from_json (String.concat strings)))
  >>| function
  | Ok (word,result) -> (word, Ok result)
  | Error exn        -> (word, Error exn)


let get_definition_with_timeout ~server ~timeout word =
  let stop = after timeout in
  get_definition ~stop ~server word

let get_definition_with_timeout ~server ~timeout word =
  let stop = Ivar.create () in
  choose
    [ choice (after timeout)
        (fun () ->
           Ivar.fill stop ();
           (word,Error "Timed out"))
    ; choice (get_definition ~server word)
        (fun (word,result) ->
           let result' = match result with
             | Ok _ as x -> x
             | Error _ -> Error "Unexpected failure"
           in
           (word,result')
        )
    ]

let get_definition_with_timeout ~server ~timeout word =
  let stop = Ivar.create () in
  get_definition stop ~server word
  >>| fun (word,result) ->
  let result' = match result with
    | Ok _ as x -> x
    | Error _ -> Error "Unexpected failure"
  in
  (word,result')

(* Print out a word/definition pair *)
let print_result (word,definition) =
  printf "%s\n%s\n\n%s\n\n"
    word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    (match definition with
     | Error msg -> "ERROR: " ^ msg
     | Ok None -> "No definition found"
     | Ok (Some def) ->
       String.concat ~sep:"\n"
         (Wrapper.wrap (Wrapper.make 70) def))

(* Run many searches in parallel, printing out the results after they're all
   done. *)
let search_and_print ~server ~timeout words =
  Deferred.all (List.map words
                  ~f:(get_definition_with_timeout ~timeout ~server))
  >>| fun results ->
  List.iter results ~f:print_result

let () =
  Command.async_basic
    ~summary:"Retrieve definitions from duckduckgo search engine"
    Command.Spec.(
      empty
      +> anon (sequence ("word" %: string))
      +> flag "-server" (optional_with_default "api.duckduckgo.com" string)
           ~doc:" Specify server to connect to"
      +> flag "-timeout" (optional_with_default (sec 5.) time_span)
           ~doc:" Abandon queries that take longer than this time"
    )
    (fun words server timeout () ->
       search_and_print ~server ~timeout words)
  |> Command.run
