open Core.Std
open Async.Std
module Cohttp = Cohttp_async

(* Generate a DuckDuckGo search URI from a query string *)
let query_uri =
  let base_uri = Uri.of_string "http://api.duckduckgo.com/?format=json" in
  fun query -> Uri.add_query_param base_uri ("q", [query])

(* Extract the "Definition" field from the DuckDuckGo results *)
let get_definition_from_json json =
  match Yojson.Safe.from_string json with
  | `Assoc kv_list ->
    begin match List.Assoc.find kv_list "Definition" with
    | None | Some (`String "") -> None
    | Some s -> Some (Yojson.Safe.to_string s)
    end
  | _ -> None

(* Execute the DuckDuckGo search *)
let get_definition word =
  Cohttp.Client.call `GET (query_uri word)
  >>= function
  | None | Some (_, None) -> return None
  | Some (_, Some body) ->
    Pipe.to_list body >>| fun strings ->
    get_definition_from_json (String.concat strings)

(* Run a single search and print out the results *)
let run_one_search search_string =
  get_definition search_string >>| fun result ->
  printf "%-10s : %s\n" search_string
    (Option.value ~default:"No definition found" result)

(* Run many searches in parallel, printing out the results as you go *)
let run_many_searches ~parallel search_strings =
  Deferred.List.iter search_strings ~f:run_one_search
    ~how:(if parallel then `Parallel else `Sequential)

let () =
  Command.async_basic
    ~summary:"Retrieve definitions from duckduckgo search engine"
    Command.Spec.(
      empty
      +> flag "-parallel" no_arg ~doc:" Run queries in parallel"
      +> anon (sequence ("search term" %: string))
    )
    (fun parallel search_strings () ->
      run_many_searches ~parallel search_strings)
  |> Command.run
