open Core.Std
open Async.Std
open Cohttp_async

(* Generate a DuckDuckGo search URI from a query string *)
let ddg_uri = 
  let uri = Uri.of_string ("http://api.duckduckgo.com/?format=json") in
  fun query ->
    Uri.add_query_param uri ("q", [query]) 

(* Extract the "Definition" field from the DuckDuckGo results *)
let get_definition_from_json json =
  match Yojson.Safe.from_string json with
  |`Assoc kv_list ->
      let open Option in
      List.Assoc.find kv_list "Definition" >>|
      Yojson.Safe.to_string
  |_ -> None

(* Execute the DuckDuckGo search *)
(* TODO: This client API is being simplified in Cohttp *)
let ddg_query query =
  Client.call `GET (ddg_uri query)
  >>= function
  | Some (res, Some body) ->
      let buf = Buffer.create 128 in
      Pipe.iter_without_pushback body ~f:(Buffer.add_string buf)
      >>| fun () ->
      get_definition_from_json (Buffer.contents buf) |!
      Option.value ~default:"???"
  | Some (_, None) | None ->
      failwith "no body in response"

(* Run a single search *)
let run_one_search =
  ddg_query "Camel" >>| prerr_endline

(* Run many searches in parallel *)
let run_many_searches =
  let searches = ["Duck"; "Sheep"; "Cow"; "Llama"; "Camel"] in
  Deferred.List.map ~how:`Parallel searches ~f:ddg_query >>|
  List.iter ~f:print_endline

(* Start the Async scheduler *)
let _ = Scheduler.go ()
