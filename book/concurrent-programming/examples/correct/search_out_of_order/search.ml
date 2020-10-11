[@@@part "0"]

open Core
open Async

(* Generate a DuckDuckGo search URI from a query string *)
let query_uri query =
  let base_uri = Uri.of_string "http://api.duckduckgo.com/?format=json" in
  Uri.add_query_param base_uri ("q", [ query ])

[@@@part "1"]

(* Extract the "Definition" or "Abstract" field from the DuckDuckGo results *)
let get_definition_from_json json =
  match Yojson.Safe.from_string json with
  | `Assoc kv_list -> (
      let find key =
        match List.Assoc.find ~equal:String.equal kv_list key with
        | None | Some (`String "") -> None
        | Some s -> Some (Yojson.Safe.to_string s)
      in
      match find "Abstract" with Some _ as x -> x | None -> find "Definition" )
  | _ -> None

[@@@part "2"]

(* Execute the DuckDuckGo search *)
let get_definition word =
  Cohttp_async.Client.get (query_uri word) >>= fun (_, body) ->
  Cohttp_async.Body.to_string body >>| fun string ->
  (word, get_definition_from_json string)

[@@@part "3"]

(* Print out a word/definition pair *)
let print_result (word, definition) =
  printf "%s\n%s\n\n%s\n\n" word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    ( match definition with
    | None -> "No definition found"
    | Some def -> String.concat ~sep:"\n" (Wrapper.wrap (Wrapper.make 70) def)
    )

[@@@part "4"]

(* Run many searches in parallel, printing out the results as you go *)
let search_and_print words =
  Deferred.all_unit
    (List.map words ~f:(fun word -> get_definition word >>| print_result))

[@@@part "5"]

let () =
  Command.async ~summary:"Retrieve definitions from duckduckgo search engine"
    Command.Let_syntax.(
      let%map_open words = anon (sequence ("word" %: string)) in
      fun () -> search_and_print words)
  |> Command.run
