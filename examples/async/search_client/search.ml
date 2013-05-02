open Core.Std
open Async.Std
module Cohttp = Cohttp_async

(* Generate a DuckDuckGo search URI from a query string *)
let query_uri =
  let base_uri = Uri.of_string "http://api.duckduckgo.com/?format=json" in
  fun query -> Uri.add_query_param base_uri ("q", [query])

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
let get_definition word =
  Cohttp.Client.call `GET (query_uri word)
  >>= function
  | None | Some (_, None) -> return (word, None)
  | Some (_, Some body) ->
    Pipe.to_list body >>| fun strings ->
    (word, get_definition_from_json (String.concat strings))

let wrap text max_width =
  let rec build completed_lines current_line current_line_length words =
    match words with
    | hd :: tl ->
      let new_line_length = current_line_length + 1 + String.length hd in
      if new_line_length > max_width && not (List.is_empty current_line) then
        build (current_line :: completed_lines) [] 0 words
      else
        build completed_lines (hd :: current_line) new_line_length tl
    | [] ->
      List.rev_map ~f:List.rev (current_line :: completed_lines)
      |> List.map ~f:(String.concat ~sep:" ")
      |> String.concat ~sep:"\n"
  in
  build [] [] 0 (String.split ~on:' ' text)

let print_result (word,definition) =
  printf "%s\n%s\n\n%s\n\n"
    word
    (String.init (String.length word) ~f:(fun _ -> '-'))
    (match definition with
    | None -> "No definition found"
    | Some def -> wrap def 70)

(* Run a single search and print out the results *)
let run_one_search word =
  get_definition word >>| print_result

(* Run many searches in parallel, printing out the results as you go *)
let run_many_searches ~parallel words =
  Deferred.List.map words ~f:get_definition
    ~how:(if parallel then `Parallel else `Sequential)
  >>| fun results ->
  List.iter results ~f:print_result

let () =
  Command.async_basic
    ~summary:"Retrieve definitions from duckduckgo search engine"
    Command.Spec.(
      empty
      +> flag "-parallel" no_arg ~doc:" Run queries in parallel"
      +> anon (sequence ("word" %: string))
    )
    (fun parallel words () ->
      run_many_searches ~parallel words)
  |> Command.run
