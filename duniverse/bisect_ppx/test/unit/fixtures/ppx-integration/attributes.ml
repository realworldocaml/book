let () =
  (let x = 1 in x) [@testing] |> ignore;

  (fun x -> x) [@testing] |> ignore;

  (string_of_int 0) [@testing] |> ignore;

  (match 0 with
  | 0 -> ()
  | _ -> ()) [@testing];

  (function
  | 0 -> 0
  | x -> x) [@testing] |> ignore;

  (try string_of_int 0 |> ignore
  with _ -> ()) [@testing];

  (if true then () else ()) [@testing]
