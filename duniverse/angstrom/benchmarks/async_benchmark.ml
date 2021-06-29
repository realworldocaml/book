open Async

let main parser () =
  let toss _ = Deferred.unit in
  let reader = Lazy.force Reader.stdin in
  let parser =
    match parser with
    | `Http -> Angstrom.(RFC2616.request >>| fun x -> `Http x)
    | `Json -> Angstrom.(RFC7159.json    >>| fun x -> `Json x)
  in
  Angstrom_async.parse_many parser toss reader
  >>| function
    | Ok () -> ()
    | Error err -> failwith err
;;

let () =
  let parser = Command.Arg_type.of_alist_exn ["http", `Http; "json", `Json] in
  Command.(async_spec ~summary:"async benchmark"
    Spec.(empty +> Param.(anon ("PARSER" %: parser))) main |> run)
