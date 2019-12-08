open! Core
open! Async
include Expect_test_helpers

let pipe commands =
  List.map commands ~f:(fun (prog, args) ->
    String.concat ~sep:" " (List.map (prog :: args) ~f:Filename.quote))
  |> String.concat ~sep:" | "
  |> sprintf "set -o pipefail; %s"
  |> system
;;

let links =
  [ "../bin/patdiff.exe", `In_path_as, "patdiff"
  ; "../../ansicodes/bin/main.exe", `In_path_as, "ansicodes"
  ]
;;

let patdiff ~extra_flags ~prev ~next =
  within_temp_dir ~links (fun () ->
    let%bind () = Writer.save "prev" ~contents:prev
    and () = Writer.save "next" ~contents:next in
    pipe
      [ "patdiff", [ "-default"; "prev"; "next" ] @ extra_flags
      ; "ansicodes", [ "visualize"; "-minimize" ]
      ])
;;
