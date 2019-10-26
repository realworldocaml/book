let test_oneline = function
  | None -> "included"
  | Some true when Random.bool () -> (*BISECT-IGNORE*) "ignored"
  | Some true -> (*BISECT-IGNORE*) "ignored"
  | Some false when Random.bool () -> (*BISECT-VISIT*) "visited"
  | Some false -> (*BISECT-VISIT*) "visited"

let test_oneline_multipat = function
  | None -> "included"
  | Some 1 | Some 2 | Some 3 -> (*BISECT-IGNORE*) "ignored"
  | Some 4 | Some 5 | Some 6 -> (*BISECT-VISIT*) "visited"
  | Some _ -> "included"

let test_oneline_split = function
  | None -> "included"
  | Some true when Random.bool () -> (*BISECT-IGNORE*)
    "ignored"
  | Some true -> (*BISECT-IGNORE*)
    "ignored"
  | Some false when Random.bool () -> (*BISECT-VISIT*)
    "visited"
  | Some false -> (*BISECT-VISIT*)
    "visited"

let test_multiline = function
  | None -> "included"
(*BISECT-IGNORE-BEGIN*)
  | Some 1 | Some 2 | Some 3 ->
    "ignored"
(*BISECT-IGNORE-END*)
(*BISECT-IGNORE-BEGIN*)
  | Some _ ->
    "ignored"
(*BISECT-IGNORE-END*)
