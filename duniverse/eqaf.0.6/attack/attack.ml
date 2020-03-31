open Microtime

let cycles = 1000
let length = 32
let range = [| "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "a"; "b"; "c"; "d"; "e"; "f"; |]

external random_seed : unit -> int array = "caml_sys_random_seed"

let pp_int_array ppf arr =
  Fmt.pf ppf "[|" ;
  for i = 0 to pred (Array.length arr) do Fmt.pf ppf "%d;" arr.(i) done ;
  Fmt.pf ppf "|]"

let () =
  let random_seed = random_seed () in
  Fmt.pr "Random: %a.\n%!" pp_int_array random_seed ;
  Random.full_init random_seed

let random length =
  let get _ = range.(Random.int (Array.length range)).[0] in
  String.init length get

exception Diff

let equal a b =
  let ln = (min : int -> int -> int) (String.length a) (String.length b) in
  try
    for i = 0 to pred ln do if not (Char.equal a.[i] b.[i]) then raise_notrace Diff ; Unix.sleepf 0.0001 done ;
    String.length a = String.length b
  with Diff -> false

let stabilize_garbage_collector () =
  let rec go limit last_heap_live_words =
    if limit <= 0 then failwith "Unable to stabilize the number of live words in the heap" ;
    Gc.compact () ;
    let stat = Gc.stat () in
    if stat.Gc.live_words <> last_heap_live_words
    then go (pred limit) stat.Gc.live_words in
  go 10 0

let compute a b =
  let t0 = microtime () in
  for _ = 0 to pred cycles do ignore (equal a b) done ;
  let t1 = microtime () in

  t1 - t0

let rec run hash prefix =
  let timers = Hashtbl.create cycles in
  for i = 0 to pred (Array.length range)
  do
    let m = prefix ^ range.(i) in
    stabilize_garbage_collector () ;
    Gc.compact () ;
    Gc.minor () ;
    let r = Sys.opaque_identity (compute m hash) in
    Hashtbl.add timers m r ;
  done ;

  let results = Hashtbl.fold (fun k v a -> (k, v) :: a) timers [] in
  let results = List.sort (fun (_, v0) (_, v1) -> (compare : int -> int -> int) v1 v0) results in

  match results with
  | [] -> assert false
  | (hit, _) :: _ ->
    if String.length hit = length
    then hit
    else run hash hit

let exit_success = 0
let exit_failure = 1

let () =
  let hash = random length in
  print_endline hash ;
  let cracked = run hash "" in
  print_endline cracked ;

  if String.equal hash cracked then exit exit_success else exit exit_failure
