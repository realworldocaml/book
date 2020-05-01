type t = V : (unit -> 'a) -> t

let stabilize_garbage_collector () =
  let rec go limit last_heap_live_words =
    if limit <= 0 then failwith "Unable to stabilize the number of live words in the heap" ;
    Gc.compact () ;
    let stat = Gc.stat () in
    if stat.Gc.live_words <> last_heap_live_words
    then go (pred limit) stat.Gc.live_words in
  go 10 0

let runnable f i =
  for _ = 1 to i do ignore @@ Sys.opaque_identity (f ()) done [@@inline]

let tmp = Bytes.create 40

let reset () = Bytes.fill tmp 0 40 ' '

let print ppf (n, m) =
  let l = n * 40 / m in
  Bytes.fill tmp 0 l '#' ;
  Fmt.pf ppf "[%s] %d%%%!" (Bytes.unsafe_to_string tmp) (n * 100 / m)

let samples = 750

let run t =
  let idx = ref 0 in
  let run = ref 0 in
  let (V fn) = t in

  let m = Array.create_float (samples * 2) in

  reset () ;
  Fmt.pr "%a" print (0, samples) ;

  while !idx < samples do
    let current_run = !run in
    let current_idx = !idx in

    (* XXX(dinosaure): GC and prints can put noise on our samples.
       - GC is not predictable
       - prints can add a latency on I/O *)
    (* Fmt.pr "\r%a" print (current_idx, samples) ; *)
    (* if current_run = 0 then stabilize_garbage_collector () ; *)

    let time_0 = Clock.tick () in

    runnable fn current_run ;

    let time_1 = Clock.tick () in

    m.((current_idx * 2) + 0) <- float_of_int current_run ;
    m.((current_idx * 2) + 1) <- Int64.to_float (Int64.sub time_1 time_0) ;

    let next =
      (max : int -> int -> int) (int_of_float (float_of_int current_run *. 1.01)) (succ current_run) in
    run := next ; incr idx
  done ;

  Fmt.pr "\r%a\n%!" print (samples, samples) ;

  Array.init samples (fun i -> [| m.((i * 2) + 0); m.((i * 2) + 1) |])

