let n = 10000

let () =
  while true do
    let t0 = Unix.gettimeofday () in
    for _i = 1 to n do
      ignore @@ Fmt.str "Hello %a" Fmt.string "world"
    done;
    let t1 = Unix.gettimeofday () in
    Printf.printf "Formatted %.0f messages/second\n%!" (float n /. (t1 -. t0))
  done
