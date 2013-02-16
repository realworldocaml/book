open Core.Std

let fib_num = 20

let memoize f =
  let table = Hashtbl.Poly.create () in
  (fun x ->
    match Hashtbl.find table x with
    | Some y -> y
    | None ->
      let y = f x in
      Hashtbl.add_exn table ~key:x ~data:y;
      y
  )

let rec fix f x = f (fix f) x

let memo_fix f x =
  let rec f' x = f (Lazy.force memo_f) x
  and memo_f = lazy (memoize f')
  in
  f' x

let time f =
  let start = Time.now () in
  let y = f () in
  let stop = Time.now () in
  printf "Time: %s\n" (Time.Span.to_string (Time.diff stop start));
  y

module Bad_fib = struct
  let rec fib i =
    if i <= 1 then i else fib (i - 1) + fib (i - 2)

  let () = printf "slow: %d\n" (time (fun () -> fib fib_num))
end

module Good_fib = struct

  let fib rec_fib i =
    if i <= 1 then i
    else rec_fib (i - 1) + rec_fib (i - 2)

  let rec slow_fib i = fib slow_fib i

  let () = printf "SLOW: %d\n" (time (fun () -> slow_fib fib_num))

  let fast_fib =
    let rec fib i =
      let memo_fib = Lazy.force memo_fib in
      if i <= 1 then i
      else memo_fib (i - 1) + memo_fib (i - 2)
    and memo_fib =
      lazy (memoize fib)
    in
    fib

  let () = printf "fast: %d\n" (time (fun () -> fast_fib fib_num))

  let fast_fib2 = memo_fix fib

  let () = printf "fast2: %d\n" (time (fun () -> fast_fib2 fib_num))

end

module Levenshtein_distance_slow = struct

  let rec edit_distance s t =
    match String.length s, String.length t with
    | (0,x) | (x,0) -> x
    | (len_s,len_t) ->
      let s' = String.drop_suffix s 1 in
      let t' = String.drop_suffix t 1 in
      let cost_to_drop_both =
        if s.[len_s - 1] = t.[len_t - 1] then 0 else 1
      in
      List.reduce_exn ~f:Int.min
        [ edit_distance s' t  + 1
        ; edit_distance s  t' + 1
        ; edit_distance s' t' + cost_to_drop_both
        ]

end

module Levenshtein_distance = struct

  let edit_distance' recur (s,t) =
    match String.length s, String.length t with
    | (0,x) | (x,0) -> x (* if either string is empty, return the length of the
                            other string. *)
    | (len_s,len_t) ->
      let s' = String.drop_suffix s 1 in
      let t' = String.drop_suffix t 1 in
      let cost_to_drop_both =
        if s.[len_s - 1] = t.[len_t - 1] then 0 else 1
      in
      List.reduce_exn ~f:Int.min
        [ recur (s',t ) + 1
        ; recur (s ,t') + 1
        ; recur (s',t') + cost_to_drop_both
        ]

  let edit_distance_fast = memo_fix edit_distance'

end

