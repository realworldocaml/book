open Core.Std

  let memoize f =
    let table = Hashtbl.Poly.create () in
    (fun x ->
      match Hashtbl.find table x with
      | Some y -> y
      | None ->
        let y = f x in
        Hashtbl.add_exn table ~key:x ~data:y;
        y
    );;

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
  ;;

  let time f =
    let start = Time.now () in
    let y = f () in
    let stop = Time.now () in
    printf "Time: %s\n" (Time.Span.to_string (Time.diff stop start));
    y ;;

  let rec fib i =
    if i <= 1 then 1 else fib (i - 1) + fib (i - 2);;

  let fib_recur recur i =
    if i <= 1 then i
    else recur (i - 1) + recur (i - 2) ;;

  let rec fix f x = f (fix f) x ;;

  let edit_distance_recur recur (s,t) =
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
        ] ;;

  let edit_distance = memo_fix edit_distance_recur


