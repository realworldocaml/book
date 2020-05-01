(* Code under Apache License 2.0 - Jane Street Group, LLC <opensource@janestreet.com> *)

let col_norm a column =
  let acc = ref 0. in
  for i = 0 to Array.length a - 1 do
    let entry = a.(i).(column) in
    acc := !acc +. (entry *. entry)
  done ;
  sqrt !acc

let col_inner_prod t j1 j2 =
  let acc = ref 0. in
  for i = 0 to Array.length t - 1 do
    acc := !acc +. (t.(i).(j1) *. t.(i).(j2))
  done ;
  !acc

let qr_in_place a =
  let m = Array.length a in
  if m = 0 then ([||], [||])
  else
    let n = Array.length a.(0) in
    let r = Array.make_matrix n n 0. in
    for j = 0 to n - 1 do
      let alpha = col_norm a j in
      r.(j).(j) <- alpha ;
      let one_over_alpha = 1. /. alpha in
      for i = 0 to m - 1 do
        a.(i).(j) <- a.(i).(j) *. one_over_alpha
      done ;
      for j2 = j + 1 to n - 1 do
        let c = col_inner_prod a j j2 in
        r.(j).(j2) <- c ;
        for i = 0 to m - 1 do
          a.(i).(j2) <- a.(i).(j2) -. (c *. a.(i).(j))
        done
      done
    done ;
    (a, r)

let qr ?(in_place = false) a =
  let a = if in_place then a else Array.map Array.copy a in
  qr_in_place a

let mul_mv ?(trans = false) a x =
  let rows = Array.length a in
  if rows = 0 then [||]
  else
    let cols = Array.length a.(0) in
    let m, n, get =
      if trans then
        let get i j = a.(j).(i) in
        (cols, rows, get)
      else
        let get i j = a.(i).(j) in
        (rows, cols, get)
    in
    if n <> Array.length x then failwith "Dimension mismatch" ;
    let result = Array.make m 0. in
    for i = 0 to m - 1 do
      let v, _ =
        Array.fold_left
          (fun (acc, j) x -> (acc +. (get i j *. x), succ j))
          (0., 0) x
      in
      result.(i) <- v
    done ;
    result

let is_nan v = match classify_float v with FP_nan -> true | _ -> false
  let error_msg msg = Error (`Msg msg)

let triu_solve r b =
  let m = Array.length b in
  if m <> Array.length r then
    error_msg
      "triu_solve R b requires R to be square with same number of rows as b"
  else if m = 0 then Ok [||]
  else if m <> Array.length r.(0) then
    error_msg "triu_solve R b requires R to be a square"
  else
    let sol = Array.copy b in
    for i = m - 1 downto 0 do
      sol.(i) <- sol.(i) /. r.(i).(i) ;
      for j = 0 to i - 1 do
        sol.(j) <- sol.(j) -. (r.(j).(i) *. sol.(i))
      done
    done ;
    if Array.exists is_nan sol then
      error_msg "triu_solve detected NaN result"
    else Ok sol

let ols ?(in_place = false) a b =
  let q, r = qr ~in_place a in
  triu_solve r (mul_mv ~trans:true q b)

let make_lr_inputs responder predictors m =
  Array.init (Array.length m) (fun i -> Array.map (fun a -> a m.(i)) predictors),
  Array.init (Array.length m) (fun i -> responder m.(i))

let r_square m responder predictors r =
  let predictors_matrix, responder_vector =
    make_lr_inputs responder predictors m
  in
  let sum_responder = Array.fold_left ( +. ) 0. responder_vector in
  let mean = sum_responder /. float (Array.length responder_vector) in
  let tot_ss = ref 0. in
  let res_ss = ref 0. in
  let predicted i =
    let x = ref 0. in
    for j = 0 to Array.length r - 1 do
      x := !x +. (predictors_matrix.(i).(j) *. r.(j))
    done ;
    !x
  in
  for i = 0 to Array.length responder_vector - 1 do
    tot_ss := !tot_ss +. ((responder_vector.(i) -. mean) ** 2.) ;
    res_ss := !res_ss +. ((responder_vector.(i) -. predicted i) ** 2.)
  done ;
  1. -. (!res_ss /. !tot_ss)

let ols responder predictors m =
  let matrix, vector = make_lr_inputs responder predictors m in
  match ols ~in_place:true matrix vector with
  | Ok estimates ->
     let r_square = r_square m responder predictors estimates in
     Ok (estimates, r_square)
  | Error _ as err -> err

