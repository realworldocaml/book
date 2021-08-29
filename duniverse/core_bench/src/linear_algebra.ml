open Core

(** Vectors *)
module Vec = struct
  type t = float array [@@deriving sexp]
  let copy = Array.copy

  let create0 len = Array.create ~len 0.

  let sumsq t =
    let accum = ref 0. in
    for i = 0 to Array.length t - 1 do
      let t_i = t.(i) in
      accum := !accum +. t_i *. t_i
    done;
    !accum

  let norm t = sqrt (sumsq t)

  let almost_equal ~tol t1 t2 =
    Array.length t1 = Array.length t2 &&
    Array.for_all2_exn t1 t2 ~f:(fun x y -> Float.(<=) (Float.abs (x -. y)) tol)

end

(** Matrices *)
module Mat = struct
  type t = float array array [@@deriving sexp]

  let copy t = Array.map t ~f:Array.copy

  let create0 ~rows ~cols =
    let make_row _ = Array.create ~len:cols 0. in
    Array.init rows ~f:make_row

  let create_per_row ~rows ~cols ~f =
    let make_row _ = Array.init cols ~f in
    Array.init rows ~f:make_row

  (** The norm of a column of a matrix. *)
  let col_norm t column =
    let accum = ref 0. in
    for i = 0 to Array.length t - 1 do
      let entry = t.(i).(column) in
      accum := !accum +. entry *. entry
    done;
    sqrt !accum

  (** The inner product of columns j1 and j2 *)
  let col_inner_prod t j1 j2 =
    let accum = ref 0. in
    for i = 0 to Array.length t - 1 do
      accum := !accum +. t.(i).(j1) *. t.(i).(j2)
    done;
    !accum

  let get_column t j =
    let len = Array.length t in
    Array.init len ~f:(fun i -> t.(i).(j))

  let almost_equal ~tol t1 t2 =
    Array.length t1 = Array.length t2 &&
    Array.for_all2_exn t1 t2 ~f:(Vec.almost_equal ~tol)

end

let qr_in_place a =
  (* Our implementation will just do Gram-Schmidt. *)
  let m = Array.length a in
  if m = 0
  then ([| |], [| |]) (* empty QR decomposition *)
  else
    let n = Array.length a.(0) in
    let r = Mat.create0 ~rows:n ~cols:n in
    for j = 0 to n - 1 do
      (* handle column j *)
      let alpha = Mat.col_norm a j in
      r.(j).(j) <- alpha;
      let one_over_alpha = 1. /. alpha in
      (* Rescale this column so that it's a unit vector. *)
      for i = 0 to m - 1 do
        a.(i).(j) <- a.(i).(j) *. one_over_alpha
      done;
      for j2 = j + 1 to n - 1 do
        let c = Mat.col_inner_prod a j j2 in
        r.(j).(j2) <- c;
        (* Now, subtract c * column j from column j2. *)
        for i = 0 to m - 1 do
          a.(i).(j2) <- a.(i).(j2) -. c *. a.(i).(j)
        done
      done
    done;
    (a, r)


(** [qr A] returns the QR-decomposition of [A] as a pair (Q,R). [A] must have
    at least as many rows as columns and have full rank.

    If [in_place] (default: [false]) is [true], then [A] is overwritten with [Q].
*)
let qr ?(in_place=false) a =
  let a = if in_place then a else Mat.copy a in
  qr_in_place a

(** [triu_solve R b] solves R x = b where [R] is an m x m upper-triangular matrix
    and [b] is an m x 1 column vector.  *)
let triu_solve r b =
  let m = Array.length b in
  if m <> Array.length r then
    Or_error.error_string "triu_solve R b requires R to be square with same \
                           number of rows as b"
  else if m = 0
  then Ok [| |]
  else if m <> Array.length r.(0)
  then Or_error.error_string "triu_solve R b requires R to be square"
  else
    let sol = Vec.copy b in
    for i = m - 1 downto 0 do
      sol.(i) <- sol.(i) /. r.(i).(i);
      for j = 0 to i - 1 do
        sol.(j) <- sol.(j) -. r.(j).(i) *. sol.(i)
      done
    done;
    if (Array.exists sol ~f:Float.is_nan)
    then Or_error.error_string "triu_solve detected NaN result"
    else Ok sol

(** [mul A B] computes the matrix product [A * B].  If [transa] (default: [false])
    is [true], then we compute A' * B where A' denotes the transpose of A. *)
(* val mul : ?transa:bool -> m -> m -> m *)

(** [mul_mv A x] computes the product [A * x] (where [A] is a matrix and [x] is
    a column vector).

    [transa] is as with [mul].
*)
let mul_mv ?(transa=false) a x =
  (* we let c denote either a or a', depending on whether transa is set. *)
  let rows = Array.length a in
  if rows = 0
  then [| |]
  else
    let cols = Array.length a.(0) in
    (* (m, n, c_get) will be (rows of c, columns of c, accessor for c). *)
    let (m, n, c_get) =
      if transa
      then
        let c_get i j = a.(j).(i) in
        (cols, rows, c_get)
      else
        let c_get i j = a.(i).(j) in
        (rows, cols, c_get)
    in
    if n <> Array.length x then failwith "Dimension mismatch";
    let result = Vec.create0 m in
    for i = 0 to m - 1 do
      result.(i) <- Array.foldi x ~init:0. ~f:(fun j accum x_j ->
        accum +. c_get i j *. x_j
      )
    done;
    result

(** [ols A b] computes the ordinary least-squares solution to A x = b.
    [A] must have at least as many rows as columns and have full rank.

    This can be used to compute solutions to non-singular square systems,
    but is somewhat sub-optimal for that purpose.

    The algorithm is to factor A = Q * R and solve R x = Q' b where Q' denotes
    the transpose of Q.
*)
let ols ?(in_place=false) a b =
  let (q, r) = qr ~in_place a in
  triu_solve r (mul_mv ~transa:true q b)

let%test_module _ = (module struct
  (* The examples and the correct reference values were generated in Octave. *)
  let mat_A =
    [| [|  1.5539829; -0.4525782; -1.1728152;  1.3674086; -1.1205482 |]
    ;  [|  0.6792944;  1.1568534;  0.4154379;  0.9084153; -2.5703106 |]
    ;  [| -0.5618483; -0.6781523; -0.5248221; -0.4142220; -0.7306068 |]
    ;  [| -0.5998192;  1.3722146; -0.5557165;  0.0363979;  0.2204308 |]
    ;  [|  0.9425094; -0.3673329;  0.0099052; -0.1091253;  0.9456771 |]
    ;  [| -0.6091836;  0.6229814;  1.1498873;  1.6160578;  0.7104362 |]
    ;  [| -0.1933751;  1.3707531;  0.6352440;  1.3795393; -1.1168355 |]
    |]
  (* In case you want to enter the matrix back into Octave:
         1.5539829  -0.4525782  -1.1728152   1.3674086  -1.1205482
         0.6792944   1.1568534   0.4154379   0.9084153  -2.5703106
        -0.5618483  -0.6781523  -0.5248221  -0.4142220  -0.7306068
        -0.5998192   1.3722146  -0.5557165   0.0363979   0.2204308
         0.9425094  -0.3673329   0.0099052  -0.1091253   0.9456771
        -0.6091836   0.6229814   1.1498873   1.6160578   0.7104362
        -0.1933751   1.3707531   0.6352440   1.3795393  -1.1168355
  *)

  (* The known correct values for QR decomposition of A. *)
  let known_Q =
    [| [|  0.7057304; -0.0081434; -0.3701402;  0.5895472;  0.0512010 |]
    ;  [|  0.3084968;  0.5535933;  0.1618997; -0.1938150; -0.5796110 |]
    ;  [| -0.2551595; -0.3432622; -0.2917051;  0.2900536; -0.4904861 |]
    ;  [| -0.2724037;  0.4956579; -0.6737123; -0.0924160;  0.3724562 |]
    ;  [|  0.4280340; -0.0431215;  0.2265497; -0.2622756;  0.4346026 |]
    ;  [| -0.2766564;  0.1864428;  0.4836269;  0.6300418;  0.2805885 |]
    ;  [| -0.0878199;  0.5416106;  0.1121823;  0.2376066; -0.1204989 |]
    |]
(*     0.7057304  -0.0081434  -0.3701402   0.5895472   0.0512010
       0.3084968   0.5535933   0.1618997  -0.1938150  -0.5796110
      -0.2551595  -0.3432622  -0.2917051   0.2900536  -0.4904861
      -0.2724037   0.4956579  -0.6737123  -0.0924160   0.3724562
       0.4280340  -0.0431215   0.2265497  -0.2622756   0.4346026
      -0.2766564   0.1864428   0.4836269   0.6300418   0.2805885
      -0.0878199   0.5416106   0.1121823   0.2376066  -0.1204989
*)

  let known_R =
    [| [|  2.2019498; -0.6132342; -0.7839086;  0.7260895; -1.1510467 |]
    ;  [|  0.0000000;  2.4314496;  0.7022565;  1.7051660; -1.5669469 |]
    ;  [|  0.0000000;  0.0000000;  1.6584752;  0.6488550;  0.4957819 |]
    ;  [|  0.0000000;  0.0000000;  0.0000000;  1.8811696; -0.4605288 |]
    ;  [|  0.0000000;  0.0000000;  0.0000000;  0.0000000;  2.6177719 |]
    |]
(*     2.2019498  -0.6132342  -0.7839086   0.7260895  -1.1510467
       0.0000000   2.4314496   0.7022565   1.7051660  -1.5669469
       0.0000000   0.0000000   1.6584752   0.6488550   0.4957819
       0.0000000   0.0000000   0.0000000   1.8811696  -0.4605288
       0.0000000   0.0000000   0.0000000   0.0000000   2.6177719
*)

  let (q, r) = qr mat_A

  let%test "qr: correct Q" = Mat.almost_equal ~tol:1e-7 q known_Q

  let%test "qr: correct R" = Mat.almost_equal ~tol:1e-7 r known_R

  let v = [| -0.1970397;  1.1226276;  2.1068430; -1.0784432; -0.2012862 |]

  let known_A_times_v = [| -4.5343322;  1.5778238; -1.1625479;
                                0.4042440; -0.6498874;  1.3562139;  1.6523560 |]

  let known_R_inverse_v = [| 0.7162378;  0.3869500;  1.5249890; -0.5921073; -0.0768922 |]

  let%test "mul_mv" = Vec.almost_equal ~tol:1e-7 (mul_mv mat_A v) known_A_times_v

  let%test "triu_solve" =
    match triu_solve r v with
    | Ok r_inverse_v ->
      Vec.almost_equal ~tol:1e-7 r_inverse_v known_R_inverse_v
    | Error _ -> false

  let w = [| -0.5465946;  0.5402624; -1.9966324;
             -1.0315546;  0.0570856;  1.3310883; -0.1333566 |]

  let known_A_backslash_w = (* like A \ w in MATLAB or Octave *)
    [| 7.08921663963053e-01; 7.00514461674804e-02;
       1.25581479352235e+00; -6.33055939677457e-04; 2.55312671644131e-01|]

  let%test "ols" =
    match ols mat_A w with
    | Ok a_backslash_w ->
      Vec.almost_equal ~tol:1e-14 a_backslash_w known_A_backslash_w
    | Error _ -> false

end)
