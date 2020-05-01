let exit_success = 0
let exit_failure = 1

(* First computation wants to count operations needed by
   - one_if_not_zero
   - zero_if_not_zero
   - select_int

   For each /assembly instructions/, we update a counter. This way is not
   totally true. Even if we check by hands that bitwise operations don't
   emit branches, this is our only assumption! *)

let operation = ref 0

let logical_shift_right a b = incr operation ; a lsr b
let logical_or a b = incr operation ; a lor b
let shift_right a b = incr operation ; a asr b
let logical_and a b = incr operation ; a land b
let logical_not a = incr operation ; lnot a
let minus a = incr operation ; (- a)
let sub a b = incr operation ; a - b

let[@inline always] minus_one_or_less n =
  logical_shift_right n (sub Sys.int_size 1)

let[@inline always] one_if_not_zero n = minus_one_or_less (logical_or (minus n) n)
let[@inline always] zero_if_not_zero n = sub (one_if_not_zero n) 1
let[@inline always] select_int choose_b a b =
  let mask = shift_right (logical_or (minus choose_b) choose_b) Sys.int_size in
  logical_or (logical_and a (logical_not mask)) (logical_and b mask)

let one_if_not_zero_ops =
  let _ = one_if_not_zero 0xdeadbeef in
  Format.printf "[one_if_not_zero]:             %d operation(s).\n%!" !operation ;
  !operation

let () = operation := 0

let zero_if_not_zero_ops =
  let _ = zero_if_not_zero 0xdeadbeef in
  Format.printf "[zero_if_not_zero]:            %d operation(s).\n%!" !operation ;
  !operation

let () = operation := 0

let select_int_ops =
  let _ = select_int 0 1 2 in
  Format.printf "[select_int]:                  %d operation(s).\n%!" !operation ;
  !operation

let eqaf_sleep () = Unix.sleep 1

let logical_shift_right a b = eqaf_sleep () ; a lsr b
let logical_or a b = eqaf_sleep () ; a lor b
let shift_right a b = eqaf_sleep () ; a asr b
let logical_and a b = eqaf_sleep () ; a land b
let logical_not a = eqaf_sleep () ; lnot a
let minus a = eqaf_sleep () ; (- a)
let sub a b = eqaf_sleep () ; a - b

let[@inline always] minus_one_or_less n =
  logical_shift_right n (sub Sys.int_size 1)

let[@inline always] one_if_not_zero n = minus_one_or_less (logical_or (minus n) n)
let[@inline always] zero_if_not_zero n = sub (one_if_not_zero n) 1
let[@inline always] select_int choose_b a b =
  let mask = shift_right (logical_or (minus choose_b) choose_b) Sys.int_size in
  logical_or (logical_and a (logical_not mask)) (logical_and b mask)

(* Finally, we count how many time we spend when we call our
   functions. [eqaf_sleep] spends 1 second, so our bitwise operators
   should spend 1 second + some nanosecond. At the end, execution of
   them should be closely equal to our operation counter where:

     1 operation ~= 1 second

   To be able to count time, we use [caml_time] which is available only
   on Linux and for a native compilation (see [@unboxed]). Because bitwise
   operation spend at least 1 second, we finally [floor] our results to
   delete noise.

   NOTE: [check/check] does a linear regression to delete noise and really
   get how long is our functions. We think that for our functions:
   - zero_if_not_zero
   - one_if_not_zero
   - select_int
   [check/check] is too huge. *)

let time () = Clock.now ()

let fdiv a b = a /. b

let () =
  let t0 = time () in
  let _  = one_if_not_zero 0xdeadbeef in
  let t1 = time () in
  let v0 = Int64.(floor (fdiv (to_float (sub t1 t0)) 1000000000.)) in
  Format.printf "[one_if_not_zero 0xdeadbeef]:  %fs.\n%!" v0 ;
  let t0 = time () in
  let _  = one_if_not_zero 0x0 in
  let t1 = time () in
  let v1 = Int64.(floor (fdiv (to_float (sub t1 t0)) 1000000000.)) in
  Format.printf "[one_if_not_zero 0x0]:         %fs.\n%!" v0 ;
  if v0 = v1
  && int_of_float v0 = one_if_not_zero_ops
  && int_of_float v1 = one_if_not_zero_ops
  then () else exit exit_failure

let () =
  let t0 = time () in
  let _  = zero_if_not_zero 0xdeadbeef in
  let t1 = time () in
  let v0 = Int64.(floor (fdiv (to_float (sub t1 t0)) 1000000000.)) in
  Format.printf "[zero_if_not_zero 0xdeadbeef]: %fs.\n%!" v0 ;
  let t0 = time () in
  let _  = zero_if_not_zero 0x0 in
  let t1 = time () in
  let v1 = Int64.(floor (fdiv (to_float (sub t1 t0)) 1000000000.)) in
  Format.printf "[zero_if_not_zero 0x0]:        %fs.\n%!" v0 ;
  if v0 = v1
  && int_of_float v0 = zero_if_not_zero_ops
  && int_of_float v1 = zero_if_not_zero_ops
  then () else exit exit_failure

let () =
  let t0 = time () in
  let _  = select_int 0 1 2 in
  let t1 = time () in
  let v0 = Int64.(floor (fdiv (to_float (sub t1 t0)) 1000000000.)) in
  Format.printf "[select_int 0 1 2]:            %fs.\n%!" v0  ;
  let t0 = time () in
  let _  = select_int 2 1 0 in
  let t1 = time () in
  let v1 = Int64.(floor (fdiv (to_float (sub t1 t0)) 1000000000.)) in
  Format.printf "[select_int 2 1 0]:            %fs.\n%!" v1 ;
  if v0 = v1
  && int_of_float v0 = select_int_ops
  && int_of_float v1 = select_int_ops
  then () else exit exit_failure
;;
