(* This code is in the public domain *)

let get = function None -> assert false | Some v -> v
let utc d t = get @@ Ptime.of_date_time (d, (t, 0))
let t0 = utc (1998, 12, 31) (23, 59, 59)
let t1 = utc (1999, 01, 01) (00, 00, 00)
let one_s = Ptime.Span.of_int_s 1
let () = assert (Ptime.equal (get @@ Ptime.add_span t0 one_s) t1)

let () = assert (Ptime.Span.equal (Ptime.diff t1 t0) one_s)

let t2 = utc (1998, 12, 31) (23, 59, 60)
let () = assert (Ptime.equal t1 t2)

let y = 9999 (* hypothetical year were this happens *)
let t0 = utc (y, 06, 30) (23, 59, 58)
let t1 = utc (y, 07, 01) (00, 00, 00)
let two_s = Ptime.Span.of_int_s 2
let () = assert (Ptime.Span.equal (Ptime.diff t1 t0) two_s)

let t2 = utc (y, 06, 30) (23, 59, 59)
let () = assert (Ptime.equal (get @@ Ptime.add_span t0 one_s) t2)
