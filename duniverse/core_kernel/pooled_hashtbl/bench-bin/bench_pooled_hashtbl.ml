open! Core_kernel

let n = 1_000_000
let times = 10

(** Benchmark the core Hashtbl *)
let test_core () =
  let module Identity_table =
    Hashtbl.Make (struct
      include Int

      let hash x = x
    end)
  in
  let tbl = Identity_table.create ~size:(2 * n) () in
  for i = 0 to n - 1 do
    Identity_table.set tbl ~key:i ~data:i
  done;
  let start = Time.now () in
  let first_n = ref 0 in
  for _ = 0 to times - 1 do
    let loop_end = !first_n + n - 1 in
    for i = !first_n to loop_end do
      let x = n + i in
      Identity_table.set tbl ~key:x ~data:x;
      Identity_table.remove tbl i
    done;
    first_n := !first_n + n
  done;
  let elapsed = Time.diff (Time.now ()) start in
  printf "Core hashtbl took %6f\n" (Time.Span.to_sec elapsed /. Float.of_int times);
  printf "Size: %d\n%!" (Identity_table.length tbl);
  for i = !first_n to !first_n + n - 1 do
    assert (Identity_table.find_exn tbl i = i)
  done
;;

(** Benchmark Pooled_hashtbl, a linked chain hashtbl backed by a Zero.Obj_array pool *)
let test_zero () =
  let module Identity_table =
    Pooled_hashtbl.Make (struct
      include Int

      let hash x = x
    end)
  in
  let tbl = Identity_table.create ~size:(2 * n) () in
  for i = 0 to n - 1 do
    Identity_table.set tbl ~key:i ~data:i
  done;
  let start = Time.now () in
  let first_n = ref 0 in
  for _ = 0 to times - 1 do
    let loop_end = !first_n + n - 1 in
    for i = !first_n to loop_end do
      let x = n + i in
      Identity_table.set tbl ~key:x ~data:x;
      Identity_table.remove tbl i
    done;
    first_n := !first_n + n
  done;
  let elapsed = Time.diff (Time.now ()) start in
  printf "Zero pooled hashtbl took %6f\n" (Time.Span.to_sec elapsed /. Float.of_int times);
  printf "Size: %d\n%!" (Identity_table.length tbl);
  for i = !first_n to !first_n + n - 1 do
    assert (Identity_table.find_exn tbl i = i)
  done
;;

let () =
  for _i = 0 to 3 do
    test_core ();
    test_zero ()
  done
;;
