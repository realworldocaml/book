(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Lwt.Syntax

open Test

let l =  [1; 2; 3; 4; 5]
let a = Lwt_seq.of_list l
let rec pause n =
   if n <= 0 then
      Lwt.return_unit
   else
      let* () = Lwt.pause () in
      pause (n - 1)
let pause n = pause (n mod 5)
let b =
   Lwt_seq.unfold_lwt
     (function
      | [] -> let+ () = pause 2 in None
      | x::xs -> let+ () = pause (x+2) in Some (x, xs))
     l

let suite_base = suite "lwt_seq" [
  test "fold_left" begin fun () ->
    let n = ref 1 in
    Lwt_seq.fold_left (fun acc x ->
      let r = x = !n && acc in
      incr n; r) true a
  end;
  test "fold_left_s" begin fun () ->
    let n = ref 1 in
    Lwt_seq.fold_left_s (fun acc x ->
      let r = x = !n && acc in
      incr n; Lwt.return r) true a
  end;

  test "map" begin fun () ->
    let v = Lwt_seq.map (fun x -> (x * 2)) a in
    let+ l' = Lwt_seq.to_list v in
    l' = [2; 4; 6; 8; 10]
  end;
  test "map_s" begin fun () ->
    let v = Lwt_seq.map_s (fun x -> Lwt.return (x * 2)) a in
    let+ l' = Lwt_seq.to_list v in
    l' = [2; 4; 6; 8; 10]
  end;

  test "filter" begin fun () ->
    let v = Lwt_seq.filter (fun x -> (x mod 2 = 0)) a in
    let+ l' = Lwt_seq.to_list v in
    l' = [2; 4]
  end;
  test "filter_s" begin fun () ->
    let v = Lwt_seq.filter_s (fun x -> Lwt.return (x mod 2 = 0)) a in
    let+ l' = Lwt_seq.to_list v in
    l' = [2; 4]
  end;

  test "iter_n(1)" begin fun () ->
    let max_concurrency = 1 in
    let running = ref 0 in
    let sum = ref 0 in
    let f x =
       incr running;
       assert (!running <= max_concurrency);
       let* () = pause x in
       sum := !sum + x;
       decr running;
       Lwt.return_unit
    in
    let* () = Lwt_seq.iter_n ~max_concurrency f a in
    assert (!sum = List.fold_left (+) 0 l);
    sum := 0;
    let* () = Lwt_seq.iter_n ~max_concurrency f b in
    assert (!sum = List.fold_left (+) 0 l);
    Lwt.return_true
  end;
  test "iter_n(2)" begin fun () ->
    let max_concurrency = 2 in
    let running = ref 0 in
    let sum = ref 0 in
    let f x =
       incr running;
       assert (!running <= max_concurrency);
       let* () = pause x in
       sum := !sum + x;
       decr running;
       Lwt.return_unit
    in
    let* () = Lwt_seq.iter_n ~max_concurrency f a in
    assert (!sum = List.fold_left (+) 0 l);
    sum := 0;
    let* () = Lwt_seq.iter_n ~max_concurrency f b in
    assert (!sum = List.fold_left (+) 0 l);
    Lwt.return_true
  end;
  test "iter_n(100)" begin fun () ->
    let max_concurrency = 100 in
    let running = ref 0 in
    let sum = ref 0 in
    let f x =
       incr running;
       assert (!running <= max_concurrency);
       let* () = pause x in
       sum := !sum + x;
       decr running;
       Lwt.return_unit
    in
    let* () = Lwt_seq.iter_n ~max_concurrency f a in
    assert (!sum = List.fold_left (+) 0 l);
    sum := 0;
    let* () = Lwt_seq.iter_n ~max_concurrency f b in
    assert (!sum = List.fold_left (+) 0 l);
    Lwt.return_true
  end;

  test "filter_map" begin fun () ->
    let v = Lwt_seq.filter_map (fun x ->
      if x mod 2 = 0 then Some (x * 2) else None) a
    in
    let+ l' = Lwt_seq.to_list v in
    l' = [4; 8]
  end;
  test "filter_map_s" begin fun () ->
    let v = Lwt_seq.filter_map_s (fun x ->
      Lwt.return (if x mod 2 = 0 then Some (x * 2) else None)) a
    in
    let+ l' = Lwt_seq.to_list v in
    l' = [4; 8]
  end;

  test "unfold" begin fun () ->
    let range first last =
      let step i = if i > last then None else Some (i, succ i) in
      Lwt_seq.unfold step first
    in
    let* a = Lwt_seq.to_list (range 1 3) in
    let+ b = Lwt_seq.to_list (range 1 0) in
      ([1;2;3] = a) &&
      ([] = b)
  end;

  test "unfold_lwt" begin fun () ->
    let range first last =
      let step i =
         if i > last then Lwt.return_none else Lwt.return_some (i, succ i)
      in
      Lwt_seq.unfold_lwt step first
    in
    let* a = Lwt_seq.to_list (range 1 3) in
    let+ b = Lwt_seq.to_list (range 1 0) in
      ([1;2;3] = a) &&
      ([] = b)
  end;


  test "fold-into-exception-from-of-seq" begin fun () ->
    let fail = fun () -> failwith "XXX" in
    let seq = fun () -> Seq.Cons (1, (fun () -> Seq.Cons (2, fail))) in
    let a = Lwt_seq.of_seq seq in
    let+ n =
      Lwt.catch
        (fun () -> Lwt_seq.fold_left (+) 0 a)
        (function
          | Failure x when x = "XXX" -> Lwt.return (-1)
          | exc -> raise exc)
    in
    n = (-1)
  end;

  test "fold-into-immediate-exception-from-of-seq" begin fun () ->
    let fail = fun () -> failwith "XXX" in
    let seq = fail in
    let a = Lwt_seq.of_seq seq in
    let+ n =
      Lwt.catch
        (fun () -> Lwt_seq.fold_left (+) 0 a)
        (function
          | Failure x when x = "XXX" -> Lwt.return (-1)
          | exc -> raise exc)
    in
    n = (-1)
  end;

  test "fold-into-exception-from-of-seq-lwt" begin fun () ->
    let fail = fun () -> failwith "XXX" in
    let seq: int Lwt.t Seq.t = fun () ->
      Seq.Cons (Lwt.return 1,
        fun () ->
          Seq.Cons (Lwt.return 2, fail)) in
    let a = Lwt_seq.of_seq_lwt seq in
    let+ n =
      Lwt.catch
        (fun () -> Lwt_seq.fold_left (+) 0 a)
        (function
          | Failure x when x = "XXX" -> Lwt.return (-1)
          | exc -> raise exc)
    in
    n = (-1)
  end;

  test "fold-into-immediate-exception-from-of-seq-lwt" begin fun () ->
    let fail = fun () -> failwith "XXX" in
    let seq: int Lwt.t Seq.t = fail in
    let a = Lwt_seq.of_seq_lwt seq in
    let+ n =
      Lwt.catch
        (fun () -> Lwt_seq.fold_left (+) 0 a)
        (function
          | Failure x when x = "XXX" -> Lwt.return (-1)
          | exc -> raise exc)
    in
    n = (-1)
  end;
]

let fs = [(+); (-); (fun x _ -> x); min; max]
let ls = [
   [];
   l;
   l@l@l;
   List.rev l;
   [0;0;0];
   [max_int;0;min_int];
   [max_int;max_int];
]
let cs = [0;1;max_int;min_int;44;5]
let with_flc test =
   Lwt_list.for_all_s
     (fun f ->
        Lwt_list.for_all_s
          (fun l ->
             Lwt_list.for_all_s
               (fun c -> test f l c)
               cs)
          ls)
     fs
let equals l1 seq2 =
   let* l2 = Lwt_seq.to_list seq2 in
   Lwt.return (l1 = l2)
let commutes lf sf l =
   equals (lf l) (sf (Lwt_seq.of_list l))


let suite_fuzzing = suite "lwt_seq(pseudo-fuzzing)" [

  test "map" begin fun () ->
     with_flc (fun f l c ->
        let lf = List.map (fun x -> f x c) in
        let sf = Lwt_seq.map (fun x -> f x c) in
        commutes lf sf l
     )
  end;

  test "map_s" begin fun () ->
     with_flc (fun f l c ->
        let lf = List.map (fun x -> f x c) in
        let sf = Lwt_seq.map_s (fun x -> Lwt.return (f x c)) in
        commutes lf sf l
     )
  end;

  test "iter" begin fun () ->
     with_flc (fun f l c ->
        let lf l =
           let r = ref c in
           List.iter (fun x -> r := f !r x) l;
           [!r] in
        let sf s =
           let r = ref c in
           fun () ->
             let* () = Lwt_seq.iter (fun x -> r := f !r x) s in
             Lwt.return (Lwt_seq.Cons (!r, Lwt_seq.empty)) in
        commutes lf sf l
     )
  end;

  test "iter_s" begin fun () ->
     with_flc (fun f l c ->
        let lf l =
           let r = ref c in
           List.iter (fun x -> r := f !r x) l;
           [!r] in
        let sf s =
           let r = ref c in
           fun () ->
             let* () = Lwt_seq.iter_s (fun x -> r := f !r x; Lwt.return_unit) s in
             Lwt.return (Lwt_seq.Cons (!r, Lwt_seq.empty)) in
        commutes lf sf l
     )
  end;

  (* the [f]s commute sufficiently for parallel execution *)
  test "iter_p" begin fun () ->
     with_flc (fun f l c ->
        let lf l =
           let r = ref c in
           List.iter (fun x -> r := f !r x) l;
           [!r]
        in
        let sf s =
           Lwt_seq.return_lwt @@
           let r = ref c in
           let+ () = Lwt_seq.iter_p (fun x -> r := f !r x; Lwt.return_unit) s in
           !r
        in
        commutes lf sf l
     )
  end;

  test "iter_p (pause)" begin fun () ->
     with_flc (fun f l c ->
        let lf l =
           let r = ref c in
           List.iter (fun x -> r := f !r x) l;
           [!r]
        in
        let sf s =
           Lwt_seq.return_lwt @@
           let r = ref c in
           let+ () =
              Lwt_seq.iter_p
                (fun x ->
                   let* () = pause x in
                   r := f !r x;
                   pause x)
                s
           in
           !r
        in
        commutes lf sf l
     )
  end;

  test "iter_n" begin fun () ->
     l |> Lwt_list.for_all_s @@ fun max_concurrency ->
     with_flc (fun f l c ->
        let lf l =
           let r = ref c in
           List.iter (fun x -> r := f !r x) l;
           [!r] in
        let sf s =
           Lwt_seq.return_lwt @@
           let r = ref c in
           let+ () = Lwt_seq.iter_n ~max_concurrency (fun x -> r := f !r x; Lwt.return_unit) s in
           !r
        in
        commutes lf sf l
     )
  end;

  test "iter_n (pause)" begin fun () ->
     l |> Lwt_list.for_all_s @@ fun max_concurrency ->
     with_flc (fun f l c ->
        let lf l =
           let r = ref c in
           List.iter (fun x -> r := f !r x) l;
           [!r] in
        let sf s =
           Lwt_seq.return_lwt @@
           let r = ref c in
           let+ () =
              Lwt_seq.iter_n ~max_concurrency
                 (fun x ->
                    let* () = pause x in
                    r := f !r x;
                    pause x)
                 s
           in
           !r
        in
        commutes lf sf l
     )
  end;

]
