open Core.Std

(* Some examples comparing the performance of pattern matching vs direct
   implementations. *)

let plus_one x =
  match x with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 3
  | _ -> x + 1

let plus_one_slow x =
  if      x = 0 then 1
  else if x = 1 then 2
  else if x = 2 then 3
  else x + 1

let pos_match l =
  match l with
  | _ :: _ :: _ :: _ :: _ :: _ :: _ :: x :: _ -> x
  | _ :: _ :: _ :: _ :: _ :: _ :: x :: _ -> x
  | _ :: _ :: _ :: _ :: _ :: x :: _ -> x
  | _ :: _ :: _ :: _ :: x :: _ -> x
  | _ :: _ :: _ :: x :: _ -> x
  | _ :: _ :: x :: _ -> x
  | _ :: x :: _ -> x
  | x :: _ -> x
  | [] -> 0

let pos_if l =
  match List.nth l 7 with
  | Some x -> x
  | None ->
    match List.nth l 6 with
    | Some x -> x
    | None ->
      match List.nth l 5 with
      | Some x -> x
      | None ->
        match List.nth l 4 with
        | Some x -> x
        | None ->
          match List.nth l 3 with
          | Some x -> x
          | None ->
            match List.nth l 2 with
            | Some x -> x
            | None ->
              match List.nth l 1 with
              | Some x -> x
              | None ->
                match List.nth l 0 with
                | Some x -> x
                | None ->
                  0
;;

let rec sum l =
  match l with
  | [] -> 0
  | hd :: tl -> hd + sum tl

let rec sum_slow l =
  if List.is_empty l then 0
  else List.hd_exn l + sum_slow (List.tl_exn l)
;;

let sqrt_ar   =
  [|6.; 203.; 56.; 12.; 98.;6.; 203.; 56.; 12.; 98.;6.; 203.; 56.; 12.; 98.;|]
let sqrt_list = Array.to_list sqrt_ar
let sqrt_ar () =
  ignore (Array.map ~f:sqrt sqrt_ar)
let sqrt_list () =
  ignore (List.map ~f:sqrt sqrt_list)

let l = [ "a";"b"; "c"; "d"; "e"; "f"]
let a = Array.of_list l
let rev_l () = ignore (List.rev l)
let rev_a () =
  let l = Array.length a in
  Array.init l ~f:(fun i ->
    a.(l - i - 1))
  |> ignore

let l = List.range 0 100000
let a = Array.of_list l
let sum_l () =
  List.fold ~init:0 ~f:(+) l
let sum_a () =
  Array.fold ~init:0 ~f:(+) a

let sum_direct_l () =
  let rec loop l acc =
    match l with
    | [] -> acc
    | hd :: tl -> loop tl (acc + hd)
  in
  loop l 0

let sum_direct_ref_a () =
  let sum = ref 0 in
  for i = 0 to Array.length a - 1 do
    sum := !sum + a.(i)
  done;
  !sum

let sum_direct_a () =
  let len = Array.length a in
  let rec loop acc i =
    if i >= len then acc
    else loop (acc + a.(i)) (i + 1)
  in
  loop 0 0


let () =
  let example = [1;2;3;4;5;6;7;8;9;10;11;12] in
  let s = sum example in
  Command.basic
    ~summary:"Run some benchamrks"
    Command.Spec.(
      empty
      +> anon ("benchmark" %: string)
    )
    (fun benchmark () ->
      Bench.config.Bench.verbose <- false;
      Bench.config.Bench.gc_between_tests <- true;
      match benchmark with
      | "plusone" ->
        Bench.bench
          [ "if"    , (fun () -> ignore (plus_one_slow 9))
          ; "match" , (fun () -> ignore (plus_one 9))
          ]
      | "pos" ->
        Bench.bench
          [ "if"    , (fun () -> assert (1 = pos_if    [0;0;0;0;0;0;1]))
          ; "match" , (fun () -> assert (1 = pos_match [0;0;0;0;0;0;1]))
          ]
      | "list" ->
        Bench.bench
          [ "if"    , (fun () -> assert (s = sum_slow example))
          ; "match" , (fun () -> assert (s = sum example))
          ]
      | "sqrt" ->
        Bench.bench
          [ "array" , sqrt_ar
          ; "list"  , sqrt_list
          ]
      | "rev" ->
        Bench.bench
          [ "array" , rev_a
          ; "list"  , rev_l
          ]
      | "sum" ->
        Bench.bench
          [ "array"            , sum_a
          ; "list"             , sum_l
          ; "list direct"      ,   sum_direct_l
          ; "array direct"     , sum_direct_a
          ; "array direct ref" , sum_direct_ref_a
          ]
      | _ ->
        failwithf "Unknown benchmark %s" benchmark ()
    )
  |> Command.run
