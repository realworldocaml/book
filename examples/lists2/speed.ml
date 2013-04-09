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
      | _ ->
        failwithf "Unknown benchmark %s" benchmark ()
    )
  |> Command.run
