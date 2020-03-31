(* A tiny library of finite or infinite streams. *)

type 'a stream =
  unit -> 'a answer

and 'a answer =
  | Done
  | More of 'a * 'a stream

let empty () =
  Done

let cons x ys () =
  More (x, ys)

let singleton x =
  cons x empty

let rec (++) xs ys () =
  match xs() with
  | More (x, xs) ->
      More (x, xs ++ ys)
  | Done ->
      ys()

let rec fold_left accu f xs =
  match xs() with
  | Done ->
      accu
  | More (x, xs) ->
    let accu = f accu x in
    fold_left accu f xs

let iter f xs =
  fold_left () (fun () x -> f x) xs

let rec map f xs () =
  match xs() with
  | Done ->
      Done
  | More (x, xs) ->
      More (f x, map f xs)

let rec list_cat_rev (xs : 'a list) (ys : 'a stream) : 'a list =
  match ys() with
  | Done ->
      xs
  | More (y, ys) ->
      list_cat_rev (y :: xs) ys

let to_list (ys : 'a stream) : 'a list =
  List.rev (list_cat_rev [] ys)

let to_array (ys : 'a stream) : 'a array =
  Array.of_list (to_list ys)

(* A finite or infinite imperative stream. By convention, end-of-stream is
   signaled via an exception. *)

type 'a imperative_stream =
  unit -> 'a

let fresh (xs : 'a stream) : 'a imperative_stream =
  let r = ref xs in
  fun () ->
    match !r() with
    | Done ->
        raise End_of_file
    | More (x, xs) ->
        r := xs;
        x

(* Beware that [find] will diverge if the stream is infinite. *)

let find (p : 'a -> bool) (xs : 'a imperative_stream) : 'a option =
  try
    let rec loop() =
      let x = xs() in
      if p x then
        Some x
      else
        loop()
    in
    loop()
  with End_of_file ->
    None
