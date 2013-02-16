module ImpLazy : sig
   type 'a t

   val create : (unit -> 'a) -> 'a t
   val force : 'a t -> 'a
end = struct
   type 'a delayed = Delayed of (unit -> 'a) | Value of 'a
   type 'a t = 'a delayed ref

   let create f = ref (Delayed f)
   let force v =
     match !v with
      | Delayed f ->
           let x = f () in
           v := Value x;
           x
      | Value x ->
           x
end;;

let x = ImpLazy.create (fun () -> print_string "performing computation"; 1);;


(* Lazy lists *)
type 'a el = Empty | Cons of 'a * 'a el Lazy.t
type 'a lazy_list = 'a el Lazy.t

let rec prefix_list l n =
  if n <= 0 then []
  else match l with
  | lazy Empty -> []
  | lazy (Cons (hd,tl)) -> hd :: prefix_list tl (n - 1)




let repeat_0 =
  let rec zero = lazy (Cons (0, zero)) in
  zero

let repeat_01 =
  let rec zero = lazy (Cons (0,one))
  and one = lazy (Cons (1,zero)) in
  zero

let cycle_of_list list =
  let rec build l rest =
    match l with
    | [] -> Lazy.force rest
    | hd :: tl -> Cons (hd, lazy (build tl rest))
  in
  let rec start = lazy (build list start) in
  start

let rec drop_equal l x =
  lazy (match l with
  | lazy Empty -> Empty
  | lazy (Cons (hd,tl)) ->
    if hd = x
    then Lazy.force (drop_equal tl x)
    else Lazy.force l
  )

let rec destutter l =
  lazy (match l with
  | lazy Empty -> Empty
  | lazy (Cons (hd,tl)) ->
    Cons (hd, destutter (drop_equal tl hd))
  )
