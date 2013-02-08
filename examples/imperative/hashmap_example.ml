open Core.Std
open Dlist_example

module HashMap : sig
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val find : ('a, 'b) t -> key:'a -> 'b option
  val iter : ('a, 'b) t -> f:(key:'a -> data:'b -> unit) -> unit
end = struct
  type ('a, 'b) t = ('a * 'b) list array

  let num_buckets = 17
  let hash_bucket key = (Hashtbl.hash key) mod num_buckets

  let create () = Array.create num_buckets []

  let add table ~key ~data =
    let index = hash_bucket key in
    table.(index) <- (key, data) :: table.(index)

  let find table ~key =
    let rec find = function
     | (k, d) :: t -> if k = key then Some d else find t
     | [] -> None
    in
    find table.(hash_bucket key)

  let iter table ~f =
    for i = 0 to num_buckets - 1 do
      List.iter table.(i) ~f:(fun (key, data) -> f ~key ~data)
    done
end

module IterableHashMap : sig
  type 'a iterator = < has_value : bool; value : 'a; next : unit; remove : unit >
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val iterator : ('a, 'b) t -> ('a * 'b) iterator
  val find : ('a, 'b) t -> key:'a -> ('a * 'b) iterator
end = struct
  type 'a iterator = < has_value : bool; value : 'a; next : unit; remove : unit >
  type ('a, 'b) t = ('a * 'b) DListX.dlist array

  let num_buckets = 17
  let hash_bucket key = (Hashtbl.hash key) mod num_buckets

  let create () = Array.init num_buckets (fun _ -> DListX.create ())

  let add table ~key ~data =
    let index = hash_bucket key in
    let it = DListX.find_it table.(index) ~data:(key, data) in
    if it#has_value then it#remove;
    ignore (DListX.insert_first table.(index) (key, data))

  let make_iterator table index_ dlist_it_ =
    object (self)
      val mutable index = index_
      val mutable dlist_it = dlist_it_
      method has_value = dlist_it#has_value
      method value = dlist_it#value
      method next =
         dlist_it#next;
         self#normalize
      method remove =
         dlist_it#remove;
         self#normalize
      method private normalize =
        while not dlist_it#has_value && index < num_buckets - 1 do
          index <- index + 1;
          dlist_it <- DListX.iterator table.(index)
        done
      initializer self#normalize
    end

  let iterator table =
    make_iterator table 0 (DListX.iterator table.(0))

  let find table ~key =
    let index = hash_bucket key in
    let it = DListX.iterator table.(index) in
    while it#has_value && fst it#value <> key do
      it#next
    done;
    if it#has_value then
       make_iterator table index it
    else
       make_iterator table num_buckets it
end


let () =
  let module IHM = IterableHashMap in
  let table = IHM.create () in
  IHM.add table ~key:"small" ~data:1.00;
  IHM.add table ~key:"medium" ~data:1.50;
  IHM.add table ~key:"large" ~data:2.25;
  IHM.add table ~key:"enormous" ~data:5.00;

  let it = IHM.iterator table in
  while it#has_value do
    let size, price = it#value in
    Printf.printf "Size %s is $%.02f\n" size price;
    it#next
  done;

  let it = IHM.find table "enormous" in
  it#remove;

  let it = IHM.iterator table in
  while it#has_value do
    let size, price = it#value in
    Printf.printf "Size %s is $%.02f\n" size price;
    it#next
  done;;

