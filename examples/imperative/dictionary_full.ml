open Core.Std

type 'a iterator =
  < has_value : bool;
    value : 'a;
    next : unit;
    remove : unit;
    insert_after : 'a -> unit >

module Dlist : sig
  type 'a t
  type 'a element

  val create   : unit -> 'a t
  val is_empty : 'a t -> bool

  val first        : 'a t       -> 'a element option
  val insert_first : 'a t       -> 'a -> 'a element
  val insert_after : 'a element -> 'a -> 'a element
  val value        : 'a element -> 'a
  val remove       : 'a t       -> 'a element -> unit
  val next         : 'a element -> 'a element option
  val previous     : 'a element -> 'a element option

  val find : 'a t -> 'a -> 'a element option
  val iter : 'a t -> f:('a -> unit) -> unit

  val iterator : 'a t -> 'a iterator
  val find_it : 'a t -> data:'a -> 'a iterator
end = struct

  type 'a element =
    { value : 'a;
      mutable next : 'a element option;
      mutable previous : 'a element option
    }

  type 'a t = 'a element option ref

  let create () = ref None
  let is_empty l = (!l = None)

  let value elt = elt.value

  let first l = !l
  let next elt = elt.next
  let previous elt = elt.previous

  let insert_first l value =
    let new_elt = { previous = None; next = !l; value } in
    begin match !l with
    | Some old_first -> old_first.previous <- Some new_elt
    | None -> ()
    end;
    l := Some new_elt;
    new_elt

  let insert_after elt value =
    let new_elt = { value; previous = Some elt; next = elt.next } in
    begin match elt.next with
    | Some old_next -> old_next.previous <- Some new_elt
    | None -> ()
    end;
    elt.next <- Some new_elt;
    new_elt

  let check_is_first_element l elt1 =
    match !l with
    | Some elt2 when phys_equal elt1 elt2 -> ()
    | _ -> raise (Invalid_argument "element has already been removed")

  let remove l elt =
    let { previous; next; _ } = elt in
    (match previous with
    | Some p -> p.next <- next
    | None -> check_is_first_element l elt; l := next);
    (match next with
    | Some n -> n.previous <- previous;
    | None -> ());
    elt.previous <- None;
    elt.next <- None

  let iter l ~f =
    let rec loop = function
      | Some { value; next; _ } -> f value; loop next
      | None -> ()
    in
    loop !l

  (* Find the element containing x, using = for comparison *)
  let find l x =
    let rec search = function
      | None -> None
      | Some elt ->
        if value elt = x then Some elt
        else search (next elt)
    in
    search !l
  ;;

  let iterator list =
    let current = ref !list in
    object
      method has_value = !current <> None
      method value =
        match !current with
        | Some { value; _ } -> value
        | None -> raise (Invalid_argument "next")
      method next =
        match !current with
        | Some { next; _ } -> current := next
        | None -> raise (Invalid_argument "next")
      method remove =
        match !current with
        | Some elt ->
          current := elt.next;
          remove list elt
        | None -> raise (Invalid_argument "remove")
      method insert_after value =
        match !current with
        | Some elt -> ignore (insert_after elt value)
        | None -> raise (Invalid_argument "insert_after")
    end

   let find_it list ~data =
     let it = iterator list in
     while it#has_value && it#value <> data do
       it#next
     done;
     it
end

let () =
  Printf.printf "DList1\n";
  let l = Dlist.create () in
  let e1 = Dlist.insert_first l 10 in
  Printf.printf "Item: %d\n" (Dlist.value e1);
  ignore (Dlist.insert_after (Dlist.insert_after e1 20) 30);

  let item = ref (Dlist.first l) in
  while !item <> None do
    match !item with
    | Some elt ->
      Printf.printf "Item: %d\n" (Dlist.value elt);
      item := Dlist.next elt
    | None ->
      ()
  done


module Dictionary : sig
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t
  val add  : ('a, 'b) t -> key:'a -> data:'b -> unit
  val find : ('a, 'b) t -> key:'a -> 'b option
  val iter : ('a, 'b) t -> f:(key:'a -> data:'b -> unit) -> unit
end = struct
  type ('a, 'b) t = ('a * 'b) list array

  let num_buckets = 17
  let hash_bucket key = (Hashtbl.hash key) mod num_buckets

  let create () = Array.create ~len:num_buckets []

  let add table ~key ~data =
    let index = hash_bucket key in
    table.(index) <- (key, data) :: table.(index)

  let find table ~key =
    List.find_map table.(hash_bucket key)
      ~f:(fun (key',data) -> if key' = key then Some data else None)

  let iter table ~f =
    for i = 0 to num_buckets - 1 do
      List.iter table.(i) ~f:(fun (key, data) -> f ~key ~data)
    done
end

module Iterable_dictionary : sig
  type 'a iterator = < has_value : bool; value : 'a; next : unit; remove : unit >
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val iterator : ('a, 'b) t -> ('a * 'b) iterator
  val find : ('a, 'b) t -> key:'a -> ('a * 'b) iterator
end = struct
  type 'a iterator = < has_value : bool; value : 'a; next : unit; remove : unit >
  type ('a, 'b) t = ('a * 'b) Dlist.t array

  let num_buckets = 17
  let hash_bucket key = (Hashtbl.hash key) mod num_buckets

  let create () = Array.init num_buckets ~f:(fun _ -> Dlist.create ())

  let add table ~key ~data =
    let index = hash_bucket key in
    let it = Dlist.find_it table.(index) ~data:(key, data) in
    if it#has_value then it#remove;
    ignore (Dlist.insert_first table.(index) (key, data))

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
          dlist_it <- Dlist.iterator table.(index)
        done

      initializer self#normalize

    end

  let iterator table =
    make_iterator table 0 (Dlist.iterator table.(0))

  let find table ~key =
    let index = hash_bucket key in
    let it = Dlist.iterator table.(index) in
    while it#has_value && fst it#value <> key do
      it#next
    done;
    if it#has_value then
       make_iterator table index it
    else
       make_iterator table num_buckets it
end


let () =
  let module IHM = Iterable_dictionary in
  let table = IHM.create () in
  IHM.add table ~key:"small"    ~data:1.00;
  IHM.add table ~key:"medium"   ~data:1.50;
  IHM.add table ~key:"large"    ~data:2.25;
  IHM.add table ~key:"enormous" ~data:5.00;

  let it = IHM.iterator table in
  while it#has_value do
    let size, price = it#value in
    Printf.printf "Size %s is $%.02f\n" size price;
    it#next
  done;

  let it = IHM.find table ~key:"enormous" in
  it#remove;

  let it = IHM.iterator table in
  while it#has_value do
    let size, price = it#value in
    Printf.printf "Size %s is $%.02f\n" size price;
    it#next
  done;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
