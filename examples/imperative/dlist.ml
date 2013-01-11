(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Author: Jason Hickey
 * @email{jasonh@gmail.com}
 * @end[license]
 *)

(************************************************************************
 * Core-style
 *)

module DList1 : sig
   type 'a t
   type 'a element

   val create : unit -> 'a t
   val is_empty : 'a t -> bool

   val value : 'a element -> 'a

   val first : 'a t -> 'a element option
   val next : 'a element -> 'a element option
   val previous : 'a element -> 'a element option

   val insert_first : 'a t -> 'a -> 'a element
   val insert_after : 'a element -> 'a -> 'a element
   val remove : 'a t -> 'a element -> unit

   val iter : ('a -> unit) -> 'a t -> unit
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
     let elt = { previous = None; next = !l; value } in
     begin match !l with
     | Some old_first -> old_first.previous <- Some elt
     | None -> ()
     end;
     l := Some elt;
     elt

   let insert_after elt value =
     let new_elt = { value; previous = Some elt; next = elt.next } in
     begin match elt.next with
     | Some old_next -> old_next.previous <- Some new_elt
     | None -> ()
     end;
     elt.next <- Some new_elt;
     new_elt

   let check_is_first l elt1 =
      match !l with
      | Some elt2 when elt1 == elt2 -> ()
      | _ -> raise (Invalid_argument "element has already been removed")

   let remove l elt =
     let { previous = previous; next = next } = elt in
     (match previous with
      | Some p -> p.next <- next
      | None -> check_is_first_element l elt; l := next);
     (match next with
      | Some n -> n.previous <- previous;
      | None -> ());
     elt.previous <- None;
     elt.next <- None

   let iter f l =
     let rec loop = function
     | Some { value; next = next } -> f value; loop next
     | None -> ()
     in
     loop !l
end

let () =
   let l = DList1.create () in
   let e1 = DList1.insert_first l 10 in
   Printf.printf "Item: %d\n" (DList1.value e1);
   ignore (DList1.insert_after (DList1.insert_after e1 20) 30);

   let item = ref (DList1.first l) in
   while !item <> None do
      match !item with
      | Some elt ->
           Printf.printf "Item: %d\n" (DList1.value elt);
           item := DList1.next elt
      | None ->
           ()
   done

(************************************************************************
 * Iterator
 *)

type 'a iterator =
   < has_value : bool; value : 'a; next : unit; remove : unit >

module DList : sig
   type 'a t

   val create : unit -> 'a t
   val is_empty : 'a t -> bool

   val push_front : 'a t -> data:'a -> unit
   val front : 'a t -> 'a
   val pop_front : 'a t -> 'a

   val iter : ('a -> unit) -> 'a t -> unit

   val iterator : 'a t -> 'a iterator
   val find : 'a t -> data:'a -> 'a iterator
end = struct
   type 'a element =
      { value : 'a;
        mutable next : 'a element option;
        mutable previous : 'a element option
      }

   type 'a t = 'a element option ref

   let create () = ref None

   let is_empty l = !l = None

   let push_front l ~data =
     let new_front = { value = data; next = None; previous = None } in
     begin match !l with
      | Some el ->
        el.previous <- Some new_front;
        new_front.next <- Some el
      | None ->
        ()
      end;
      l := Some new_front

   let front = function
    | { contents = Some { value = v } } -> v
    | { contents = None } -> raise (Invalid_argument "front")

   let pop_front l =
     match !l with
      | Some { value = v; next = None } -> l := None; v
      | Some { value = v; next = (Some el) as next } ->
        l := next;
        el.previous <- None;
        v
      | None -> raise (Invalid_argument "pop_front")

   let iterator (list : 'a t) =
     let current = ref !list in
     object
       method has_value = !current <> None
       method value =
         match !current with
          | Some { value = v } -> v
          | None -> raise (Invalid_argument "next")
       method next =
         match !current with
          | Some { next = next } -> current := next
          | None -> raise (Invalid_argument "next")
       method remove =
         match !current with
          | Some { previous = previous; next = next } ->
               (match previous with
                 | Some el -> el.next <- next
                 | None -> list := next);
               (match next with
                 | Some el -> el.previous <- previous;
                 | None -> ());
               current := next
          | None -> raise (Invalid_argument "remove")
     end

   let iter f l =
     let rec loop = function
      | Some { value = v; next = next } -> f v; loop next
      | None -> ()
     in
     loop !l

   let find l ~data =
      let it = iterator l in
      while it#has_value && it#value <> data do
         it#next
      done;
      it
end

let l = DList.create ();;
DList.push_front l 1;;
DList.push_front l 2;;
DList.push_front l 3;;
DList.iter (Printf.printf "%d\n") l;;

let it = DList.iterator l;;

Printf.printf "Item: %d\n" it#value;
it#next;
Printf.printf "Item: %d\n" it#value;
it#remove;
Printf.printf "Item: %d\n" it#value;
it#next;
Printf.printf "has_value=%b\n" it#has_value;
DList.iter (Printf.printf "%d\n") l;;

(*
 * -*-
 * Local Variables:
 * Fill-column: 100
 * End:
 * -*-
 * vim:ts=3:et:tw=100
 *)
