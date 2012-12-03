(*
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Author: Jason Hickey
 * @email{jasonh@gmail.com}
 * @end[license]
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
end = struct
   type 'a element =
      { mutable value : 'a;
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
