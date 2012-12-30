open Core.Std

type 'a iterator =
   < has_value : bool; value : 'a; next : unit; remove : unit >

module Iterator : sig
  type 'a t
  val create
    :  next    : (unit -> unit Or_error.t)
    -> current : (unit -> 'a option)
    -> remove  : (unit -> unit Or_error.t)
    -> 'a t

  val next    : 'a t -> unit Or_error.t
  val current : 'a t -> 'a option
  val remove  : 'a t -> unit Or_error.t
end = struct
  type 'a t = { next : (unit -> unit Or_error.t);
                current : (unit -> 'a option);
                remove : (unit -> unit Or_error.t);
              }

  let create ~next ~current ~remove = { next; current; remove }
  let next t = t.next ()
  let remove t = t.remove ()
  let current t = t.current ()
end

module DList : sig
   type 'a t

   val create : unit -> 'a t
   val is_empty : 'a t -> bool

   val push_front : 'a t -> data:'a -> unit
   val front : 'a t -> 'a
   val pop_front : 'a t -> 'a

   val iter : ('a -> unit) -> 'a t -> unit

   val iterator : 'a t -> 'a Iterator.t
   val find : 'a t -> data:'a -> 'a Iterator.t option
end = struct
  type 'a element =
    { value : 'a;
      mutable next     : 'a element option;
      mutable previous : 'a element option;
    }

   type 'a t = 'a element option ref

   let create () =
     ref None

   let is_empty l =
     !l = None

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
    | { contents = Some { value; _ } } -> value
    | { contents = None } -> raise (Invalid_argument "front")

   let pop_front l =
     match !l with
      | Some { value; next = None; _ } -> l := None; value
      | Some { value; next = (Some el) as next; _ } ->
        l := next;
        el.previous <- None;
        value
      | None -> raise (Invalid_argument "pop_front")

   let iterator t =
     let current = ref !t in
     Iterator.create
       ~current:(fun () ->
         Option.map !current ~f:(fun { value; _ } -> value))
       ~next:(fun () ->
         match !current with
         | Some { next; _ } -> current := next; Ok ()
         | None -> Or_error.error_string "No next element")
       ~remove:(fun () ->
         match !current with
         | Some { previous; next; _ } ->
           (match previous with
           | Some el -> el.next <- next
           | None -> t := next);
           (match next with
           | Some el -> el.previous <- previous;
           | None -> ());
           current := next;
           Ok ()
         | None -> Or_error.error_string "No element to remove")

   let iter f l =
     let rec loop = function
      | Some { value; next; _ } -> f value; loop next
      | None -> ()
     in
     loop !l

   let find l ~data =
      let it = iterator l in
      let rec loop () =
        match Iterator.current it with
        | None -> None
        | Some data' ->
          if data' = data then Some it
          else
            match Iterator.next it with
            | Error _ -> None
            | Ok () -> loop ()
      in
      loop ()

end

let () =
  let l = DList.create () in
  DList.push_front l ~data:1;
  DList.push_front l ~data:2;
  DList.push_front l ~data:3;
  DList.iter (Printf.printf "%d\n") l;

  let it = DList.iterator l in
  let print_current () =
    match Iterator.current it with
    | Some x -> printf "Item: %d\n" x
    | None -> printf "No current item"
  in
  print_current ();
  ignore (Iterator.next it);
  print_current ();
  ignore (Iterator.remove it);
  print_current ();
  ignore (Iterator.next it);
  print_current ();
  DList.iter (fun x -> Printf.printf "%d\n" x) l

