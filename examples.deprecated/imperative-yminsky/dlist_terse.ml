open Core.Std

type 'a iterator =
  { next_el : (unit -> unit Or_error.t);
    current : (unit -> 'a option);
    remove  : (unit -> unit Or_error.t);
  }

module DList : sig
   type 'a t

   val create : unit -> 'a t
   val is_empty : 'a t -> bool

   val push_front : 'a t -> data:'a -> unit
   val front      : 'a t -> 'a option
   val pop_front  : 'a t -> 'a option

   val iter : ('a -> unit) -> 'a t -> unit

   val iterator : 'a t -> 'a iterator
   val find     : 'a t -> data:'a -> 'a iterator option
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
    | { contents = Some { value; _ } } -> Some value
    | { contents = None } -> None

   let pop_front l =
     match !l with
      | Some { value; next = None; _ } -> l := None; Some value
      | Some { value; next = (Some el) as next; _ } ->
        l := next;
        el.previous <- None;
        Some value
      | None -> None

   let iterator t =
     let current = ref !t in
     { current =
         (fun () -> Option.map !current ~f:(fun { value; _ } -> value));
       next_el =
         (fun () ->
           match !current with
           | Some { next; _ } -> current := next; Ok ()
           | None -> Or_error.error_string "No next element");
       remove =
         (fun () ->
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
     }

   let iter f l =
     let rec loop = function
      | Some { value; next; _ } -> f value; loop next
      | None -> ()
     in
     loop !l

   let find l ~data =
      let it = iterator l in
      let rec loop () =
        match it.current () with
        | None -> None
        | Some data' ->
          if data' = data then Some it
          else
            match it.next_el () with
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
    match it.current () with
    | Some x -> printf "Item: %d\n" x
    | None -> printf "No current item"
  in
  print_current ();
  ignore (it.next_el ());
  print_current ();
  ignore (it.remove ());
  print_current ();
  ignore (it.next_el ());
  print_current ();
  DList.iter (fun x -> Printf.printf "%d\n" x) l

