open Core.Std

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
let find l x : 'a element option =
  let rec search = function
    | None -> None
    | Some elt ->
      if value elt = x then Some elt
      else search (next elt)
  in
  search !l

let () =
  Printf.printf "DList1\n";
  let l = create () in
  let e1 = insert_first l 10 in
  Printf.printf "Item: %d\n" (value e1);
  ignore (insert_after (insert_after e1 20) 30);

  let item = ref (first l) in
  while !item <> None do
    match !item with
    | Some elt ->
      Printf.printf "Item: %d\n" (value elt);
      item := next elt
    | None ->
      ()
  done

(* Defining iterators *)
type 'a iterator =
  < has_value : bool;
  value : 'a;
  next : unit;
  remove : unit;
  insert_after : 'a -> unit
                 >

    let iterator (list : 'a t) : 'a iterator =
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

let () =
  Printf.printf "\nDList2\n";
  let l = create () in
  ignore (insert_first l 1);
  ignore (insert_first l 2);
  ignore (insert_first l 3);
  iter ~f:(Printf.printf "%d\n") l;

  let it = iterator l in
  while it#has_value do
    Printf.printf "Item: %d\n" it#value;
    it#next
  done
