open Core.Std

type 'a element =
  { value : 'a;
    mutable next : 'a element option;
    mutable prev : 'a element option
  }

type 'a t = 'a element option ref

let create () = ref None
let is_empty l = !l = None

let value elt = elt.value

let first l = !l
let next elt = elt.next
let prev elt = elt.prev

let insert_first l value =
  let new_elt = { prev = None; next = !l; value } in
  begin match !l with
  | Some old_first -> old_first.prev <- Some new_elt
  | None -> ()
  end;
  l := Some new_elt;
  new_elt

let insert_after elt value =
  let new_elt = { value; prev = Some elt; next = elt.next } in
  begin match elt.next with
  | Some old_next -> old_next.prev <- Some new_elt
  | None -> ()
  end;
  elt.next <- Some new_elt;
  new_elt

let check_is_first_element l elt1 =
  match !l with
  | Some elt2 when phys_equal elt1 elt2 -> ()
  | _ -> raise (Invalid_argument "element has already been removed")

let remove l elt =
  let { prev; next; _ } = elt in
  (match prev with
  | Some p -> p.next <- next
  | None -> check_is_first_element l elt; l := next);
  (match next with
  | Some n -> n.prev <- prev;
  | None -> ());
  elt.prev <- None;
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

