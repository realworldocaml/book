open Core.Std
open Iterator_intf

let of_dlist_element list current =
object
  val list = list
  val mutable current = current
  method has_value = current <> None
  method value =
    match current with
    | Some elt -> Dlist.value elt
    | None -> raise (Invalid_argument "next")
  method next =
    match current with
    | Some elt -> current <- Dlist.next elt
    | None -> raise (Invalid_argument "next")
  method remove =
    match current with
    | Some elt ->
      current <- Dlist.next elt;
      Dlist.remove list elt
    | None -> raise (Invalid_argument "remove")
  method insert_after value =
    match current with
    | Some elt -> ignore (Dlist.insert_after elt value)
    | None -> raise (Invalid_argument "insert_after")
end

let iterator_of_dlist list =
  of_dlist_element list (Dlist.first list)


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

let () =
  Printf.printf "\nDList2\n";
  let l = Dlist.create () in
  ignore (Dlist.insert_first l 1);
  ignore (Dlist.insert_first l 2);
  ignore (Dlist.insert_first l 3);
  Dlist.iter ~f:(Printf.printf "%d\n") l;

  let it = iterator_of_dlist l in
  while it#has_value do
    Printf.printf "Item: %d\n" it#value;
    it#next
  done
