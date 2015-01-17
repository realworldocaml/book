open Core.Std
open Iterator_intf

let iterator_of_dlist list =
object

  (* The current location of the iterator *)
  val mutable current = Dlist.first list

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

let to_list dl =
  let rec el_to_list el_opt acc =
    match el_opt with
    | None -> List.rev acc
    | Some el ->
      el_to_list (Dlist.next el) (Dlist.value el :: acc)
  in
  el_to_list (Dlist.first dl) []

let () =
  let l = Dlist.create () in
  ignore (Dlist.insert_first l 3);
  ignore (Dlist.insert_first l 2);
  ignore (Dlist.insert_first l 1);
  assert (to_list l = [1;2;3]);
  let drop_first l exp =
    let first = Option.value_exn (Dlist.first l) in
    Dlist.remove l first;
    let list = to_list l in
    printf "%s\n" (Sexp.to_string_hum (List.sexp_of_t Int.sexp_of_t list));
    assert (list = exp)
  in
  drop_first l [2;3];
  drop_first l [3];
  drop_first l [];
;;

let () =
  printf "Tests complete\n"
