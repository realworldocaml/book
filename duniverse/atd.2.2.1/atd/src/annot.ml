(*
   Utilities for interpreting annotations of type Ast.annot.
*)

open Import

type t = Ast.annot

let error_at loc s =
  failwith (sprintf "%s:\n%s" (Ast.string_of_loc loc) s)

let field ~section ~field l =
  let fieldmatches = List.filter_map (fun (s, (_, fs)) ->
    if s = section then Some fs else None) l
    |> List.map (fun fs ->
      List.filter_map (fun (f, (l, s)) ->
        if f = field then Some (l, s) else None)
      fs)
    |> List.flatten in
  match fieldmatches with
  | [fieldmatch] -> Some fieldmatch
  | (loc, _) :: others -> error_at loc
    (sprintf "Duplicate annotation %s.%s (also in:\n  %s\n)" section field
    (List.map (fun (loc, _) -> (Ast.string_of_loc loc)) others
     |> String.concat ",\n  "))
  | _ -> None

let has_section k l =
  Option.is_some (List.assoc k l)

let has_field ~sections:k ~field:k2 l =
  List.exists (fun k1 ->
    field ~section:k1 ~field:k2 l
    |> Option.is_some
  ) k

let get_flag ~sections:k ~field:k2 l =
  k
  |> List.find_map (fun k1 ->
    field ~section:k1 ~field:k2 l
    |> Option.map (fun (loc, o) ->
      match o with
      | None | Some "true" -> true
      | Some "false" -> false
      | Some s ->
          error_at loc
            (sprintf "Invalid value %S for flag %s.%s" s k1 k2)))
  |> Option.value ~default:false

let get_field ~parse ~default ~sections:k ~field:k2 l =
  k
  |> List.find_map (fun k1 ->
    let open Option.O in
    field l ~section:k1 ~field:k2 >>= fun (loc, o) ->
    match o with
    | Some s ->
        (match parse s with
           Some _ as y -> y
         | None ->
             error_at loc
               (sprintf "Invalid annotation <%s %s=%S>" k1 k2 s))
    | None ->
        error_at loc
          (sprintf "Missing value for annotation %s.%s" k1 k2))
  |> Option.value ~default

let get_opt_field ~parse ~sections ~field l =
  let parse s =
    match parse s with
    | None -> None (* indicates parse error *)
    | Some v -> Some (Some v)
  in
  get_field ~parse ~default:None ~sections ~field l

let set_field ~loc ~section:k ~field:k2 v l : Ast.annot =
  match List.assoc k l with
  | None -> (k, (loc, [ k2, (loc, v) ])) :: l
  | Some (section_loc, section) ->
      let section_loc, section = List.assoc_exn k l in
      let section =
        match List.assoc k2 section with
        | None -> (k2, (loc, v)) :: section
        | Some _ -> List.assoc_update k2 (loc, v) section
      in
      List.assoc_update k (section_loc, section) l

let collapse merge l =
  let tbl = Hashtbl.create 10 in
  let n = ref 0 in

  List.iter (
    fun (s1, f1) ->
      incr n;
      try
        let _, f2 = Hashtbl.find tbl s1 in
        Hashtbl.replace tbl s1 (!n, merge f1 f2)
      with Not_found ->
        Hashtbl.add tbl s1 (!n, f1)
  ) (List.rev l);

  let l = Hashtbl.fold (fun s (i, f) l -> (i, (s, f)) :: l) tbl [] in
  let l = List.sort (fun (i, _) (j, _) -> compare j i) l in
  List.map snd l

let override_values x1 _ = x1

let override_fields (loc1, l1) (_, l2) =
  (loc1, collapse override_values (l1 @ l2))

let merge l =
  collapse override_fields l

let create_id =
  let n = ref (-1) in
  fun () ->
    incr n;
    if !n < 0 then
      failwith "Annot.create_id: counter overflow"
    else
      string_of_int !n
