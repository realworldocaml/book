(*
   Utilities for interpreting annotations of type Ast.annot.
*)

open Import

type t = Ast.annot

let error_at loc s =
  failwith (sprintf "%s:\n%s" (Ast.string_of_loc loc) s)

let has_section k l =
  try ignore (List.assoc k l); true
  with Not_found -> false

let has_field ~sections:k ~field:k2 l =
  List.exists (
    fun k1 ->
      try
        (* each section must be unique *)
        let _, l2 = List.assoc k1 l in
        ignore (List.assoc k2 l2);
        true
      with Not_found -> false
  ) k

let get_flag ~sections:k ~field:k2 l =
  let result =
    List.find_map (fun k1 ->
        try
          (* each section must be unique *)
          let _, l2 = List.assoc k1 l in
          let loc, o = List.assoc k2 l2 in
          match o with
              None -> Some true
            | Some "true" -> Some true
            | Some "false" -> Some false
            | Some s ->
                error_at loc
                  (sprintf "Invalid value %S for flag %s.%s" s k1 k2)
        with Not_found -> None
    ) k
  in
  match result with
      None -> false
    | Some x -> x

let get_field ~parse ~default ~sections:k ~field:k2 l =
  let result =
    List.find_map (fun k1 ->
        try
          (* each section must be unique *)
          let _, l2 = List.assoc k1 l in
          let loc, o = List.assoc k2 l2 in
          match o with
              Some s ->
                (match parse s with
                     Some _ as y -> y
                   | None ->
                       error_at loc
                         (sprintf "Invalid annotation <%s %s=%S>" k1 k2 s)
                )
            | None ->
                error_at loc
                  (sprintf "Missing value for annotation %s.%s" k1 k2)
        with Not_found ->
          None
    ) k
  in
  match result with
      None -> default
    | Some x -> x

let get_opt_field ~parse ~sections ~field l =
  let parse s =
    match parse s with
    | None -> None (* indicates parse error *)
    | Some v -> Some (Some v)
  in
  get_field ~parse ~default:None ~sections ~field l

let set_field ~loc ~section:k ~field:k2 v l : Ast.annot =
  try
    let section_loc, section = List.assoc k l in
    let section =
      try
        let _field = List.assoc k2 section in
        List.assoc_update k2 (loc, v) section
      with Not_found ->
        (k2, (loc, v)) :: section
    in
    List.assoc_update k (section_loc, section) l

  with Not_found ->
    (k, (loc, [ k2, (loc, v) ])) :: l


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
