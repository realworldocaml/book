let rec biniou_of_json = function
    `Null -> `Unit
  | `Bool b -> `Bool b
  | `Int i -> `Svint i
  | `Intlit i -> failwith "Cannot convert big int to biniou"
  | `Float f -> `Float64 f
  | `String s -> `String s
  | `Assoc l ->
      let a =
        Array.map (
          fun (s, x) ->
            (Some s, Bi_io.hash_name s, biniou_of_json x)
        ) (Array.of_list l)
      in
      `Record a

  | `List l ->
      (match l with
           [] -> `Array None
         | l ->
             let a = Array.map biniou_of_json (Array.of_list l) in
             let tag = Bi_io.tag_of_tree a.(0) in
             try
               for i = 1 to Array.length a - 1 do
                 if Bi_io.tag_of_tree a.(i) <> tag then
                   raise Exit
               done;
               `Array (Some (tag, a))
             with Exit ->
               failwith "Cannot convert heterogenous array to biniou"
      )

  | `Tuple l -> `Tuple (Array.map biniou_of_json (Array.of_list l))
  | `Variant (s, o) ->
      let o =
        match o with
            None -> None
          | Some x -> Some (biniou_of_json x)
      in
      `Variant (Some s, Bi_io.hash_name s, o)


let rec json_of_biniou (x : Bi_io.tree) =
  match x with
      `Unit -> `Null
    | `Bool b -> `Bool b
    | `Int8 _ -> failwith "Cannot convert int8 to JSON"
    | `Int16 _ -> failwith "Cannot convert int16 to JSON"
    | `Int32 _ -> failwith "Cannot convert int32 to JSON"
    | `Int64 _ -> failwith "Cannot convert int64 to JSON"
    | `Float32 f
    | `Float64 f -> `Float f
    | `Uvint i -> failwith "Cannot convert uvint to JSON"
    | `Svint i -> `Int i
    | `String s -> `String s
    | `Array None -> `List []
    | `Array (Some (_, a)) -> `List (Array.to_list (Array.map json_of_biniou a))
    | `Tuple a -> `Tuple (Array.to_list (Array.map json_of_biniou a))
    | `Record a ->
        `Assoc (
          Array.to_list (
            Array.map (
              function
                  (Some s, _, x) -> (s, json_of_biniou x)
                | (None, _, _) ->
                    failwith "Cannot convert hashed field name to JSON"
            ) a
          )
        )

    | `Num_variant _ -> failwith "Cannot convert num_variant to JSON"
    | `Variant (Some s, _, Some x) -> `Variant (s, Some (json_of_biniou x))
    | `Variant (Some s, _, None) -> `Variant (s, None)
    | `Variant (None, _, _) ->
        failwith "Cannot convert hashed variant name to JSON"

    | `Table None -> `List [] (* not reversible *)
    | `Table (Some (header, rows)) -> (* not reversible *)
        `List (Array.to_list (Array.map (json_of_row header) rows))

    | `Shared _ -> failwith "Cannot convert shared node to JSON"


and json_of_row header a =
  let n = Array.length header in
  if Array.length a <> n then
    failwith "Malformed biniou table";
  let l = ref [] in
  for i = n - 1 downto 0 do
    let o, _, _ = header.(i) in
    let x = a.(i) in
    match o with
        None -> failwith "Cannot convert hashed field name to JSON"
      | Some s -> l := (s, json_of_biniou x) :: !l
  done;
  `Assoc !l
