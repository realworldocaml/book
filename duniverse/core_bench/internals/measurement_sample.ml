open Core

type t =
  { mutable runs : int
  ; mutable cycles : Int63.t
  ; mutable nanos : Int63.t
  ; mutable compactions : int
  ; mutable minor_allocated : int
  ; mutable major_allocated : int
  ; mutable promoted : int
  ; mutable major_collections : int
  ; mutable minor_collections : int
  }
[@@deriving sexp, fields]

let create () =
  { runs = 0
  ; cycles = Int63.zero
  ; nanos = Int63.zero
  ; compactions = 0
  ; minor_allocated = 0
  ; major_allocated = 0
  ; promoted = 0
  ; major_collections = 0
  ; minor_collections = 0
  }
;;

let field_names () =
  let field_name accum f = Field.name f :: accum in
  Fields.fold
    ~init:[]
    ~runs:field_name
    ~cycles:field_name
    ~nanos:field_name
    ~compactions:field_name
    ~minor_allocated:field_name
    ~major_allocated:field_name
    ~promoted:field_name
    ~minor_collections:field_name
    ~major_collections:field_name
  |> List.rev
;;

let field_names_to_string () = String.concat ~sep:"," (field_names ())

let field_values_to_string t =
  let prepend_int accum field = Int.to_string (Field.get field t) :: accum in
  let prepend_int63 accum field = Int63.to_string (Field.get field t) :: accum in
  Fields.fold
    ~init:[]
    ~runs:prepend_int
    ~nanos:prepend_int63
    ~minor_collections:prepend_int
    ~major_collections:prepend_int
    ~compactions:prepend_int
    ~cycles:prepend_int63
    ~minor_allocated:prepend_int
    ~major_allocated:prepend_int
    ~promoted:prepend_int
  |> List.rev
  |> String.concat ~sep:","
;;

(* Can handle current format ("100,200,300") and previous format ("100 200 300 "). *)
let of_field_values_string line =
  let parts = String.split_on_chars ~on:[ ','; ' ' ] (String.strip line) in
  let t = create () in
  let set_value of_string vs field =
    match vs with
    | v :: vs ->
      (Option.value_exn (Field.setter field)) t (of_string v);
      vs
    | [] -> failwith "Too few columns in saved metrics file."
  in
  let set_int_value = set_value Int.of_string in
  let set_int63_value = set_value Int63.of_string in
  (match
     Fields.fold
       ~init:parts
       ~runs:set_int_value
       ~nanos:set_int63_value
       ~minor_collections:set_int_value
       ~major_collections:set_int_value
       ~compactions:set_int_value
       ~cycles:set_int63_value
       ~minor_allocated:set_int_value
       ~major_allocated:set_int_value
       ~promoted:set_int_value
   with
   | [] -> ()
   | ls ->
     failwithf
       "Too many data values in saved metrics file: %s"
       (String.concat ~sep:"," ls)
       ());
  t
;;

let max stats ~len ~field =
  let x = field stats.(0) in
  let rec loop i x = if i < len then loop (i + 1) (Int.max x (field stats.(i))) else x in
  loop 1 x
;;

let floatify_int = Fn.compose Float.of_int
let floatify_int63 = Fn.compose Int63.to_float

let accessor = function
  | `Runs -> floatify_int runs
  | `Minor_collections -> floatify_int minor_collections
  | `Major_collections -> floatify_int major_collections
  | `Compactions -> floatify_int compactions
  | `Nanos -> floatify_int63 nanos
  | `Cycles -> floatify_int63 cycles
  | `Minor_allocated -> floatify_int minor_allocated
  | `Major_allocated -> floatify_int major_allocated
  | `Promoted -> floatify_int promoted
  | `One -> fun _ -> 1.
;;
