(** A module internal to [Core_bench]. Please look at {!Bench}.

   Contains the measurements of one run of a benchmark. *)
open Core

type t = {
  mutable runs              : int;
  mutable cycles            : int;
  mutable nanos             : int;
  mutable compactions       : int;
  mutable minor_allocated   : int;
  mutable major_allocated   : int;
  mutable promoted          : int;
  mutable major_collections : int;
  mutable minor_collections : int;
} [@@deriving sexp, fields]

let create () = {
  runs              = 0;
  cycles            = 0;
  nanos             = 0;
  compactions       = 0;
  minor_allocated   = 0;
  major_allocated   = 0;
  promoted          = 0;
  major_collections = 0;
  minor_collections = 0;
}

let field_names () =
  let field_name accum f = Field.name f :: accum in
  Fields.fold ~init:[] ~runs:field_name ~cycles:field_name ~nanos:field_name
    ~compactions:field_name ~minor_allocated:field_name ~major_allocated:field_name
    ~promoted:field_name ~minor_collections:field_name ~major_collections:field_name
  |> List.rev

let field_names_to_string () = String.concat ~sep:"," (field_names ())

let to_int_list t =
  let prepend accum field = Field.get field t :: accum in
  Fields.fold ~init:[]
    ~runs:prepend
    ~nanos:prepend
    ~minor_collections:prepend
    ~major_collections:prepend
    ~compactions:prepend
    ~cycles:prepend
    ~minor_allocated:prepend
    ~major_allocated:prepend
    ~promoted:prepend
  |> List.rev

let field_values_to_string t =
  to_int_list t
  |> List.map ~f:Int.to_string
  |> String.concat ~sep:","

(* Can handle current format ("100,200,300") and previous format ("100 200 300 "). *)
let of_field_values_string line =
  let parts = String.split_on_chars ~on:[','; ' '] (String.strip line) in
  let t = create () in
  let set_value vs field =
    match vs with
    | v :: vs -> (Option.value_exn (Field.setter field)) t (Int.of_string v); vs
    | [] -> failwith "Too few columns in saved metrics file."
  in
  begin match (Fields.fold ~init:parts
                 ~runs:set_value
                 ~nanos:set_value
                 ~minor_collections:set_value
                 ~major_collections:set_value
                 ~compactions:set_value
                 ~cycles:set_value
                 ~minor_allocated:set_value
                 ~major_allocated:set_value
                 ~promoted:set_value) with
  | [] -> ()
  | ls -> failwithf "Too many data values in saved metrics file: %s"
            (String.concat ~sep:"," ls) ()
  end;
  t

let max stats ~len ~field =
  let x = field stats.(0) in
  let rec loop i x =
    if i < len
    then loop (i+1) (Int.max x (field stats.(i)))
    else x
  in loop 1 x

let floatify = Fn.compose Float.of_int

let accessor = function
  | `Runs              -> floatify runs
  | `Minor_collections -> floatify minor_collections
  | `Major_collections -> floatify major_collections
  | `Compactions       -> floatify compactions
  | `Nanos             -> floatify nanos
  | `Cycles            -> floatify cycles
  | `Minor_allocated   -> floatify minor_allocated
  | `Major_allocated   -> floatify major_allocated
  | `Promoted          -> floatify promoted
  | `One               -> fun _ -> 1.

