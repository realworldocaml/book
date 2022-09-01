(*
   ATD annotations to be interpreted specifically by atdpy.

   Atdpy also honors json-related annotations defined in Atd.Json.
*)

type assoc_repr =
  | List
  | Dict

let get_python_default an : string option =
    Atd.Annot.get_opt_field
      ~parse:(fun s -> Some s)
      ~sections:["python"]
      ~field:"default"
      an

let get_python_assoc_repr an : assoc_repr =
  Atd.Annot.get_field
    ~parse:(function
      | "list" -> Some List
      | "dict" -> Some Dict
      | _ -> None
    )
    ~default:List
    ~sections:["python"]
    ~field:"repr"
    an

(* class decorator

   Later, we might want to add support for decorators on the from_json
   and to_json methods.

   These would have more specific names such as "to_json_decorator"
   and "from_json_decorator" or just "to_json" and "from_json".
   If we want to be consistent with atdgen adapters, we might
   want something like this:

     <json adapter.to_ocaml="Lift.normalize \"body\" [\"a\"]"
           adapter.from_ocaml="Lift.restore \"body\" [\"a\"]"
           adapter.to_python="normalize_whatever"
           adapter.from_python="restore_whatever"
      >

   (which is less flexible than method decorators since method decorators
   are left in charge of calling the origin method but the adapters
   are simple functions from json to json)
*)
let get_python_decorators an : string list =
  Atd.Annot.get_fields
    ~parse:(fun s -> Some s)
    ~sections:["python"]
    ~field:"decorator"
    an

(* imports etc. *)
let get_python_text an : string list =
  Atd.Annot.get_fields
    ~parse:(fun s -> Some s)
    ~sections:["python"]
    ~field:"text"
    an

let get_python_json_text an : string list =
  get_python_text an
  @ Atd.Annot.get_fields
    ~parse:(fun s -> Some s)
    ~sections:["python"]
    ~field:"json_py.text"
    an
