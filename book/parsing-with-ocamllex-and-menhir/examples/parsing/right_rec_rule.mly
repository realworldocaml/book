(* Inefficient right-recursive rule *)
object_fields:
  | (* empty *) { [] }
  | k = ID; COLON; v = value; COMMA; obj = object_fields
    { (k, v) :: obj }
