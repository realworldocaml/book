

(* part "4" *)
(* Quadratic left-recursive rule *)
object_fields:
  | (* empty *) { [] }
  | obj = object_fields; COMMA; k = ID; COLON; v = value
    { obj @ [k, v] }
  ;
