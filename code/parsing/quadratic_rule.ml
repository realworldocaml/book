/* Quadratic left-recursive rule */
object_fields:
    k = ID; COLON; v = value
    { [k, v] }
  | obj = rev_object_fields; COMMA; k = ID; COLON; v = value
    { obj @ [k, v] }
  ;
