/* Inefficient right-recursive rule */
object_fields:
    k = ID; COLON; v = value
    { [k, v] }
  | k = ID; COLON; v = value; COMMA; obj = object_fields
    { (k, v) :: obj }
