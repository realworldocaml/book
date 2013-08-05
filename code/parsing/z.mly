(* part 4 *)
opt_object_fields:
  | fields = rev_object_fields
    { List.rev obj }
  ;

rev_object_fields:
  | /* empty */ { [] }
  | obj = rev_object_fields; COMMA; k = ID; COLON; v = value
    { (k, v) :: obj }
  ;
