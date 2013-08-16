%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token TRUE
%token FALSE
%token NULL
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token COLON
%token COMMA
%token EOF
(* part 1 *)
%start <Json.value option> prog
%%
(* part 2 *)
prog:
  | EOF       { None }
  | v = value { Some v }
  ;

(* part 3 *)
value:
  | LEFT_BRACE; obj = object_fields; RIGHT_BRACE
    { `Assoc obj }
  | LEFT_BRACK; vl = array_values; RIGHT_BRACK
    { `List vl }
  | s = STRING
    { `String s }
  | i = INT
    { `Int i }
  | x = FLOAT
    { `Float x }
  | TRUE
    { `Bool true }
  | FALSE
    { `Bool false }
  | NULL
    { `Null }
  ;

(* part 4 *)
(* Inefficient right-recursive rule *)
object_fields:
  | (* empty *) { [] }
  | k = ID; COLON; v = value; COMMA; obj = object_fields
    { (k, v) :: obj }

(* part 5 *)
array_values: /* empty */
    { [] }
  | vl = rev_values
    { List.rev vl }
  ;

rev_values: v = value
    { [v] }
  | vl = rev_values; COMMA; v = value
    { v :: vl }
  ;
