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

%type <Json.value option> prog

%start prog

%%

prog: v = value
    { Some v }
  | EOF
    { None }
  ;

value: LEFT_BRACE; obj = opt_object_fields; RIGHT_BRACE
    { `Object obj }
  | LEFT_BRACK; vl = array_values; RIGHT_BRACK
    { `Array vl }
  | s = STRING
    { `String s }
  | i = INT
    { `Int i }
  | x = FLOAT
    { `Float x }
  | TRUE
    { `True }
  | FALSE
    { `False }
  | NULL
    { `Null }
  ;


opt_object_fields: /* empty */
    { [] }
  | obj = rev_object_fields
    { List.rev obj }
  ;

rev_object_fields: k = ID; COLON; v = value
    { [k, v] }
  | obj = rev_object_fields; COMMA; k = ID; COLON; v = value
    { (k, v) :: obj }
  ;

array_values: /* empty */
    { [||] }
  | vl = rev_values
    { Array.of_list (List.rev vl) }
  ;

rev_values: v = value
    { [v] }
  | vl = rev_values; COMMA; v = value
    { v :: vl }
  ;
