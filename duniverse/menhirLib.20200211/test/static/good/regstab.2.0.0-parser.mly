(* Original file: regstab.2.0.0/regstab-2.0.0/src/executables/parser.mly *)
%{
  (** Parses a schema with possible let-bindings and returns the corresponding {!Human.schema} value. *)
  open Softcore
  open Schema
  open Constraint
  module H = Human

  (** {6 AST definition} *)

  (** Type of the AST. This contains more than just schemata: we also have to deal with let-bindings and constraints. *)
  type ast = { defs : definition list; sch : sch; cstr : H.cstr }

  (** A definition is a let-binding, i.e. a name, a list of parameters and a schema (the body of the let-binding) *)
  and definition = string * param list * sch

  (** A schema. Very close to {!Human.schema} except that negation is allowed (i.e. we accept non-n.n.f. schemata as input),
   * and function calls (i.e. references to let-bindings) can be used. *)
  and sch =
    |Top
    |Bot
    |Op of H.connective * sch * sch 
    |Neg of sch
    |Atom of string * H.indexes
    |It of H.big_connective * string * int * H.upper_bound * sch
    |FunctionCall of string * arg list

  (** (propositional or term) Arguments to a function call. *)
  and arg = PropArg of string | TermeArg of H.indexes

  (** (propositional or term) parameters to a let-binding. *)
  and param = PropParam of string | TermeParam of string

  (** {6 Environments used for the evaluation of the AST} *)

  (** Environments in order to evaluate let-bindings. *)
  type environment = {
    defs_map : def_environment;
    prop_map : prop_environment;
    var_map : var_environment;
  }

  (** Content of a let-binding: a body and a list of parameters. *)
  and def_content = { body : sch; params : param list }

  (** Mapping "definition name" -> "definition content" *)
  and def_environment  = (string, def_content) Map.t

  (** Mapping "propositional variable name" -> "propositional variable name"
   * (the left one is a local name, the right one is its outer substitute when called). *)
  and prop_environment = (string, string) Map.t

  (** Mapping "integer variable name" -> "integer variable name"
   * (the left one is a local name, the right one is its outer substitute when called). *)
  and var_environment  = (string, H.indexes) Map.t

  (** The initial, empty, environment. *)
  let empty_environment = {
    defs_map = Map.empty; 
    prop_map = Map.empty;
    var_map = Map.empty;
  }

  (** Append a definition to an environment. *)
  let append_def ~name ~content env          = { env with defs_map = Map.add env.defs_map ~key:name ~data:content }

  (** Append a propositional variable assignment to an environment. *)
  let append_prop_assignment ~param ~arg env = { env with prop_map = Map.add env.prop_map ~key:param ~data:arg }

  (** Append an integer variable assignment to an environment. *)
  let append_var_assignment ~param ~arg env  = { env with var_map = Map.add env.var_map ~key:param ~data:arg }

  (** Look for a propositional variable in an environment. *)
  let find_in_prop_map ~env x = Map.find_with_default env.prop_map x ~default:x          

  (** Look for an integer variable in an environment. *)
  let find_in_var_map  ~env x = Map.find_with_default env.var_map  x ~default:(H.Sum(x,0))

  (** {6 Evaluation of the AST.} *)

  (** Generic parse error function. *)
  let parse_error s =
    let lexing_pos = symbol_start_pos () in
    let file_string = if lexing_pos.Lexing.pos_fname = "" then "stdin" else "file " ^ lexing_pos.Lexing.pos_fname in
    let line_string = string_of_int lexing_pos.Lexing.pos_lnum in
    let col_string = string_of_int (lexing_pos.Lexing.pos_cnum - lexing_pos.Lexing.pos_bol + 1) in
    let error = s ^ "." in
    raise (Failure ("Error at " ^ file_string ^ ", line " ^ line_string ^ ", character " ^ col_string ^ ":
" ^ error))

  (** Parse error due to an input schema not respecting the conditions of regular schemata. *)
  let parse_error_regular s = parse_error (s ^ " (regular schema required)")

  (** Evaluation of an expression in an environment. *)
  let eval_expr ~env = H.(function
    |Int n    -> Int n
    |Sum(v,k) -> 
        match find_in_var_map ~env v with
        |Int k'     -> Int(k+k')
        |Sum(v',k') -> Sum(v',k+k'))
  
  (** Evaluation of a schema. *)
  let rec eval_sch ~env = 
    function 
    |Top -> Top
    |Bot -> Bot
    |Op(op,s1,s2) -> Op(op,eval_sch ~env s1,eval_sch ~env s2)
    |Neg s -> Neg(eval_sch ~env s)
    |Atom(p,idx) -> Atom(find_in_prop_map ~env p, eval_expr ~env idx)
    |It(op,bv,l,(v,k),s) -> 
        let upper =
          match find_in_var_map ~env v with
          |H.Int _      -> parse_error_regular "Some iteration has an integer for its upper bound (due to your function definitions)"
          |H.Sum(v',k') -> (v',k+k')
        in
        It(op,bv,l,upper,eval_sch ~env s)
    |FunctionCall(f,args) ->
        match Map.find env.defs_map f with
        |None -> parse_error ("Unkown function \"" ^ f ^ "\"")
        |Some def -> 
          if List.length args <> List.length def.params then
              parse_error ("'" ^ f ^ "' is called with " ^ (string_of_int (List.length args))
              ^ " arguments instead of " ^ (string_of_int (List.length def.params)))
          else begin
            let (prop_map,var_map) = List.fold_left2 def.params args ~init:(Map.empty,Map.empty) ~f:(
              fun (prop_acc,var_acc) param arg -> 
              match param,arg with
              |PropParam key,PropArg data   -> Map.add prop_acc ~key ~data:(find_in_prop_map ~env data),var_acc
              |TermeParam key,TermeArg data -> prop_acc, Map.add var_acc ~key ~data:(eval_expr ~env data)
              |PropParam _,TermeArg v       -> 
                  let expr_to_string = function H.Int n -> String.ASCII.of_int n | H.Sum(v,k) -> String.ASCII.of_expr v k in
                  parse_error ("\"" ^ expr_to_string v ^ "\" is an arithmetic expression whereas a propositional one was expected")
              |TermeParam _,PropArg pa      -> parse_error ("\"" ^ pa ^ "\" is a proposition variable whereas an arithmetic one was expected")
            )
            in
            eval_sch ~env:{ env with prop_map = prop_map; var_map = var_map } def.body
          end

  (** Evaluation of a full AST (i.e. schema + constraint + let-bindings). *)
  let eval_ast ~env ast = 
    let env = List.fold ast.defs ~f:(fun env (name,params,body) -> append_def env ~name ~content:{ body = body; params = params }) ~init:empty_environment in
    eval_sch ~env ast.sch, ast.cstr

  (** Conversion to {!Human.schema} (once the schema is evaluated and all the
   * let-bindings are unfolded we can turn the result into a {!Human.schema}. *)
  let rec to_human = 
    let module H = H in
    function
    |Top -> H.Top
    |Bot -> H.Bot
    |Atom(p,H.Int n) -> H.(Lit (Pos, p, H.Int n))
    |Atom(p,H.Sum(v,n)) -> H.(Lit (Pos, p, H.Sum(v,n)))
    |Neg s -> H.complementary (to_human s)
    |Op(op,s1,s2) -> H.Op(op, to_human s1, to_human s2)
    |It(op,v,l,u,s) -> H.It(op,v,l,u, to_human s)
    |FunctionCall _ -> assert false

  (** Main function:
    * - evaluates the AST
    * - turns it into a {!Human.schema}
    * - turns it into a {!Schema.t}
    * The last step might raise exceptions if the schema is not regular, in which case user-level error messages are printed.
    *)
  let regular_schema_of_ast ast =
    let module S = String.ASCII in
    let (sch,cstr) = eval_ast ast ~env:empty_environment in
    let different_bounds ~msg b1 b2 = Printf.sprintf "One iteration has %s bound %s while another has %s bound %s" msg b1 msg b2 in
    let human_sch = to_human sch in
    try (Schema.of_human human_sch, Constraint.of_human cstr, Human.free_variable_of human_sch, Human.first_bound_variable_of human_sch)
    with e ->
      let handle e =
        match e with
        |H.Different_lower_bounds(l1,l2) ->
            different_bounds ~msg:"lower" (S.of_int l1) (S.of_int l2)
        |H.Different_upper_bounds((n1,u1),(n2,u2)) ->
            different_bounds ~msg:"upper" (S.of_expr n1 u1) (S.of_expr n2 u2)
        |Prop.Nested_iteration_disallowed(it) -> 
            let module O = H.Output(S) in
            Printf.sprintf "The iteration %s is contained in another iteration" (O.it_to_string it)
        |Iteration.Only_bound_variable_shall_be_used_in_body(v1,v2) ->
            Printf.sprintf "The variable %s is used in the body of an iteration whose bound variable is %s; only %s shall be used" v2 v1 v1
        |Flat.More_than_one_parameter(n1,n2) ->
            Printf.sprintf "There are more than one parameter: %s and %s; only one shall be used" n1 n2
        |e -> raise e
      in
      parse_error_regular (handle e)
%}
%token <int> INT
%token <string> VAR
%token <string> PROP
%token SAT UNSAT
%token PLUS MINUS LPAREN RPAREN EQUAL
%token LE LT GE GT
%token AND OR NOT ITAND ITOR XOR EQUIV IMPLY
%token COMMA DOTS ST
%token INDEX
%token DEF LET EOF IN

%left EQUIV IMPLY
%left XOR
%left OR
%left AND
%nonassoc ITAND ITOR 
%nonassoc DOTS EQUAL
%left PLUS MINUS
%nonassoc NOT
%nonassoc LPAREN RPAREN
%nonassoc LET DEF COMMA EOF IN

%start main
%type <[`Sat|`Unsat] option * (Schema.t * Constraint.t * string option * string option) > main
%%
main:
  option main { let (_,res) = $2 in Some $1, res} 
  |sentences   { None, regular_schema_of_ast $1 }

option:
  SAT    { `Sat   }
  |UNSAT { `Unsat }

sentences:
  LET def_lhs def_rhs    { { $3#cons with defs = ($2#name, $2#params, $3#body) :: $3#cons.defs } }
  |regschema EOF         { { defs = []; sch = $1;  cstr = H.True } }
  |regschema ST cstr EOF { { defs = []; sch = $1;  cstr = $3      } }
  |EOF                   { { defs = []; sch = Top; cstr = H.True } }
;

def_rhs:
  DEF regschema in_sentences { object method body = $2 method cons = $3 end }
  |EQUAL                     { parse_error "'=' used instead of ':='" }

in_sentences:
  IN sentences { $2 }
  |sentences   { parse_error "Missing 'in' in 'let ... in' statement" }
;

def_lhs:
  PROP LPAREN params RPAREN { object method name = $1 method params = $3 end }
  |PROP                     { object method name = $1 method params = [] end }
;

params:
  PROP               { [PropParam $1]      }
  |VAR               { [TermeParam $1]     }
  |PROP COMMA params { (PropParam $1)::$3  }
  |VAR COMMA params  { (TermeParam $1)::$3 }
;

regschema:
  PROP INDEX terme { Atom($1,$3) }
  |NOT regschema             { Neg $2 }
  |regschema AND regschema   { Op(H.And,$1, $3)   }
  |regschema OR regschema    { Op(H.Or,$1, $3)    }
  |regschema XOR regschema   { Op(H.Xor,$1, $3)   }
  |regschema EQUIV regschema { Op(H.Equiv,$1, $3) }
  |regschema IMPLY regschema { Op(H.Imply,$1, $3) }
  |LPAREN regschema RPAREN   { $2 } 
  |AND VAR EQUAL INT DOTS upper flatschema %prec ITAND { It(H.Big_and,$2,$4,$6,$7) }
  |OR  VAR EQUAL INT DOTS upper flatschema %prec ITOR  { It(H.Big_or,$2,$4,$6,$7) }
  |function_call                                       { FunctionCall($1#name,$1#args) }
;

function_call:
  PROP LPAREN arguments RPAREN { object method name = $1 method args = $3 end }
  |PROP LPAREN RPAREN          { object method name = $1 method args = [] end }
  |PROP AND                    { parse_error "Missing parentheses in function call" }
  |PROP OR                     { parse_error "Missing parentheses in function call" }
  |PROP NOT                    { parse_error "Missing parentheses in function call" }
  |PROP PROP                   { parse_error "Missing parentheses in function call" }
  |PROP EOF                    { parse_error "Missing parentheses in function call" }
;

arguments:
  terme                  { [TermeArg $1]       }
  |PROP                  { [PropArg $1]        }
  |PROP COMMA arguments  { (PropArg $1) :: $3  }
  |terme COMMA arguments { (TermeArg $1) :: $3 }
;

flatschema:
  PROP INDEX terme                     { Atom($1, $3) }
  |NOT flatschema                      { Neg $2 }
  |flatschema AND flatschema           { Op(H.And, $1, $3)   }
  |flatschema OR  flatschema           { Op(H.Or, $1, $3)    }
  |flatschema XOR flatschema           { Op(H.Xor, $1, $3)   }
  |flatschema EQUIV flatschema         { Op(H.Equiv, $1, $3) }
  |flatschema IMPLY  flatschema        { Op(H.Imply, $1, $3) }
  |LPAREN flatschema RPAREN            { $2 } 
  |AND VAR EQUAL INT DOTS  %prec ITAND { parse_error "Iterations cannot be nested (regular schemata)" } 
  |OR VAR EQUAL INT DOTS  %prec ITOR   { parse_error "Iterations cannot be nested (regular schemata)" } 
  |function_call                       { FunctionCall($1#name,$1#args) } 
;

cstr:
  |VAR GE INT    { H.(Atom($1,Ge,$3)) }
  |VAR GT INT    { H.(Atom($1,Gt,$3)) }
  |VAR EQUAL INT { H.(Atom($1,Eq,$3)) }
;

terme:
   VAR            { H.Sum($1,0)   }
   |INT           { H.Int $1        }
   |VAR PLUS  INT { H.Sum($1,$3)  }
   |VAR MINUS INT { H.Sum($1,-$3) }
;

upper:
   VAR            { ($1,0)   }
   |VAR PLUS  INT { ($1,$3)  }
   |VAR MINUS INT { ($1,-$3) }
