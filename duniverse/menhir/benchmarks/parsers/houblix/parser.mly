%{ (* -*- tuareg -*- *)

  open AST
  open Position

  let list_of_list_option o =
    match o with
    | Some l -> l
    | None -> []

  let add_backtick s =
    "`" ^ s ^ "`"

  let binop_name binop =
    Id(add_backtick
        (match binop with
        | PLUS                -> "+"
        | MINUS               -> "-"
        | STAR                -> "*"
        | SLASH               -> "/"
        | EQUALQUESTION       -> "=?"
        | LANGLEEQUALQUESTION -> "<=?"
        | RANGLEEQUALQUESTION -> ">=?"
        | LANGLEQUESTION      -> "<?"
        | RANGLEQUESTION      -> ">?"
        | DOUBLEAMPERSAND     -> "&&"
        | PIPEPIPE            -> "||"
        | _                   -> failwith "not a binop (should never be reached no matter user input)"))

%}
%token<string> LOWERCASE_ID
%token<string> UPPERCASE_ID
%token<string> TYPE_VARIABLE
%token<Int64.t> INT
%token<char>   CHAR
%token<string> STRING

%token         TYPE
%token         LET
%token         FUN
%token         EXTERN
%token         BACKSLASH
%token         AND
%token         AMPERSAND

%token         UNDERSCORE

%token         IF
%token         ELSE
%token         WHILE
%token         DO
%token         FOR
%token         IN
%token         TO
%token         SWITCH

%token         REF

%token         SEMICOLON

%token         COLONEQUAL
%token         DOUBLEAMPERSAND
%token         PIPEPIPE
%token         EQUALQUESTION
%token         LANGLEEQUALQUESTION
%token         RANGLEEQUALQUESTION
%token         LANGLEQUESTION
%token         RANGLEQUESTION
%token         DOT
%token         EXCLAMATION
%token         PIPE
%token         COLON
%token         EQUAL
%token         PLUS
%token         MINUS
%token         STAR
%token         SLASH
%token         ARROW
%token         COMMA

%token         LANGLE
%token         RANGLE
%token         LBRACK
%token         RBRACK
%token         LPAR
%token         RPAR
%token         LCBRACK
%token         RCBRACK

%token         EOF

%start<AST.t> main
%%

main:
| definitions = list(located(definition)) EOF { definitions }
| e=located(error)                            { Error.error "parsing" e.position "Syntax error." }

definition:
| val_def = value_definition          { val_def              }
| type_def = define_type              { type_def             }
| EXTERN id=located(identifier) COLON
         s=located(type_scheme)       { DeclareExtern(id, s) }

identifier:
| id=LOWERCASE_ID { Id(id) }

%inline located(X):
  x=X { Position.with_poss $startpos $endpos x }

separated_twolong_list(SEP, ELE):
 e1=ELE SEP l=separated_nonempty_list(SEP, ELE) { e1 :: l }

twolong_list(ELE):
 e1=ELE l=nonempty_list(ELE) { e1 :: l }

value_definition:
| v = vdefinition { DefineValue v }

vdefinition:
| LET LPAR  id=located(identifier)
        s=preceded(COLON, located(type_scheme))
      RPAR
  EQUAL e=located(expr)                                 { SimpleValue(id, Some s, e) }
| LET   id=located(identifier)
  EQUAL e=located(expr)                                 { SimpleValue(id, None, e) }
| d = function_definitions                              { d                     }

simple_vdefinition:
| LET LPAR  id=located(identifier)
        s=preceded(COLON, located(type_scheme))
      RPAR
  EQUAL e=located(nodef_expr)                                 { SimpleValue(id, Some s, e) }
| LET   id=located(identifier)
  EQUAL e=located(nodef_expr)                                 { SimpleValue(id, None, e) }
| d = function_definitions                              { d                     }

function_definitions:
| FUN l=separated_nonempty_list(AND, function_definition) { RecFunctions(l) }

function_definition:
| s=option(preceded(COLON, located(type_scheme)))
  id=located(identifier)
  arg = located(pattern) EQUAL
  e=located(expr)                                 { (id, s, FunctionDefinition(arg, e)) }
(*
simple_function_definitions:
| FUN l=separated_nonempty_list(AND, simple_function_definition) { RecFunctions(l) }
simple_function_definition:
| s=option(preceded(COLON, located(type_scheme)))
  id=located(identifier)
  arg=located(pattern) EQUAL
  e=located(nodef_expr)                           { (id, s, FunctionDefinition(arg, e)) }*)


%public expr:
| e=nodef_expr                                      { e                   }
| LPAR e1=located(nodef_expr) SEMICOLON e2=located(expr) RPAR { Sequence ([e1; e2]) }
| LPAR d=simple_vdefinition SEMICOLON e=located(expr) RPAR    { Define(d, e)        }

nodef_expr:
| e=binop_prio_0_expr                      { e                                    }
| e=control_structure                      { e                                    }
| LPAR BACKSLASH arg=located(pattern) ARROW
            body=located(nodef_expr) RPAR      { Fun(FunctionDefinition(arg, body))   }
| LPAR e1=located(binop_prio_0_expr) COLONEQUAL
  e2=located(binop_prio_0_expr) RPAR            { Assign(e1, e2)                       }

binop_prio_0_expr:
| e = binop_prio_1_expr                                   { e }
| e = binop(binop_prio_0_expr, prio_0, binop_prio_0_expr) { e }

binop_prio_1_expr:
| e = binop_prio_2_expr                                   { e }
| e = binop(binop_prio_1_expr, prio_1, binop_prio_1_expr) { e }

binop_prio_2_expr:
| e = apply_expr                                          { e }
| e = binop(binop_prio_2_expr, prio_2, binop_prio_2_expr) { e }

apply_expr:
| e = ref_deref_expr                      { e                               }
| l=twolong_list(located(ref_deref_expr)) { ASTHelper.expr_of_apply_list l  }

ref_deref_expr:
| REF e=located(atomic_expr)            { Ref e                                 }
| EXCLAMATION e=located(ref_deref_expr) { Read e                                }
| e = atomic_expr                       { e                                     }


atomic_expr:
| r = record                            { r                    }
| t = tuple                             { t                    }
| LPAR e=located(atomic_expr) DOT
  l=located(label) RPAR                      { Field(e, l)          }
| LPAR e=expr RPAR                      { e                    }
| LPAR e=located(expr)
       COLON
       t=located(type_)
  RPAR                                  { TypeAnnotation(e, t) }
| v=variable                            { v                    }
| l=located(literal)                    { Literal l            }
| LPAR const=located(constructor)
  type_args=option(type_argument_apply)
  args=ioption(constructor_arguments) RPAR   { Tagged(const, type_args, list_of_list_option args) }

%inline constructor_arguments:
| LPAR l=separated_nonempty_list(COMMA, located(binop_prio_0_expr)) RPAR { l }

%inline tuple:
| LPAR  l = separated_twolong_list(COMMA, located(binop_prio_0_expr)) RPAR { Tuple(l) }

%inline variable:
| id = located(identifier)
  types=option(type_argument_apply) { Variable (id, types) }

%inline record:
| LCBRACK
    l=separated_nonempty_list(COMMA, record_expr_member)
  RCBRACK
  types=option(type_argument_apply)                      { Record(l, types) }


%inline binop(E1, OP, E2):
| LPAR e1 = located(E1) b = located(OP) e2 = located(E2) RPAR { Apply(
                                                        { value=Apply(b, e1); position=join e1.position b.position }, e2) }
%inline prio_2:
(* int -> int -> int *)
| p = located(STAR)  { Variable(with_val (binop_name STAR) p, None)  }
| p = located(SLASH) { Variable(with_val (binop_name SLASH) p, None) }

%inline prio_1:
(* int -> int -> int *)
| p = located(MINUS) { Variable(with_val (binop_name MINUS) p, None) }
| p = located(PLUS)  { Variable(with_val (binop_name PLUS) p, None)  }
(* int -> int -> bool *)
| p = located(EQUALQUESTION)       { Variable(with_val (binop_name EQUALQUESTION) p, None)       }
| p = located(LANGLEEQUALQUESTION) { Variable(with_val (binop_name LANGLEEQUALQUESTION) p, None) }
| p = located(RANGLEEQUALQUESTION) { Variable(with_val (binop_name RANGLEEQUALQUESTION) p, None) }
| p = located(LANGLEQUESTION)      { Variable(with_val (binop_name LANGLEQUESTION) p, None)      }
| p = located(RANGLEQUESTION)      { Variable(with_val (binop_name RANGLEQUESTION) p, None)      }

%inline prio_0:
(* bool -> bool -> bool *)
| p = located(DOUBLEAMPERSAND)   { Variable(with_val (binop_name DOUBLEAMPERSAND) p, None)   }
| p = located(PIPEPIPE)          { Variable(with_val (binop_name PIPEPIPE) p, None)          }

%inline literal:
| c = CHAR   { LChar c   }
| s = STRING { LString s }
| i = INT    { LInt i    }


record_expr_member:
| label=located(label) EQUAL e=located(nodef_expr) { (label, e) }

pattern:
| p=atomic_pattern                                                      { p                     }
| LPAR p=located(atomic_pattern) COLON t=located(type_) RPAR                      { PTypeAnnotation(p, t) }
| LPAR  l=separated_twolong_list(COMMA, located(pattern)) RPAR          { PTuple(l)             }
| LPAR branches = separated_twolong_list(PIPE, located(atomic_pattern)) RPAR      { POr(branches)         }
| LPAR branches = separated_twolong_list(AMPERSAND, located(atomic_pattern)) RPAR { PAnd(branches)        }

record_pattern:
| l=located(label) EQUAL p=located(pattern) { (l, p) }

atomic_pattern:
(* Not in the grammar as is. Permited by the tuple rule. *)
| LPAR p = pattern RPAR    { p             }
| c=located(constructor)
  targs=option(type_argument_apply)
  l=option(
      delimited(
        LPAR,
        separated_nonempty_list(COMMA, located(pattern)),
        RPAR))                                            { PTaggedValue(c, targs, list_of_list_option l) }
| UNDERSCORE                                              { PWildcard                                     }
| id = located(identifier)                                { PVariable id                                  }
| lit = located(literal)                                  { PLiteral lit                                  }
| LCBRACK
    l=separated_nonempty_list(COMMA, record_pattern)
  RCBRACK
  t = option(type_argument_apply)                         { PRecord(l, t)                                 }




control_structure:
| IF LPAR cond=located(expr) RPAR
  LCBRACK body1=located(expr) RCBRACK
  ELSE
  LCBRACK body2=located(expr) RCBRACK    { IfThenElse(cond, body1, body2) }
| FOR     id=located(identifier) IN
  LPAR    e1=located(expr)
    TO    e2=located(expr)
  RPAR
  LCBRACK body=located(expr) RCBRACK     { For(id, e1, e2, body)                 }

| WHILE LPAR cond=located(expr) RPAR
  LCBRACK    body=located(expr) RCBRACK  { While(cond, body)                     }
| DO LCBRACK body=located(expr) RCBRACK
  WHILE LPAR cond=located(expr) RPAR     { Sequence([ body
                                                    ; { value=While(cond, body)
                                                      ; position=body.position }
                                                    ])                           }
| SWITCH LPAR cond=located(expr) RPAR
  LCBRACK     cases=switch_cases RCBRACK { Case(cond, cases)                     }

switch_cases:
| option(PIPE) cases=separated_nonempty_list(PIPE, located(switch_branch)) { cases }

switch_branch:
| p=located(pattern) ARROW e=located(expr) { Branch(p, e) }


%public type_argument_apply:
(* according to spec, this should be
| LANGLE args = separated_nonempty_list(COMMA, located(type_)) RANGLE { args }
Is not because of test 47-instanciation.bad *)
| LANGLE args = separated_list(COMMA, located(type_)) RANGLE { args }

%public type_scheme:
| l = option(delimited(LBRACK, nonempty_list(located(type_variable)), RBRACK))
  t = located(type_)                                                           { ForallTy(list_of_list_option l, t) }

%public %inline constructor:
| id = UPPERCASE_ID { KId id }

%public define_type:
| TYPE id=located(type_constructor)
  args=option(type_argument_declaration) EQUAL
  t=type_definition                            { DefineType(id, list_of_list_option args, t) }

%public label:
| id=LOWERCASE_ID { LId id }

%public type_:
| t = simple_type     { t               }
| LPAR t1 = located(type_)
  ARROW
  t2 = located(type_) RPAR { TyArrow(t1, t2) }

simple_type:
| t = very_simple_type                                        { t          }
| LPAR l = separated_twolong_list(STAR, located(very_simple_type)) RPAR { TyTuple(l) }

very_simple_type:
| LPAR t = type_ RPAR              { t                                    }
| var = type_variable              { TyVar var                            }
| con = type_constructor
  args=option(type_argument_apply) { TyCon(con, list_of_list_option args) }

type_argument_declaration:
| LANGLE args = separated_nonempty_list(COMMA, located(type_variable)) RANGLE { args }

type_definition:
| option(PIPE) l=separated_nonempty_list(PIPE, sum_type_constructor_definition) { DefineSumType(l)   }
| LCBRACK l=separated_nonempty_list(COMMA, record_def) RCBRACK                  { DefineRecordType l }

record_def:
| label=located(label) COLON
  ty=located(type_)          { (label, ty) }

sum_type_constructor_definition:
| c=located(constructor)
  args=option(sum_type_constructor_arg_list) { (c, list_of_list_option args) }

sum_type_constructor_arg_list:
| LPAR l = separated_nonempty_list(COMMA, located(type_)) RPAR { l }

type_constructor:
| id=LOWERCASE_ID { TCon(id) }

type_variable:
| v = TYPE_VARIABLE { TId v }
