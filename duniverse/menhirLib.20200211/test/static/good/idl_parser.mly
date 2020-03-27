%{

open Full_idl_syntax

let find_attr attr attrs =
  let at = AttrNoArgs (Id.id_of_string attr) in
  List.exists (fun a -> a == at) attrs
let ifaceMemWithAttrs mem attrs = match mem with
  | Attribute(p, _, ro, s, t, id) -> Attribute(p, attrs, ro, s, t, id)
  | Operation(p, _, s, q, ret, id, args) ->
    let q' = if (find_attr "getter" attrs) then {q with getter = true} else q in
    let q' = if (find_attr "setter" attrs) then {q' with setter = true} else q' in
    Operation(p, attrs, s, q', ret, id, args)
  | ConstMember(p, _, t, id, value) -> ConstMember(p, attrs, t, id, value)
  | Stringifier(p, _) -> Stringifier(p, attrs)
let defWithAttrs def attrs = match def with
  | Module(p, _, id, defs) -> Module(p, attrs, id, defs)
  | Typedef(p, _, typ, id) -> Typedef(p, attrs, typ, id)
  | Interface(p, _, id, parent, mems, callback) -> Interface(p, attrs, id, parent, mems, callback)
  | ForwardInterface(p, _, id) -> ForwardInterface(p, attrs, id)
  | Exception(p, _, id, parent, mems) -> Exception(p, attrs, id, parent, mems)
  | Implements(p, _, id, impl) -> Implements(p, attrs, id, impl)
  | Const(p, _, typ, id, value) -> Const(p, attrs, typ, id, value)
  | Dictionary(p, _, id, parent, mems) -> Dictionary(p, attrs, id, parent, mems)
  | PartialInterface(p, _, id, mems) -> PartialInterface(p, attrs, id, mems)
  | Include(p, _, file) -> Include(p, attrs, file)
  | Callback(p, _, id, args, ret) -> Callback(p, attrs, id, args, ret)
  | Enum(p, _, id, items) -> Enum(p, attrs, id, items)
let noQualifiers = {static=false;getter=false;setter=false;
                    creator=false;deleter=false;legacyCaller=false}
%}

%token <Id.t> ID
%token <string> STRING
%token <string> UUID
%token <string * string> NATIVE
%token <int64> INTLIT
%token <float> FLOATLIT
%token MODULE INTERFACE LBRACE RBRACE SEMI SHORT LONG BOOLEAN
       BYTE OCTET FLOAT DOUBLE ANY QUES LRBRACK
       UNSIGNED READONLY VOID COMMA IN OUT INOUT OPTIONAL LPAREN RPAREN EOF
       ATTRIBUTE LBRACK RBRACK TYPEDEF EXCEPTION CONST EQUALS RAISES
       COLON COLONCOLON DOTDOTDOT LEGACYCALLER GETTER SETTER CREATOR DELETER
       NOINTERFACEOBJECT OVERRIDEBUILTINS PUTFORWARDS IMPLEMENTS
       STRINGIFIER NAMEDCONSTRUCTOR CONSTRUCTOR REPLACEABLENAMEDPROPERTIES
       UNFORGEABLE REPLACEABLE DICTIONARY CALLBACK TREATNULLAS FUNCTIONONLY
       ALLOWANY PARTIAL SEQUENCE LANGLE RANGLE CLAMP NOSCRIPT OPTIONAL_ARGC NOTXPCOM RETVAL
       INCLUDE SCRIPTABLE IMPLICIT_JSCONTEXT INHERIT STATIC ENUM SIZE_IS
       BAR XOR AND SHLEFT SHRIGHT PLUS MINUS TILDE TIMES DIVIDE MOD TRUE FALSE
       PRIVATEBROWSINGCHECK UNSAFE QUERYINTERFACETYPE QUERYELEMENTATTYPE
       (* PRUint32 PRInt32 PRUint16 PRInt16 PRUnichar *)

%type <Full_idl_syntax.definition list> idlFile

%left BAR
%left XOR
%left AND
%nonassoc SHLEFT SHRIGHT
%left PLUS MINUS
%left TIMES DIVIDE MOD


%start idlFile

%%

%inline iboption(X):
  /* nothing */
    { false }
| X
    { true }


ilist(X):
  /* nothing */
    { [] }
| x = X; xs = ilist(X)
    { x :: xs }

inonempty_list(X):
  x = X
    { [ x ] }
| x = X; xs = inonempty_list(X)
    { x :: xs }

iseparated_nonempty_list(separator, X):
  x = X
    { [ x ] }
| x = X; separator; xs = iseparated_nonempty_list(separator, X)
    { x :: xs }


extAttrIfaceMemList(X) :
  | attrs=extendedAttributeList x=X { ifaceMemWithAttrs x attrs }
extAttrExnMemList(X) :
  | attrs=extendedAttributeList x=X { ifaceMemWithAttrs x attrs }
extAttrArgList(X) :
  | attrs=extendedAttributeList x=X { (attrs, x) }
extAttrDefList(X) :
  | attrs=extendedAttributeList x=X { defWithAttrs x attrs }
extAttrDictList(X) :
  | attrs=extendedAttributeList x=X { ifaceMemWithAttrs x attrs }

definitions:
  | defs=ilist(extAttrDefList(definition)) { defs }

definition:
  | callbackOrInterface { $1 }
  | partialInterface { $1 }
  | dictionary { $1 }
  | exceptionDef { $1 }
  | enum { $1 }
  | typedef { $1 }
  | implementsStatement { $1 }
  (* NONSTANDARD DEFINITONS *)
  | INCLUDE STRING { Include($startpos, [], $2) }
  | MODULE name=ID defs=delimited(LBRACE, ilist(definition), RBRACE) SEMI
    { Module ($startpos, [], name, defs) }
  | INTERFACE ID SEMI { ForwardInterface ($startpos, [], $2) }
  | c=const { let (p, m, t, id, e) = c in Const(p,m,t,id,e) }

%inline identOrKeyword:
  | CONSTRUCTOR { Id.id_of_string "Constructor" }
  | NOTXPCOM { Id.id_of_string "notxpcom" }
  | RETVAL { Id.id_of_string "retval" }
  | UNSAFE { Id.id_of_string "unsafe" }
  | PRIVATEBROWSINGCHECK { Id.id_of_string "PrivateBrowsingCheck" }
  | QUERYINTERFACETYPE { Id.id_of_string "QueryInterfaceType" }
  | id=identOrKeywordNotConstructor { id }

%inline identOrKeywordNotSpecial:
  | CONSTRUCTOR { Id.id_of_string "Constructor" }
  | NOTXPCOM { Id.id_of_string "notxpcom" }
  | RETVAL { Id.id_of_string "retval" }
  | UNSAFE { Id.id_of_string "unsafe" }
  | PRIVATEBROWSINGCHECK { Id.id_of_string "PrivateBrowsingCheck" }
  | QUERYINTERFACETYPE { Id.id_of_string "QueryInterfaceType" }
  | id=identOrKeywordNotConstructorOrSpecial { id }

%inline identOrKeywordNotConstructor:
  | id=specialsAsKeyword { id }
  | id=identOrKeywordNotConstructorOrSpecial { id }

%inline identOrKeywordNotConstructorOrSpecial:
  | id=ID { id }
  (* | OBJECT { Printf.printf "KW(object)\n"; Id.id_of_string "object" } *)
  | DICTIONARY { Id.id_of_string "dictionary" }
  | PARTIAL { Id.id_of_string "partial" }

%inline specialsAsKeyword:
  | CREATOR { Id.id_of_string "creator" }
  | GETTER { Id.id_of_string "getter" }
  | SETTER { Id.id_of_string "setter" }

callbackOrInterface:
  | CALLBACK callbackRestOrInterface { $2 }
  | interface { $1 }

callbackRestOrInterface:
  | callbackRest { $1 }
  | interface {
    match $1 with
    | Interface(_, id, nameOpt, members, metas, _) ->
      Interface($startpos, id, nameOpt, members, metas, IsCallbackInterface)
    | _ -> failwith "Impossible"
  }

callbackRest:
  | name=identOrKeyword EQUALS ret=returnType args=delimited(LPAREN, argumentList, RPAREN) SEMI
      { Callback($startpos, [], name, args, ret) }

interface:
  | INTERFACE name=ID inherits=inheritance mems=delimited(LBRACE, interfaceMembers, RBRACE) SEMI
      { Interface($startpos, [], name, inherits, mems, IsNormalInterface) }

partialInterface:
  | PARTIAL INTERFACE name=ID mems=delimited(LBRACE, interfaceMembers, RBRACE) SEMI
      { PartialInterface($startpos, [], name, mems) }

interfaceMembers:
  | mems=ilist(extAttrIfaceMemList(interfaceMember)) { mems }

interfaceMember:
  | c=const { let (p,m,t,id,c) = c in ConstMember(p,m,t,id,c) }
  | attributeOrOperation { $1 }

dictionary:
  | DICTIONARY name=ID inherits=inheritance defs=delimited(LBRACE, dictionaryMembers, RBRACE) SEMI
      { Dictionary($startpos, [], name, inherits, defs) }

dictionaryMembers:
  | mems=ilist(extAttrDictList(dictionaryMember)) { mems }

dictionaryMember:
  | ty=typeDecl name=identOrKeyword ioption(default) SEMI
      { Attribute($startpos, [], NoReadOnly, IsNormal, ty, name) } (* default? *)

default:
  | EQUALS constValue { $2 }

(* defaultValue: *)
(*   | constValue { $1 } *)
(*   | STRING { String $1 } *)

exceptionDef:
  | EXCEPTION name=ID inherits=inheritance mems=delimited(LBRACE, exceptionMembers, RBRACE) SEMI
      { Exception($startpos, [], name, inherits, mems) }

exceptionMembers:
  | mems=ilist(extAttrExnMemList(exceptionMember)) { mems }

inheritance:
  | COLON scopedName { Some ($2) }
  | { None }

scopedName:
  | n=absoluteScopedName { n }
  | n=relativeScopedName { n }
absoluteScopedName:
  | COLONCOLON id=identOrKeyword parts=scopedNameParts { AbsoluteName (id::parts) }
relativeScopedName:
  | id=identOrKeyword parts=scopedNameParts { RelativeName (id::parts) }
scopedNameParts:
  | COLONCOLON id=identOrKeyword parts=scopedNameParts { id::parts }
  | { [] }

enum:
  | ENUM name=ID mems=delimited(LBRACE, iseparated_nonempty_list(COMMA, STRING), RBRACE) SEMI
      { Enum($startpos, [], name, mems) }

typedef:
  | TYPEDEF attrs=extendedAttributeList ty=typeDecl id=ID SEMI
      { Typedef ($startpos, [], ty, id) } (* extendedAttributeList? *)
  | nat=NATIVE SEMI
      { let (id, natid) = nat in Typedef ($startpos, [], Native natid, Id.id_of_string id) }

implementsStatement:
  | ID IMPLEMENTS ID SEMI
      { Implements($startpos, [], $1, $3) }

const:
  | CONST cty=constType id=ID EQUALS cval=constValue SEMI
      { ($startpos, [], cty, id, cval) }

constValue:
  | expr { $1 }
  (* | booleanLiteral { () } *)
  (* | INT { () } *)
  (* | FLOAT { () } *)
  (* | NULL { () } *)

expr:
  | exprUn { $1 }
  | expr MOD expr { BinOp($1, Mod, $3) }
  | expr DIVIDE expr { BinOp($1, Divide, $3) }
  | expr TIMES expr { BinOp($1, Times, $3) }
  | expr MINUS expr { BinOp($1, Minus, $3) }
  | expr PLUS expr { BinOp($1, Plus, $3) }
  | expr SHRIGHT expr { BinOp($1, ShRight, $3) }
  | expr SHLEFT expr { BinOp($1, ShLeft, $3) }
  | expr AND expr { BinOp($1, And, $3) }
  | expr XOR expr { BinOp($1, Xor, $3) }
  | expr BAR expr { BinOp($1, Or, $3) }
exprUn:
  | atom=exprAtom { atom }
  | TILDE atom=exprAtom { UnOp(UTilde, atom) }
  | PLUS atom=exprAtom { UnOp(UPlus, atom) }
  | MINUS atom=exprAtom { UnOp(UMinus, atom) }
%inline exprAtom:
  | name=scopedName { Ident name }
  | lit=literal { lit }
  | exp=delimited(LPAREN, expr, RPAREN) { exp }

%inline literal:
  | lit=INTLIT { IntLit lit }
  | lit=FLOATLIT { FloatLit lit }
  | lit=STRING { String lit }
  | TRUE { Bool true }
  | FALSE { Bool false }

attributeOrOperation:
  | STRINGIFIER stringifierAttributeOrOperation { $2 }
  | attribute { $1 }
  | operation { $1 }

stringifierAttributeOrOperation:
  | attribute {
    match $1 with
    | Attribute(_, meta, readonly, _, ty, id) -> Attribute($startpos, meta, readonly, IsStringifier, ty, id)
    | _ -> failwith "Impossible"
  }
  | operation {
    match $1 with
    | Operation(_, meta, _, quals, ret, id, args) -> Operation($startpos, meta, IsStringifier, quals, ret, id, args)
    | _ -> failwith "Impossible"
  }
  | SEMI { Stringifier($startpos, []) }

attribute:
  | INHERIT? readonly=iboption(READONLY) ATTRIBUTE ty=typeDecl name=identOrKeyword SEMI
      { Attribute($startpos, [], (if readonly then ReadOnly else NoReadOnly), IsNormal, ty, name) }

operation:
  | q=qualifiers op=operationRest { op $startpos q }

raisesClause:
  | RAISES delimited(LPAREN, separated_list(COMMA, ID), RPAREN) { () }

%inline qualifiers: (* NOTE: Experimentally checked for these combinations; others are possible *)
  | STATIC { {noQualifiers with static=true} }
  | legacy=iboption(LEGACYCALLER) GETTER { {noQualifiers with legacyCaller=legacy; getter=true} }
  | SETTER creator=iboption(CREATOR) { {noQualifiers with setter=true; creator=creator} }
  | CREATOR { {noQualifiers with creator=true} }
  | DELETER { {noQualifiers with deleter=true} }
  | { noQualifiers }
(*   | specials { $1 } *)

(* specials: *)
(*   | GETTER specials { {$2 with getter = true} } *)
(*   | SETTER specials { {$2 with setter = true} } *)
(*   | CREATOR specials { {$2 with creator = true} } *)
(*   | DELETER specials { {$2 with deleter = true} } *)
(*   | LEGACYCALLER specials { {$2 with legacyCaller = true} } *)
(*   | { noQualifiers } *)

operationRest:
  | ret=returnType name=identOrKeyword? args=delimited(LPAREN, argumentList, RPAREN) raisesClause? SEMI
      { (fun start quals -> Operation(start, [], IsNormal, quals, ret, name, args)) }

argumentList:
 | args=separated_list(COMMA, argument) { args }

argument:
  | arg=extAttrArgList(optionalOrRequiredArgument) {
    let (m, (io, opt, ty, dots, id, def)) = arg in
    if opt then (io, Optional::m, ty, dots, id, def)
    else (io, m, ty, dots, id, def) }

optionalOrRequiredArgument:
  | io=inout OPTIONAL ty=typeDecl id=identOrKeyword def=default? { (io, true, ty, Single, id, def) }
  | io=inout ty=typeDecl dots=dots id=identOrKeyword { (io, false, ty, dots, id, None) }

%inline dots:
  | DOTDOTDOT { Variadic }
  | { Single }

inout:
  | IN { InParam }
  | OUT { OutParam }
  | INOUT { InOutParam }
  | { InParam }

exceptionMember:
  | c=const { let (p,m,t,id,e) = c in ConstMember(p,m,t,id,e) }
  | exceptionField { $1 }

exceptionField:
  | ty=typeDecl id=ID SEMI { Attribute($startpos, [], NoReadOnly, IsNormal, ty, id) }

extendedAttributeList:
  | attrs=delimited(LBRACK, iseparated_nonempty_list(COMMA, extendedAttribute), RBRACK) { attrs }
  | { [] }

extendedAttribute:
  | extendedAttributeNoArgs { $1 }
  | extendedAttributeArgList { $1 }
  | extendedAttributeNamedArgList { $1 }
  | extendedAttributeIdent { $1 }

typeDecl:
  | singleType { $1 }
  (* | unionType typeSuffix *) (* TODO *)

singleType:
  | nonAnyType { $1 }
  | ANY typeSuffixStartingWithArray { $2 Any }

(* unionType: *)
(*   | delimited(LPAREN, unionMemberType OR iseparated_nonempty_list(OR, unionMemberType), RPAREN) *)

(* unionMemberType: *)
(*   | nonAnyType *)
(*   | unionType typeSuffix *)
(*   | ANY LRBRACK typeSuffix *)

nonAnyType:
  | primitiveType typeSuffix { ($2 $1) }
  (* | DOMSTRING typeSuffix { ($2 DOMString) } *)
  | SEQUENCE LANGLE t=typeDecl RANGLE q=iboption(QUES) { if q then Ques (Sequence t) else Sequence t }
  (* | OBJECT typeSuffix { ($2 Object) } *)
  (* | DATE typeSuffix { ($2 Date) } *)

constType:
  | ty=primitiveType readonly=iboption(QUES) { if readonly then Ques ty else ty }

primitiveType:
  | unsignedIntegerType { $1 }
  | BOOLEAN { Boolean }
  | OCTET { Octet }
  | FLOAT { Float }
  | DOUBLE { Double }
  (* | PRUnichar { Octet } *)
  | id=identOrKeywordNotSpecial {
    match (Id.string_of_id id) with
    | "PRUnichar" -> Octet
    | "PRUint32" -> Long Unsigned
    | "PRInt32" -> Long NoUnsigned
    | "PRUint16" -> Short Unsigned
    | "PRInt16" -> Short NoUnsigned
    | "DOMString" -> DOMString
    | "object" -> Object
    | "Date" -> Date
    | _ -> Name (RelativeName[id])
  }

unsignedIntegerType:
  | unsigned=iboption(UNSIGNED) ty=integerType { ty (if unsigned then Unsigned else NoUnsigned) }
  (* | PRUint32 { Long Unsigned } *)
  (* | PRInt32 { Long NoUnsigned } *)
  (* | PRUint16 { Short Unsigned } *)
  (* | PRInt16 { Short NoUnsigned } *)

integerType:
  | BYTE { (fun u -> match u with | Unsigned -> Octet | NoUnsigned -> Byte) }
  (* According to a comment in typedarray.idl, the 'unsigned byte' type doesn't exist,
     but is equivalent to octet *)
  | SHORT { (fun u -> Short u) }
  | LONG longopt=iboption(LONG) { if longopt then (fun u -> LongLong u) else (fun u -> Long u) }

typeSuffix:
  | LRBRACK typeSuffix { (fun t -> $2 (Array t)) }
  | QUES typeSuffixStartingWithArray { (fun t -> $2 (Ques t)) }
  | { (fun t -> t) }

typeSuffixStartingWithArray:
  | LRBRACK typeSuffix { (fun t -> $2 (Array t)) }
  | { (fun t -> t) }

returnType:
  | typeDecl { $1 }
  | VOID { Void }

extendedAttributeNoArgs:
  | CONST { AttrNoArgs (Id.id_of_string "const") }
  | NOINTERFACEOBJECT { NoInterfaceObject }
  | OVERRIDEBUILTINS { OverrideBuiltins }
  | REPLACEABLENAMEDPROPERTIES { ReplaceableNamedProperties }
  | REPLACEABLE { Replaceable }
  | UNFORGEABLE { Unforgeable }
  | CALLBACK { MCallback }
  | ALLOWANY { AllowAny }
  | CLAMP { Clamp }
  | NOSCRIPT { NoScript }
  | PRIVATEBROWSINGCHECK { PrivateBrowsingCheck }
  | QUERYINTERFACETYPE { QueryInterfaceType }
  | UNSAFE { Unsafe }
  | OPTIONAL {Optional}
  | OPTIONAL_ARGC { OptionalArgc }
  | SCRIPTABLE { Scriptable }
  | IMPLICIT_JSCONTEXT { ImplicitJSContext }
  | CONSTRUCTOR { Constructor [] }
  | NOTXPCOM { NotXPCOM }
  | RETVAL { Retval }
  | id=identOrKeywordNotConstructor { AttrNoArgs id }
(* | ID *)

extendedAttributeArgList:
  | UUID { Uuid ($1) }
  | CONSTRUCTOR args=delimited(LPAREN, argumentList, RPAREN) { Constructor args }
  | SIZE_IS arg=delimited(LPAREN, ID, RPAREN) { SizeOf arg }
  | QUERYELEMENTATTYPE arg = delimited(LPAREN, INTLIT, RPAREN) { QueryElementAtType(Int64.to_int arg) }
  | id=identOrKeywordNotConstructor args=delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { AttrArgList (id, args) }
(* | ID delimited(LPAREN, argumentList, RPAREN) *)

extendedAttributeIdent:
  | PUTFORWARDS EQUALS id=ID { PutForwards id }
  | TREATNULLAS EQUALS typeDecl { TreatNullAs $3 }
  | CALLBACK EQUALS FUNCTIONONLY { MCallbackFunctionOnly }
  | name=identOrKeyword EQUALS id=identOrKeyword { AttrNamedIdent(name, id) }
(* | ID EQUALS ID *)

extendedAttributeNamedArgList:
  | NAMEDCONSTRUCTOR EQUALS name=ID args=delimited(LPAREN, argumentList, RPAREN) { NamedConstructor(name,args) }
  | name=identOrKeyword EQUALS id=identOrKeyword args=delimited(LPAREN, separated_list(COMMA, expr), RPAREN) { AttrNamedArgList (name,id,args) }

































idlFile :
  | definitions EOF  { $1 }

(* definition : *)
(*   | INCLUDE STRING { Include($startpos, $2) } *)
(*   | MODULE ID LBRACE list(definition) RBRACE SEMI  *)
(*     { Module ($startpos, $2, $4) } *)
(*   | INTERFACE name=ID SEMI { ForwardInterface ($startpos, name) } *)
(*   | LBRACK metas=meta_comma RBRACK INTERFACE name=ID super=inheritance  *)
(*     LBRACE mems=interfaceMembers RBRACE SEMI *)
(*     { Interface ($startpos, name, super, mems, metas) } *)
(*   | PARTIAL INTERFACE x=ID LBRACE mems=interfaceMembers RBRACE SEMI *)
(*     { PartialInterface ($startpos, x, mems) } *)
(*   | INTERFACE name=ID super=inheritance  *)
(*     LBRACE mems=interfaceMembers RBRACE SEMI *)
(*     { Interface ($startpos, name, super, mems, []) } *)
(*   | TYPEDEF typeWithSuffix ID SEMI { Typedef ($startpos, $2, $3) } *)
(*   | EXCEPTION ID LBRACE exceptionMembers RBRACE SEMI *)
(*     { Exception ($startpos, $2, $4) } *)
(*   | CONST attributeType ID EQUALS constExpr SEMI *)
(*     { Const ($startpos, $2, $3) } *)
(*   | name=ID IMPLEMENTS super=ID  SEMI *)
(*     { Implements ($startpos, name, super) } *)
(*   | DICTIONARY name=ID inheritance LBRACE  *)
(*       members=list(dictionary_member)  *)
(*     RBRACE SEMI *)
(*     { Dictionary ($startpos, name, members) } *)

(* constExpr : *)
(*   | INT { () } *)

(* inheritance : *)
(*   | { None } *)
(*   | COLON scopedName { Some $2 } *)

(* scopedName : *)
(*   | ID { RelativeName [$1] } *)

(* interfaceMembers : *)
(*   | mems=list(interfaceMember) { mems } *)

(* exceptionMembers : *)
(*   | exns=list(exceptionMember) { exns } *)

(* dictionary_member : *)
(*   | t=attributeType x=ID SEMI { Attribute ($startpos, NoReadOnly, t, x) } *)

(* exceptionMember : *)
(*   | typ=attributeType name=ID SEMI *)
(*     { Attribute ($startpos, NoReadOnly, typ, name) } *)
(*   | CONST typ=attributeType name=ID EQUALS constExpr SEMI *)
(*     { ConstMember ($startpos, typ, name) } *)

(* interfaceMember : *)
(*   | metas STRINGIFIER? readOnly=readOnly ATTRIBUTE typ=attributeType name=ID SEMI *)
(*     { Attribute ($startpos, readOnly, typ, name) } *)
(*   | metas LEGACYCALLER? ret=returnType name=ID LPAREN args=argumentList RPAREN  *)
(*     raisesClause SEMI *)
(*     { Operation ($startpos, ret, name, args) } *)
(*   | metas LEGACYCALLER? SETTER ret=returnType name=ID LPAREN args=argumentList RPAREN  *)
(*     raisesClause SEMI *)
(*     { Operation ($startpos, ret, name, args) } *)
(*   | LEGACYCALLER? GETTER ret=returnType name=ID LPAREN args=argumentList RPAREN  *)
(*     raisesClause SEMI *)
(*     { Operation ($startpos, ret, name, args) } *)
(*   | LEGACYCALLER? CREATOR ret=returnType LPAREN args=argumentList RPAREN SEMI *)
(*     { Creator ($startpos, ret, args) } *)
(*   | LEGACYCALLER? GETTER CREATOR ret=returnType LPAREN args=argumentList RPAREN SEMI *)
(*     { Creator ($startpos, ret, args) } *)
(*   | LEGACYCALLER? SETTER CREATOR ret=returnType LPAREN args=argumentList RPAREN SEMI *)
(*     { Creator ($startpos, ret, args) } *)
(*   | CONST attributeType ID EQUALS constExpr SEMI *)
(*     { ConstMember ($startpos, $2, $3) } *)
(*   | LEGACYCALLER? GETTER ret=returnType LPAREN args=argumentList RPAREN SEMI *)
(*     { Getter ($startpos, ret, args) } *)
(*   | LEGACYCALLER? SETTER ret=returnType LPAREN args=argumentList RPAREN SEMI *)
(*     { Setter ($startpos, ret, args) } *)
(*   | LEGACYCALLER? DELETER ret=returnType LPAREN args=argumentList RPAREN SEMI *)
(*     { Deletor ($startpos, ret, args) } *)

(* ids: *)
(*   | ids=separated_list(COMMA, ID) { ids } *)

(* raisesClause : *)
(*   | { () } *)
(*   | RAISES LPAREN ids RPAREN { () } *)

(* primitiveOrStringType : *)
(*   | unsigned SHORT { Short $1 } *)
(*   | unsigned LONG { Long $1 } *)
(*   | unsigned LONGLONG { LongLong $1 } *)
(*   | BOOLEAN { Boolean } *)
(*   | unsigned BYTE { Byte } *)
(*   | OCTET { Octet } *)
(*   | FLOAT { Float } *)
(*   | DOUBLE { Double } *)
(*   | DOMSTRING { DOMString } *)

(* attributeType : *)
(*   | typeWithSuffix { $1 } *)
(*   | typeWithSuffix DOTDOTDOT { $1 } *)

(* typeWithSuffix : *)
(*   | primitiveOrStringType { $1 } *)
(*   | typeWithSuffix QUES { Ques $1 } *)
(*   | typeWithSuffix LRBRACK { Array $1 } *)
(*   | ANY { Any } *)
(*   | OBJECT { Object } *)
(*   | DATE { Date } *)
(*   | scopedName { Name $1 } *)
(*   | SEQUENCE LANGLE t=typeWithSuffix RANGLE { Sequence t } *)

(* unsigned : *)
(*   | { NoUnsigned } *)
(*   | UNSIGNED { Unsigned } *)


(* readOnly : *)
(*   | READONLY { ReadOnly } *)
(*   | { NoReadOnly } *)


(* returnType : *)
(*   | attributeType { $1 } *)
(*   | VOID { Void } *)

(* argumentList :  *)
(*   | args=separated_list(COMMA, argument) { args } *)

(* argument : *)
(*   | metas IN? OPTIONAL? t=attributeType ID { t } *)

(* meta : *)
(*   | NOINTERFACEOBJECT { NoInterfaceObject } *)
(*   | OVERRIDEBUILTINS { OverrideBuiltins } *)
(*   | PUTFORWARDS EQUALS ID { PutForwards $3 } *)
(*   | NAMEDCONSTRUCTOR EQUALS constr=ID  *)
(*       args=delimited(LPAREN, argumentList, RPAREN)  *)
(*     { NamedConstructor(constr, args) } *)
(*   | CONSTRUCTOR { Constructor ([]) } *)
(*   | CONSTRUCTOR  *)
(*       args=delimited(LPAREN, argumentList, RPAREN)  *)
(*     { Constructor args } *)
(*   | REPLACEABLENAMEDPROPERTIES { ReplaceableNamedProperties }  *)
(*   | REPLACEABLE { Replaceable } *)
(*   | UNFORGEABLE { Unforgeable } *)
(*   | CALLBACK { Callback } *)
(*   | CALLBACK EQUALS FUNCTIONONLY { CallbackFunctionOnly } *)
(*   | TREATNULLAS EQUALS t=attributeType { TreatNullAs t } *)
(*   | ALLOWANY { AllowAny } *)
(*   | CLAMP { Clamp } *)
(*   | SCRIPTABLE { Scriptable } *)
(*   | UUID { Uuid $1 } *)
(*   | IMPLICIT_JSCONTEXT { ImplicitJSContext } *)

(* meta_comma : *)
(*   | metas=separated_list(COMMA, meta) { metas } *)

(* metas : *)
(*   | { () } *)
(*   | LBRACK meta_comma RBRACK  { () } *)

%%
