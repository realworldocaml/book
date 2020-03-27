%{
    let return_if_strict strict value =
      if strict then
        raise Parsing.Parse_error
      else
        value

%}

%parameter<Param : sig
  val strict: bool
end>

%%

%public legacyArray :
  | nonAnyType LBRACKET RBRACKET null 
  { 
    return_if_strict Param.strict
      (to_non_any $4 (`FrozenArray ([], ($1 :> type_)))) 
  }

%public promiseOnly :
  | PROMISE { return_if_strict Param.strict (`Promise `Any) }

%public promiseNull :
  | PROMISE LT returnType GT QUESTION { return_if_strict Param.strict $3 }

%public attributeSerializetion :
  | ATTRIBUTE  { return_if_strict Param.strict (`Identifiers [attribute]) }

%public commnaEnd :
  | COMMA { return_if_strict Param.strict [] } 

%public emptyExtendedAttributeList :
  | LBRACKET RBRACKET { return_if_strict Param.strict [] }