/*
  Standard ASN.1 grammar

  X.680: Specification of basic notation
  X.681: Information object specification
  X.682: Constraint specification
  X.683: Parameterization of ASN.1 specification
*/

%{
%}

/* TOKENS */

/*
hyphen = [\-]
digit = [0-9]
uppercase = [A-Z]
lowercase = [a-z]
letter = uppercase | lowercase
*/

/*
Word is defined in X.681, 7.9 and 10.6:

  uppercase ([hyphen] uppercase)*

is stricly included in objectclassreference

%token <string> Word
*/

/*
UpRef is defined in X.681, 7.1 (objectclassreference):

  uppercase ([hyphen] (uppercase | digit))*

is strictly included in typereference

%token <string> UpRef
*/

/*
Upper can denote either
  1) typereference (X.680, 11.2):

       uppercase ([hyphen] (letter | digit))*

  2) objectsetreference (X.681, 7.3):
       typereference

  3) modulereference (X.680, 11.5):
       typereference

but not Word or UpRef (see above).
*/

%token <string> Upper

/*
Lower can denote either
  1) valuereference (x.680, 11.4):

       lowercase ([hyphen] (letter | digit))*

  2) identifier (X.680, 11.3)

  3) objectreference (X.681, 7.2):
       valuereference
*/

%token <string> Lower

/*
UpperField can denote either
  1) typefieldreference (X.681, 7.4):
       &typereference

  2) valuesetfieldreference (X.681, 7.6):
       &typereference

  3) objectsetfieldreference (X.681, 7.8):
       &objectsetreference
*/

%token <string> UpperField

/*
LowerField can denote either
  1) valuefieldreference (X.681, 7.5):
       &valuereference

  2) objectfieldreference (X.681, 7.7):
       &objectreference
*/

%token <string> LowerField

/* Literals */

%token <string> Number
%token <string> RealNumber  /* Overlaps in 0 with Number */
/* %token Zero */

%token <string> Bstring
%token <string> Hstring
%token <string> Cstring

/* Symbols */

%token Assignment           /* ::= */
%token LeftDoubleBracket    /* [[  */
%token RightDoubleBracket   /* ]]  */
%token DoubleDot            /* ..  */
%token Ellipsis             /* ... */
%token LeftBrace            /*  {  */
%token RightBrace           /*  }  */
%token LessThan             /*  <  */
%token Comma                /*  ,  */
%token Dot                  /*  .  */
%token LeftParen            /*  (  */
%token RightParen           /*  )  */
%token LeftBracket          /*  [  */
%token RightBracket         /*  ]  */
%token Hyphen               /*  -  */
%token Colon                /*  :  */
%token SemiColon            /*  ;  */
%token At                   /*  @  */
%token AtDot                /*  @. */
%token Middle               /*  |  */
%token Bang                 /*  !  */
%token Circumflex           /*  ^  */

/* Keywords X.680 + X.681 + X.682 + X.683 */

%token ABSENT
%token ABSTRACT_SYNTAX
%token ALL
%token APPLICATION
%token AUTOMATIC
%token BEGIN
%token BIT
%token BMPString
%token BOOLEAN
%token BY
%token CHARACTER
%token CHOICE
%token CLASS
%token COMPONENT
%token COMPONENTS
%token CONSTRAINED
%token CONTAINING
%token DEFAULT
%token DEFINITIONS
%token EMBEDDED
%token ENCODED
%token END
%token ENUMERATED
%token EXCEPT
%token EXPLICIT
%token EXPORTS
%token EXTENSIBILITY
%token EXTERNAL
%token FALSE
%token FROM
%token GeneralizedTime
%token GeneralString
%token GraphicString
%token IA5String
%token IDENTIFIER
%token IMPLICIT
%token IMPLIED
%token IMPORTS
%token INCLUDES
%token INSTANCE
%token INTEGER
%token INTERSECTION
%token ISO646String
%token MAX
%token MIN
%token MINUS_INFINITY
%token NULL
%token NumericString
%token OBJECT
%token ObjectDescriptor
%token OCTET
%token OF
%token OPTIONAL
%token PATTERN
%token PDV
%token PLUS_INFINITY
%token PRESENT
%token PrintableString
%token PRIVATE
%token REAL
%token RELATIVE_OID
%token SEQUENCE
%token SET
%token SIZE
%token STRING
%token SYNTAX
%token T61String
%token TAGS
%token TeletexString
%token TRUE
%token TYPE_IDENTIFIER
%token UNION
%token UNIQUE
%token UNIVERSAL
%token UniversalString
%token UTCTime
%token UTF8String
%token VideotexString
%token VisibleString
%token WITH

%token <string> DefinedSyntax

/* Sentinel */

%token EOF

/*
The token DefinedSyntax holds the concrete syntax corresponding to an
instance of production definedSyntax, which should be handled
dynamically.


%token <string> DefinedSyntax
*/

/* Entrance points */

%start main
%type <unit> main

%%

/* RULES */

%inline empty: {}

main:
  moduleDefinition EOF
  {}

/*------------------------- X.680 -------------------------*/

moduleDefinition:
  moduleIdentifier
  DEFINITIONS
    tagDefault
    extensionDefault
    Assignment
  BEGIN
    moduleBody
  END
  {}

moduleIdentifier:
  Upper  /* modulereference */
  definitiveIdentifier
  {}

definitiveIdentifier:
  LeftBrace definitiveObjIdComponentList RightBrace
  {}
| empty
  {}

definitiveObjIdComponentList:
  definitiveObjIdComponent
  {}
| definitiveObjIdComponent definitiveObjIdComponentList
  {}

definitiveObjIdComponent:
  nameForm
  {}
| definitiveNumberForm
  {}
| definitiveNameAndNumberForm
  {}

definitiveNumberForm:
  Number
  {}

definitiveNameAndNumberForm:
  Lower  /* identifier */
  LeftParen
    definitiveNumberForm
  RightParen
  {}

tagDefault:
  EXPLICIT TAGS
  {}
| IMPLICIT TAGS
  {}
| AUTOMATIC TAGS
  {}
| empty
  {}

extensionDefault:
  EXTENSIBILITY IMPLIED
  {}
| empty
  {}

moduleBody:
  exports
  imports
  assignmentList
  {}
| empty
  {}

exports:
  EXPORTS symbolsExported SemiColon
  {}
| EXPORTS ALL SemiColon
  {}
| empty
  {}

symbolsExported:
  symbolList
  {}
| empty
  {}

imports:
  IMPORTS symbolsImported SemiColon
  {}
| empty
  {}

symbolsImported:
  symbolsFromModuleList
  {}
| empty
  {}

symbolsFromModuleList:
  symbolsFromModule
  {}
| symbolsFromModuleList symbolsFromModule
  {}

symbolsFromModule:
  symbolList FROM globalModuleReference
  {}

globalModuleReference:
  Upper /* modulereference */
  assignedIdentifier
  {}

assignedIdentifier:
  objectIdentifierValue
  {}
| definedValue
  {}
| empty
  {}

symbolList:
  symbol
  {}
| symbolList Comma symbol
  {}

symbol:
  reference
  {}
| parameterizedReference
  {}

reference:
  Upper /* typereference objectsetreference objectclassreference */
  {}
| Lower /* valuereference objectreference */
  {}

assignmentList:
  assignment
  {}
| assignmentList assignment
  {}

assignment:
  typeAssignment
  {}
| valueAssignment
  {}
| valueSetTypeAssignment
  {}
| objectClassAssignment
  {}
| objectAssignment
  {}
| objectSetAssignment
  {}
| parameterizedAssignment
  {}

definedType:
  externalTypeReference
  {}
| Upper /* typereference */
  {}
| parameterizedType
  {}
| parameterizedValueSetType
  {}

externalTypeReference:
  Upper /* modulereference */
  Dot
  Upper /* typereference */
  {}

/* The following production has been merged into the XML tokens.
nonParameterizedTypeName:
  externalTypeReference
  {}
| Upper (* typereference *)
  {}
| XMLasn1typename
  {}
*/

definedValue:
  externalValueReference
  {}
| Lower /* valuereference */
  {}
| parameterizedValue
  {}

externalValueReference:
  Upper /* modulereference */
  Dot
  Lower /* valuereference */
  {}

/* The following productions are not used formally.

absoluteValueReference:
  At Upper
  Dot itemSpec
  { (* modulereference *) }

itemSpec:
  Upper
  { (* typereference *) }
| itemId Dot componentId
  {}

itemId:
  itemSpec
  {}

componentId:
  Lower
  { (* identifier *) }
| Number
  {}
| Times
  {}
*/

typeAssignment:
  Upper /* typereference */
  Assignment
  type_x
  {}

valueAssignment:
  Lower /* valuereference */
  type_x
  Assignment
  value
  {}

valueSetTypeAssignment:
  Upper /* typereference */
  type_x
  Assignment
  valueSet
  {}

valueSet:
  LeftBrace elementSetSpecs RightBrace
  {}

type_x:
  builtinType
  {}
| referencedType
  {}
| constrainedType
  {}

builtinType:
  bitStringType
  {}
| booleanType
  {}
| characterStringType
  {}
| choiceType
  {}
| embeddedPDVType
  {}
| enumeratedType
  {}
| externalType
  {}
| instanceOfType
  {}
| integerType
  {}
| nullType
  {}
| objectClassFieldType
  {}
| objectIdentifierType
  {}
| octetStringType
  {}
| realType
  {}
| relativeOIDType
  {}
| sequenceType
  {}
| sequenceOfType
  {}
| setType
  {}
| setOfType
  {}
| taggedType
  {}

namedType:
  Lower /* identifier */
  type_x
  {}

referencedType:
  definedType
  {}
| usefulType
  {}
| selectionType
  {}
| typeFromObject
  {}
| valueSetFromObjects
  {}

value:
  builtinValue
  {}
| referencedValue
  {}
| objectClassFieldValue
  {}

builtinValue:
  bitStringValue
  {}
| booleanValue
  {}
| characterStringValue
  {}
| choiceValue
  {}
| embeddedPDVValue
  {}
| enumeratedValue
  {}
| externalValue
  {}
| integerValue
  {}
| nullValue
  {}
| objectIdentifierValue
  {}
| octetStringValue
  {}
| realValue
  {}
| relativeOIDValue
  {}
| sequenceValue
  {}
| sequenceOfValue
  {}
| setValue
  {}
| setOfValue
  {}

referencedValue:
  definedValue
  {}
| valueFromObject
  {}

namedValue:
  Lower /* valuereference */
  value
  {}

booleanType:
  BOOLEAN
  {}

booleanValue:
  TRUE
  {}
| FALSE
  {}

integerType:
  INTEGER
  {}
| INTEGER LeftBrace namedNumberList RightBrace
  {}

namedNumberList:
  namedNumber
  {}
| namedNumberList Comma namedNumber
  {}

namedNumber:
  Lower /* identifier */
  LeftParen signedNumber RightParen
  {}
| Lower /* identifier */
  LeftParen definedValue RightParen
  {}

signedNumber:
  Number
  {}
| Hyphen Number /* Number != 0 */
  {}

integerValue:
  signedNumber
  {}
| Lower /* identifier */
  {}

enumeratedType:
  ENUMERATED LeftBrace enumerations RightBrace
  {}

enumerations:
  rootEnumeration
  {}
| rootEnumeration Comma Ellipsis
  exceptionSpec
  {}
| rootEnumeration Comma Ellipsis
  exceptionSpec Comma
  additionalEnumeration
  {}

rootEnumeration:
  enumeration
  {}

additionalEnumeration:
  enumeration
  {}

enumeration:
  enumerationItem
  {}
| enumerationItem Comma enumeration
  {}

enumerationItem:
  Lower /* identifier */
  {}
| namedNumber
  {}

enumeratedValue:
  Lower /* identifier */
  {}

realType:
  REAL
  {}

realValue:
  numericRealValue
  {}
| specialRealValue
  {}

numericRealValue:
  RealNumber
  {}
| Hyphen RealNumber /* RealNumber != 0 or 0.0 etc. */
  {}
| sequenceValue
  {}
/*
  LeftBrace
    Lower (* mantissa *) integerValue Comma
    Lower (* base *) integerValue Comma
    Lower (* exponent *) integerValue
  RightBrace

  The standard says "sequenceValue", which must be a
  value of the associated type

    SEQUENCE {
      mantissa INTEGER,
      base INTEGER (2|10),
      exponent INTEGER
    }
*/

specialRealValue:
  PLUS_INFINITY
  {}
| MINUS_INFINITY
  {}

bitStringType:
  BIT STRING
  {}
| BIT STRING LeftBrace namedBitList RightBrace
  {}

namedBitList:
  namedBit
  {}
| namedBitList Comma namedBit
  {}

namedBit:
  Lower /* identifier */
  LeftParen Number RightParen
  {}
| Lower /* identifier */
  LeftParen definedValue RightParen
  {}

bitStringValue:
  Bstring
  {}
| Hstring
  {}
| LeftBrace identifierList RightBrace
  {}
| LeftBrace RightBrace
  {}
| CONTAINING value
  {}

identifierList:
  Lower /* identifier */
  {}
| identifierList Comma Lower /* identifier */
  {}

octetStringType:
  OCTET STRING
  {}

octetStringValue:
  Bstring
  {}
| Hstring
  {}
| CONTAINING value
  {}

nullType:
  NULL
  {}

nullValue:
  NULL
  {}

sequenceType:
  SEQUENCE LeftBrace RightBrace
  {}
| SEQUENCE
    LeftBrace
      extensionAndException
      optionalExtensionMarker
    RightBrace
  {}
| SEQUENCE
    LeftBrace
      componentTypeLists
    RightBrace
  {}

extensionAndException:
  Ellipsis
  {}
| Ellipsis exceptionSpec
  {}

optionalExtensionMarker:
  Comma Ellipsis
  {}
| empty
  {}

componentTypeLists:
  rootComponentTypeList
  {}
| rootComponentTypeList Comma
  extensionAndException
  extensionAdditions
  optionalExtensionMarker
  {}
| rootComponentTypeList Comma
  extensionAndException
  extensionAdditions
  extensionEndMarker Comma
  rootComponentTypeList
  {}
| extensionAndException
  extensionAdditions
  extensionEndMarker Comma
  rootComponentTypeList
  {}
| extensionAndException
  extensionAdditions
  optionalExtensionMarker
  {}

rootComponentTypeList:
  componentTypeList
  {}

extensionEndMarker:
  Comma Ellipsis
  {}

extensionAdditions:
  Comma extensionAdditionList
  {}
| empty
  {}

extensionAdditionList:
  extensionAddition
  {}
| extensionAdditionList Comma extensionAddition
  {}

extensionAddition:
  componentType
  {}
| extensionAdditionGroup
  {}

extensionAdditionGroup:
  LeftDoubleBracket
    versionNumber
   componentTypeList
  RightDoubleBracket
  {}

versionNumber:
  empty
  {}
| Number Colon
  {}

componentTypeList:
  componentType
  {}
| componentTypeList Comma componentType
  {}

componentType:
  namedType
  {}
| namedType OPTIONAL
  {}
| namedType DEFAULT value
  {}
| COMPONENTS OF type_x
  {}

sequenceValue:
  LeftBrace componentValueList RightBrace
  {}
| LeftBrace RightBrace
  {}

componentValueList:
  namedValue
  {}
| componentValueList Comma namedValue
  {}

sequenceOfType:
  SEQUENCE OF type_x
  {}
| SEQUENCE OF namedType
  {}

sequenceOfValue:
  LeftBrace valueList RightBrace
  {}
| LeftBrace namedValueList RightBrace
  {}
| LeftBrace RightBrace
  {}

valueList:
  value
  {}
| valueList Comma value
  {}

/* namedValueList is missing in the notation summary of X.680 */
namedValueList:
  namedValue
  {}
| namedValueList Comma namedValue
  {}

setType:
  SET LeftBrace RightBrace
   {}
| SET LeftBrace
        extensionAndException
        optionalExtensionMarker
      RightBrace
  {}
| SET LeftBrace componentTypeLists RightBrace
  {}

setValue:
  LeftBrace componentValueList RightBrace
  {}
| LeftBrace RightBrace
  {}

setOfType:
  SET OF type_x
  {}
| SET OF namedType
  {}

setOfValue:
  LeftBrace valueList RightBrace
  {}
| LeftBrace namedValueList RightBrace
  {}
| LeftBrace RightBrace
  {}

choiceType:
  CHOICE LeftBrace
           alternativeTypeLists
         RightBrace
  {}

alternativeTypeLists:
  rootAlternativeTypeList
  {}
| rootAlternativeTypeList Comma
  extensionAndException
  extensionAdditionAlternatives
  optionalExtensionMarker
  {}

rootAlternativeTypeList:
  alternativeTypeList
  {}

extensionAdditionAlternatives:
  Comma extensionAdditionAlternativesList
  {}
| empty
  {}

extensionAdditionAlternativesList:
  extensionAdditionAlternative
  {}
| extensionAdditionAlternativesList Comma
  extensionAdditionAlternative
  {}

extensionAdditionAlternative:
  extensionAdditionAlternativesGroup
  {}
| namedType
  {}

extensionAdditionAlternativesGroup:
  LeftDoubleBracket
    versionNumber
    alternativeTypeList
  RightDoubleBracket
  {}

alternativeTypeList:
  namedType
  {}
| alternativeTypeList Comma namedType
  {}

choiceValue:
  Lower /* identifier */
  Colon
  value
  {}

selectionType:
  Lower /* identifier */
  LessThan
  type_x
  {}

taggedType:
  tag type_x
  {}
| tag IMPLICIT type_x
  {}
| tag EXPLICIT type_x
  {}

tag:
  LeftBracket class_x  classNumber RightBracket
  {}

classNumber:
  Number
  {}
| definedValue
  {}

class_x:
  UNIVERSAL
  {}
| APPLICATION
  {}
| PRIVATE
  {}
| empty
  {}

embeddedPDVType:
  EMBEDDED PDV
  {}

embeddedPDVValue:
  sequenceValue
  {}
/*
The associated type is defined in X.680 33.5 as
  SEQUENCE {
    identification CHOICE {
      syntaxes SEQUENCE {
        abstract OBJECT IDENTIFIER,
        transfer OBJECT IDENTIFIER
        },
      syntax OBJECT IDENTIFIER,
      presentation-context-id INTEGER,
      context-negotiation SEQUENCE {
        presentation-context-id INTEGER,
        transfer-syntax OBJECT IDENTIFIER
        },
      transfer-syntax OBJECT IDENTIFIER,
      fixed NULL
     },
   data-value-descriptor ObjectDescriptor OPTIONAL,
   string-value OCTET STRING
  } (WITH COMPONENTS {..., data-value-descriptor ABSENT})
*/

externalType:
  EXTERNAL
  {}

externalValue:
  sequenceValue
  {}
/*
The associated type is defined in X.680, 34.5 as
SEQUENCE {
  identification CHOICE {
    syntaxes SEQUENCE {
      abstract OBJECT IDENTIFIER,
      tranfer OBJECT IDENTIFIER
    },
    syntax OBJECT IDENTIFIER,
    presentation-context-id INTEGER,
    context-negotiation SEQUENCE {
      presentation-context-id INTEGER,
      transfer-syntax OBJECT IDENTIFIER
    },
    transfer-syntax OBJECT IDENTIFIER,
    fixed NULL
  },
  data-value-descriptor ObjectDescriptor OPTIONAL,
  data-value OCTET STRING
} (WITH COMPONENTS {
     ...,
    identification (WITH COMPONENTS {
                      ...,
                      syntaxes ABSENT,
                      transfer-syntax ABSENT,
                      fixed ABSENT})})
*/

objectIdentifierType:
  OBJECT IDENTIFIER
  {}

objectIdentifierValue:
  LeftBrace objIdComponentsList RightBrace
  {}
| LeftBrace definedValue objIdComponentsList RightBrace
  {}

objIdComponentsList:
  objIdComponents
  {}
| objIdComponents objIdComponentsList
  {}

objIdComponents:
  nameForm
  {}
| numberForm
  {}
| nameAndNumberForm
  {}
| definedValue
  {}

nameForm:
  Lower /* identifier */
  {}

numberForm:
  Number
  {}
| definedValue
  {}

nameAndNumberForm:
  Lower /* identifier */
  LeftParen numberForm RightParen
  {}

relativeOIDType:
  RELATIVE_OID
  {}

relativeOIDValue:
  LeftBrace relativeOIDComponentsList RightBrace
  {}

relativeOIDComponentsList:
  relativeOIDComponents
  {}
| relativeOIDComponents relativeOIDComponentsList
  {}

relativeOIDComponents:
  numberForm
  {}
| nameAndNumberForm
  {}
| definedValue
  {}

characterStringType:
  restrictedCharacterStringType
  {}
| unrestrictedCharacterStringType
  {}

restrictedCharacterStringType:
  BMPString
  {}
| GeneralString
  {}
| GraphicString
  {}
| IA5String
  {}
| ISO646String
  {}
| NumericString
  {}
| PrintableString
  {}
| TeletexString
  {}
| T61String
  {}
| UniversalString
  {}
| UTF8String
  {}
| VideotexString
  {}
| VisibleString
  {}

restrictedCharacterStringValue:
  Cstring
  {}
| characterStringList
  {}
| quadruple
  {}
| tuple
  {}

characterStringList:
  LeftBrace charSyms RightBrace
  {}

charSyms:
  charsDefn
  {}
| charSyms Comma charsDefn
  {}

charsDefn:
  Cstring
  {}
| quadruple
  {}
| tuple
  {}
| definedValue
  {}

quadruple:
  LeftBrace
    group Comma
    plane Comma
    row Comma
    cell
  RightBrace
  {}

group:
  Number
  {}

plane:
  Number
  {}

row:
  Number
  {}

cell:
  Number
  {}

tuple:
  LeftBrace tableColumn Comma tableRow RightBrace
  {}

tableColumn:
  Number
  {}

tableRow:
  Number
  {}

unrestrictedCharacterStringType:
  CHARACTER STRING
  {}

characterStringValue:
  restrictedCharacterStringValue
  {}
| unrestrictedCharacterStringValue
  {}

unrestrictedCharacterStringValue:
  sequenceValue
  {}
/*
The associated type is defined in X.680, 40.5 as
SEQUENCE {
  identification CHOICE {
    syntaxes SEQUENCE {
      abstract OBJECT IDENTIFIER,
      transfer OBJECT IDENTIFIER
    },
    syntax OBJECT IDENTIFIER,
    presentation-context-id INTEGER,
    context-negotiation SEQUENCE {
      presentation-context-id INTEGER,
      transfer-syntax OBJECT IDENTIFIER
    },
    transfer-syntax OBJECT IDENTIFIER,
    fixed NULL,
  },
  data-value-descriptor ObjectDescriptor OPTIONAL,
  string0value OCTET STRING
} (WITH COMPONENTS {
     ...,
    data-value-descriptor ABSENT})
*/

usefulType:
  GeneralizedTime
  {}
| UTCTime
  {}
| ObjectDescriptor
  {}

constrainedType:
  type_x constraint_x
  {}
| typeWithConstraint
  {}

typeWithConstraint:
  SET constraint_x OF type_x
  {}
| SET sizeConstraint OF type_x
  {}
| SEQUENCE constraint_x OF type_x
  {}
| SEQUENCE sizeConstraint OF type_x
  {}
| SET constraint_x OF namedType
  {}
| SET sizeConstraint OF namedType
  {}
| SEQUENCE constraint_x OF namedType
  {}
| SEQUENCE sizeConstraint OF namedType
  {}

constraint_x:
  LeftParen constraintSpec exceptionSpec RightParen
  {}

constraintSpec:
  subtypeConstraint
  {}
| generalConstraint
  {}

exceptionSpec:
  Bang exceptionIdentification
  {}
| empty
  {}

exceptionIdentification:
  signedNumber
  {}
| definedValue
  {}
| type_x Colon value
  {}

subtypeConstraint:
  elementSetSpecs
  {}

elementSetSpecs:
  rootElementSetSpec
  {}
| rootElementSetSpec Comma Ellipsis
  {}
| rootElementSetSpec Comma Ellipsis Comma
  additionalElementSetSpec
  {}

rootElementSetSpec:
  elementSetSpec
  {}

additionalElementSetSpec:
  elementSetSpec
  {}

elementSetSpec:
  unions
  {}
| ALL exclusions
  {}

unions:
  intersections
  {}
| uElems unionMark intersections
  {}

uElems:
  unions
  {}

intersections:
  intersectionElements
  {}
| iElems intersectionMark intersectionElements
  {}

iElems:
  intersections
  {}

intersectionElements:
  elements
  {}
| elems exclusions
  {}

elems:
  elements
  {}

exclusions:
  EXCEPT elements
  {}

unionMark:
  Middle
  {}
| UNION
  {}

intersectionMark:
  Circumflex
  {}
| INTERSECTION
  {}

elements:
  subtypeElements
  {}
| objectSetElements
  {}
| LeftParen elementSetSpec RightParen
  {}

subtypeElements:
  singleValue
  {}
| containedSubtype
  {}
| valueRange
  {}
| permittedAlphabet
  {}
| sizeConstraint
  {}
| typeConstraint
  {}
| innerTypeConstraints
  {}
| patternConstraint
  {}

singleValue:
  value
  {}

containedSubtype:
  includes type_x
  {}

includes:
  INCLUDES
  {}
| empty
  {}

valueRange:
  lowerEndPoint DoubleDot upperEndPoint
  {}

lowerEndPoint:
  lowerEndValue
  {}
| lowerEndValue LessThan
  {}

upperEndPoint:
  upperEndValue
  {}
| LessThan upperEndValue
  {}

lowerEndValue:
  value
  {}
| MIN
  {}

upperEndValue:
  value
  {}
| MAX
  {}

sizeConstraint:
  SIZE constraint_x
  {}

permittedAlphabet:
  FROM constraint_x
  {}

typeConstraint:
  type_x
  {}

innerTypeConstraints:
  WITH COMPONENT singleTypeConstraint
  {}
| WITH COMPONENTS multipleTypeConstraints
  {}

singleTypeConstraint:
  constraint_x
  {}

multipleTypeConstraints:
  fullSpecification
  {}
| partialSpecification
  {}

fullSpecification:
  LeftBrace typeConstraints RightBrace
  {}

partialSpecification:
  LeftBrace
    Ellipsis Comma
    typeConstraints
  RightBrace
  {}

typeConstraints:
  namedConstraint
  {}
| namedConstraint Comma typeConstraints
  {}

namedConstraint:
  Lower /* identifier */
  componentConstraint
  {}

componentConstraint:
  valueConstraint presenceConstraint
  {}

valueConstraint:
  constraint_x
  {}
| empty
  {}

presenceConstraint:
  PRESENT
  {}
| ABSENT
  {}
| OPTIONAL
  {}
| empty
  {}

patternConstraint:
  PATTERN value
  {}

/*------------------------- X.681 -------------------------*/

definedObjectClass:
  externalObjectClassReference
  {}
| Upper /* objectclassreference */
  {}
| usefulObjectClassReference
  {}

externalObjectClassReference:
  Upper /* modulereference */
  Dot
  Upper /* objectclassreference */
  {}

usefulObjectClassReference:
  TYPE_IDENTIFIER
  {}
| ABSTRACT_SYNTAX
  {}

objectClassAssignment:
  Upper /* objectclassreference */
  Assignment
  objectClass
  {}

objectClass:
  definedObjectClass
  {}
| objectClassDefn
  {}
| parameterizedObjectClass
  {}

objectClassDefn:
  CLASS
    LeftBrace
      separated_nonempty_list(Comma,fieldSpec)
    RightBrace
  withSyntaxSpec?
  {}

fieldSpec:
  typeFieldSpec
  {}
| fixedTypeValueFieldSpec
  {}
| variableTypeValueFieldSpec
  {}
| fixedTypeValueSetFieldSpec
  {}
| variableTypeValueSetFieldSpec
  {}
| objectFieldSpec
  {}
| objectSetFieldSpec
  {}

primitiveFieldName:
  UpperField
  /* typefieldreference
     valuesetfieldreference
     objectsetfieldreference */
  {}
| LowerField /* valuefieldreference objectfieldreference */
  {}

fieldName:
  separated_nonempty_list(Dot,primitiveFieldName)
  {}

typeFieldSpec:
  UpperField /* typefieldreference */
  typeOptionalitySpec?
  {}

typeOptionalitySpec:
  OPTIONAL
  {}
| DEFAULT type_x
  {}

fixedTypeValueFieldSpec:
  LowerField /* valuefieldreference */
  type_x
  UNIQUE?
  valueOptionalitySpec?
  {}

valueOptionalitySpec:
  OPTIONAL
  {}
| DEFAULT value
  {}

variableTypeValueFieldSpec:
  LowerField /* valuefieldreference */
  fieldName
  valueOptionalitySpec?
  {}

fixedTypeValueSetFieldSpec:
  UpperField /* valuesetfieldreference */
  type_x
  valueSetOptionalitySpec?
  {}

valueSetOptionalitySpec:
  OPTIONAL
  {}
| DEFAULT valueSet
  {}

variableTypeValueSetFieldSpec:
  UpperField /* valuesetfieldreference */
  fieldName
  valueSetOptionalitySpec?
  {}

objectFieldSpec:
  LowerField /* objectfieldreference */
  definedObjectClass
  objectOptionalitySpec?
  {}

objectOptionalitySpec:
  OPTIONAL
  {}
| DEFAULT object_x
  {}

objectSetFieldSpec:
  UpperField /* objectsetfieldreference */
  definedObjectClass
  objectSetOptionalitySpec?
  {}

objectSetOptionalitySpec:
  OPTIONAL
  {}
| DEFAULT objectSet
  {}

withSyntaxSpec:
  WITH SYNTAX syntaxList
  {}

syntaxList:
  LeftBrace tokenOrGroupSpec+ RightBrace
  {}

tokenOrGroupSpec:
  requiredToken
  {}
| optionalGroup
  {}

optionalGroup:
  LeftBracket tokenOrGroupSpec+ RightBrace
  {}

requiredToken:
  literal
  {}
| primitiveFieldName
  {}

literal:
  Upper /* Word */
  {}
| Comma
  {}

definedObject:
  externalObjectReference
  {}
| Lower /* objectreference */
  {}

externalObjectReference:
  Upper /* modulereference */
  Dot
  Lower /* objectreference */
  {}

objectAssignment:
  Lower /* objectreference */
  definedObjectClass
  Assignment
  object_x
  {}

object_x:
  definedObject
  {}
| objectDefn
  {}
| objectFromObject
  {}
| parameterizedObject
  {}

objectDefn:
  defaultSyntax
  {}
| DefinedSyntax
  {}

defaultSyntax:
  LeftBrace separated_list(Comma,fieldSetting) RightBrace
  {}

fieldSetting:
  primitiveFieldName setting
  {}

setting:
  type_x
  {}
| value
  {}
| valueSet
  {}
| object_x
  {}
| objectSet
  {}

definedObjectSet:
  externalObjectSetReference
  {}
| Upper /* objectsetreference */
  {}

externalObjectSetReference:
  Upper /* modulereference */
  Dot
  Upper /* objectsetreference */
  {}

objectSetAssignment:
  Upper /* objectsetreference */
  definedObjectClass
  Assignment
  objectSet
  {}

objectSet:
  LeftBrace objectSetSpec RightBrace
  {}

objectSetSpec:
  rootElementSetSpec
  {}
| rootElementSetSpec Comma Ellipsis
  {}
| Ellipsis
  {}
| Ellipsis Comma additionalElementSetSpec
  {}
| rootElementSetSpec Comma Ellipsis Comma
  additionalElementSetSpec
  {}

objectSetElements:
  object_x
  {}
| definedObjectSet
  {}
| objectSetFromObjects
  {}
| parameterizedObjectSet
  {}

objectClassFieldType:
  definedObjectClass Dot fieldName
  {}

objectClassFieldValue:
  openTypeFieldVal
  {}
| fixedTypeFieldVal
  {}

openTypeFieldVal:
  type_x Colon value
  {}

fixedTypeFieldVal:
  builtinValue
  {}
| referencedValue
  {}

/* Not used formally. See X.681, 15.1.

informationFromObjects:
  valueFromObject
  {}
| valueSetFromObjects
  {}
| typeFromObject
  {}
| objectFromObject
  {}
| objectSetFromObjects
  {}
*/

referencedObjects:
  definedObject
  {}
| parameterizedObject
  {}
| definedObjectSet
  {}
| parameterizedObjectSet
  {}

valueFromObject:
  referencedObjects Dot fieldName
  {}

valueSetFromObjects:
  referencedObjects Dot fieldName
  {}

typeFromObject:
  referencedObjects Dot fieldName
  {}

objectFromObject:
  referencedObjects Dot fieldName
  {}

objectSetFromObjects:
  referencedObjects Dot fieldName
  {}

instanceOfType:
  INSTANCE OF definedObjectClass
  {}

/*------------------------- X.682 -------------------------*/

generalConstraint:
  userDefinedConstraint
  {}
| tableConstraint
  {}
| contentsConstraint
  {}

userDefinedConstraint:
  CONSTRAINED BY
    LeftBrace
      separated_list(Comma,userDefinedConstraintParameter)
    RightBrace
  {}

userDefinedConstraintParameter:
  governor Colon value
  {}
| governor Colon valueSet
  {}
| governor Colon object_x
  {}
| governor Colon objectSet
  {}
| type_x
  {}
| definedObjectClass
  {}

tableConstraint:
  simpleTableConstraint
  {}
| componentRelationConstraint
  {}

simpleTableConstraint:
  objectSet
  {}

componentRelationConstraint:
  LeftBrace definedObjectSet RightBrace
  LeftBrace separated_nonempty_list(Comma,atNotation) RightBrace
  {}

atNotation:
  At componentIdList
  {}
| AtDot level componentIdList
  {}

level:
  Dot level
  {}
| empty
  {}

componentIdList:
  separated_nonempty_list(Dot,Lower) /* identifier */
  {}

contentsConstraint:
  CONTAINING type_x
  {}
| ENCODED BY value
  {}
| CONTAINING type_x ENCODED BY value
  {}

/*------------------------- X.683 -------------------------*/

parameterizedAssignment:
  parameterizedTypeAssignment
  {}
| parameterizedValueAssignment
  {}
| parameterizedValueSetTypeAssignment
  {}
| parameterizedObjectClassAssignment
  {}
| parameterizedObjectAssignment
  {}
| parameterizedObjectSetAssignment
  {}

parameterizedTypeAssignment:
  Upper /* typereference */
  parameterList
  Assignment
  type_x
  {}

parameterizedValueAssignment:
  Lower /* valuereference */
  parameterList
  type_x
  Assignment
  value
  {}

parameterizedValueSetTypeAssignment:
  Upper /* typereference */
  parameterList
  type_x
  Assignment
  valueSet
  {}

parameterizedObjectClassAssignment:
  Upper /* objectclassreference */
  parameterList
  Assignment
  objectClass
  {}

parameterizedObjectAssignment:
  Lower /* objectreference */
  parameterList
  definedObjectClass
  Assignment
  object_x
  {}

parameterizedObjectSetAssignment:
  Upper /* objectsetreference */
  parameterList
  definedObjectClass
  Assignment
  objectSet
  {}

parameterList:
  LeftBrace separated_nonempty_list(Comma,parameter) RightBrace
  {}

parameter:
  paramGovernor Colon dummyReference
  {}
| dummyReference
  {}

paramGovernor:
  governor
  {}
| dummyGovernor
  {}

governor:
  type_x
  {}
| definedObjectClass
  {}

dummyGovernor:
  dummyReference
  {}

dummyReference:
  reference
   {}

parameterizedReference:
  reference
  {}
| reference LeftBrace RightBrace
  {}

simpleDefinedType:
  externalTypeReference
  {}
| Upper /* typereference */
  {}

simpleDefinedValue:
  externalValueReference
  {}
| Lower /* valuereference */
  {}

parameterizedType:
  simpleDefinedType actualParameterList
  {}

parameterizedValue:
  simpleDefinedValue actualParameterList
  {}

parameterizedValueSetType:
  simpleDefinedType actualParameterList
  {}

parameterizedObjectClass:
  definedObjectSet actualParameterList
  {}

parameterizedObject:
  definedObject actualParameterList
  {}

parameterizedObjectSet:
  definedObjectSet actualParameterList
  {}

actualParameterList:
  LeftBrace
    separated_nonempty_list(Comma,actualParameter)
  RightBrace
  {}

actualParameter:
  type_x
  {}
| value
  {}
| valueSet
  {}
| definedObjectClass
  {}
| object_x
  {}
| objectSet
  {}
