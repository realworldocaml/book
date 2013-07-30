# Parsing with OCamllex and Menhir

OCaml provides lexer and parser generators modeled on lex and yacc.  Similar
tools are available in a variety of languages, and with them you can parse a
variety of kinds of input, including web formats or full blown programming
languages.

Let's be more precise about these terms.  By _parsing_, we mean reading a
textual input into a form that is easier for a program to manipulate.  For
example, suppose we want to read a file containing a value in JSON format.  JSON
has a variety of values, including numbers, strings, arrays, and objects, and
each of these has a textual representation.  For example, the following
text represents an object containing a string labeled `title`, and an array
containing two objects, each with a name and array of zip codes.

```json
{ title: "Cities",
  cities: [{ name: "Chicago", zips: [60601] },
           { name: "New York", zips: [10004] }]
}
```

The input text is represented as a sequence of characters.  Manipulating it in
that form would be really hard, so what we want is to give it a structured type
that is easier for our programs to manipulate.  For our example, we'll use the
following type to represent JSON _abstract syntax_.

```ocaml
type value = [
| `Assoc of (string * value) list
| `Bool of bool
| `Float of float
| `Int of int
| `List of value list
| `String of string
| `Null ]
```

The objective of _parsing_ is to convert the text input into a value of type
`value`.  This is normally done in two phase.  First, _lexical_ analysis (or
lexing, for short) is used to convert the text input into a sequence of tokens,
or words.  For example, the JSON input would be tokenized into a sequence of
tokens like the following.  In most cases (and in this example), lexical
analysis will choose to omit white space from the token stream.

```
LEFT_BRACE, ID("title"), COLON, STRING("Cities"), COMMA, ID("cities"), ...
```

The next step is to convert the token stream into a program value that
represents the abstract syntax tree, like the type `value` above.  This is
called _parsing_.

```
`Assoc
  ["title", `String "Cities";
   "cities", `List
     [`Assoc ["name", `String "Chicago"; "zips", `List [Int 60601]];
	   `Assoc ["name", `String "New York"; "zips", `List [Int 10004]]]]
```

There are many techniques for lexing and parsing.  In the lex/yacc world, lexing
is specified using regular expressions, and parsing is specified using
context-free grammars.  These are concepts from formal languages; the lex/yacc
tools construct the machinery for you.  For `lex`, this means constructing a
finite automaton; and for `yacc`, this means constructing a pushdown automaton.

Parsing is a broad and often intricate topic, and our purpose here is not to
teach all of the ins and outs of yacc and lex, but to show how to use these
tools in OCaml.  There are online resources, and most experience you may have
using lex/yacc in other languages will also apply in OCaml.  However, there are
differences, and we'll try to point out the larger ones here.

For illustration, let's continue with the JSON example.  For lexing, we'll use
`ocamllex`, and for parsing, we'll use `menhir`, which is somewhat easier to
use than `ocamlyacc`.

## Defining a JSON parser with menhir

The process of building a parser is interleaved between constructing the lexer
and parser; you will have to do them simultaneously.  The first step is to
define the set of tokens that will be produced by the lexer.  For various
reasons, the tokens are specified by the parser (to specify what it expects as
input), so we'll start with the parser first.

A parser file has suffix `.mly` (we'll use the name `parser.mly`) and it
contains several parts in the following sequence:

```
  declarations
  %%
  rules
  %%
  optional OCaml code
```
  
The `%%` are section separators; they have to be on a line by themselves.  The
declarations include token and type specifications, precedence directives, and
other things, but we start by declaring the tokens.

### Token declarations

A token is declared using the syntax `%token <`_type_`>` _uid_, where the
`<type>` is optional, and _uid_ is an capitalized identifier.  For JSON, we need
tokens for numbers, strings, identifiers, and punctuation.  To start, let's
define just the tokens in the `parser.mly` file.  For technical reasons, we need
to include a `%start` declaration.  For now, we'll include just a dummy grammar
specification `exp: { () }` (we'll replace this when we implement the grammar
below).

```ocaml
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

%start <unit> exp

%%

exp: { () }
```

The `<`_type_`>` specifications mean that a token carries a value.  The `INT`
token carries an integer value with it, `FLOAT` has a `float` value, etc.  Most
of the remaining tokens, like `TRUE`, `FALSE`, the punctuation, aren't associated
with any value, so we omit the `<`_type_`>` specification.

Compile this file with `menhir`.  It will issue multiple warnings about unused
tokens because we haven't actually defined a grammar yet.  It is ok to ignore
the warnings for now.

```console
$ menhir parser.mly
Warning: the token COLON is unused.
...
```

The `menhir` tool is a parser generator, meaning it generates the code to
perform parsing from the `parser.mly` description.  The `parser.ml` contains an
automaton implementation, and is generally difficult to read.  However, the
`parser.mli` contains declarations that we need to build a lexer.

```console
$ cat parser.mli
exception Error

type token = 
  | TRUE
  | STRING of (string)
  | RIGHT_BRACK
  | RIGHT_BRACE
  | NULL
  | LEFT_BRACK
  | LEFT_BRACE
  | INT of (int)
  | ID of (string)
  | FLOAT of (float)
  | FALSE
  | EOF
  | COMMA
  | COLON


val exp: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit)
```

### Specifying the grammar rules

The grammar itself is specified using a set of rules, where a rule contains a
set of productions.  Abstractly, a production looks like the following.

```
symbol: [ id1 = ] symbol1; [ id2 = ] symbol2; ...; [ idN = ] symbolN
   { OCaml code }
```

A production can be interpreted as follows: given values `id1`, ..., `idN` for
the input symbols `symbol1`, ..., `symbolN`; the OCaml code computes a value for
the target `symbol`.  That's too abstract, so let's get down to defining
productions for parsing JSON.  Here is the main production for a JSON value.

```
value: LEFT_BRACE; obj = opt_object_fields; RIGHT_BRACE
    { `Assoc obj }
  | LEFT_BRACK; vl = list_values; RIGHT_BRACK
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
```

We can read it like this: a JSON `value` is either an object bracketed by curly
braces, or an array bracketed by square braces. or a string, integer, float,
etc.  In each of the productions, the right hand side specfies the expected
sequence.  For example, the object is specified with the curly-bracket
production.

```
value: LEFT_BRACE; obj = opt_object_fields; RIGHT_BRACE
    { `Assoc obj }
```

That is, an object value starts with a `LEFT_BRACE`, contains some optional
object field values (to be defined), and ends with a `RIGHT_BRACE`.  The returned
value is `Assoc obj`, where `obj` is the sequence of object fields.  Note that
we've left out bindings for `LEFT_BRACE` and `RIGHT_BRACE`, because their tokens
don't have values.

Next, let's define the object fields.  In the following rules, the
`opt_object_fields` are either empty, or a non-empty sequence of fields in
reverse order.  Note that if you wish to have comments in the rule definitions,
you have to use C comment delimiters.  By convention, the C comment `/*
empty */` is used to point out that a production has an empty right hand side.

```
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
```

The rule `rev_object_fields` is defined recursively.  It has either one
key/value field, or it is a sequence of fields, followed by a `COMMA` and one
more field definition.

The `rev_` prefix is intended to point out that the fields are returned in
reverse order.  Why would we do that?  One reason is that the `menhir` parser
generator is left-recursive, which means that the constructed pushdown automoton
uses less stack space with left-recursive definitions.  The following
right-recursive rule accepts the same input, but during parsing it requires
linear stack space to read object field definitions.

```
/* Inefficient right-recursive rule */
object_fields: k = ID; COLON; v = value
    { [k, v] }
  | k = ID; COLON; v = value; COMMA; obj = object_fields
    { (k, v) :: obj }
```

Alternatively, we could keep the left-recursive definition and simply construct
the returned value in left-to-right order.  This is fine, though less efficient.
You will have to choose you technique according to circumstances.

```
/* Quadratic left-recursive rule */
object_fields: k = ID; COLON; v = value
    { [k, v] }
  | obj = rev_object_fields; COMMA; k = ID; COLON; v = value
    { obj @ [k, v] }
  ;
```

Finally, we can finish off the grammar by defining the rules for arrays, and
adding a correct `%start` production.  For the `%start` production, we'll return
a `value option`, using `None` to represent end of file.  Here is the complete file.

```
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
    { `Assoc obj }
  | LEFT_BRACK; vl = list_values; RIGHT_BRACK
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

list_values: /* empty */
    { [] }
  | vl = rev_values
    { List.rev vl }
  ;

rev_values: v = value
    { [v] }
  | vl = rev_values; COMMA; v = value
    { v :: vl }
  ;
```

That's it.  We can compile this with `menhir`, which will now no longer complain
about unused symbols.

## Defining a lexer with ocamllex

For the next part, we need to define a lexer to tokenize the input text, meaning
that we break the input into a sequence of words or tokens.  For this, we'll
define a lexer using `ocamllex`.  In this case, the specification is placed in a
file with a `.mll` suffix (we'll use the name `lexer.mll`).  A lexer file has
several parts in the following sequence.

```
{ OCaml code }
let definitions...
rules...
{ OCaml code }
```

### Let-definitions for regular expressions

The OCaml code for the header and trailer is optional.  The let-definitions are
used to ease the definition of regular expressions.  They are optional, but very
useful.  To get started, we know that we'll need to match numbers and strings,
so let's define names for the regular expressions that specify their form.

An integer is a sequence of digits, optionally preceded by a minus sign.
Leading zeroes are not allowed.  The question mark means that the preceding
symbol `-` is optional.  The square brackets ['1'-'9'] define a character range,
meaning that the first digit of the integer should be 1-9.  The final range
`['0'-'9']*` includes star `*`, which means zero-or-more occurrences of the
characters 0-9.  Read formally then, an `int` has an optional minus sign,
followed by a digit in the range 1-9, followed by zero or more digits in the
range 0-9.

```
let int = '-'? ['1'-'9'] ['0'-'9']*
```

Floating-point numbers are similar, but we deal with decimal points and
exponents.  We can use multiple let-definitions for the different parts.

```
let digits = ['0'-'9']+
let frac = '.' digits
let exp = ['e' 'E'] ['-' '+']? digits
let float = int (frac | exp | frac exp)
```

The `digits` expression has a `+` symbol, meaning that `digits` has one or more
occurrences of digits in the range 0-9.  A fractional part `frac` has a decimal
point followed by some digits; an exponent `exp` begins with an `e` followed by
some digits; and a `float` has an integer part, and one or both of a `frac` and
`exp` part.  The vertical bar is a choice; the expression `(frac | exp | frac
exp)` is either a `frac`, or an `exp`, or a `frac` followed by an `exp`.

Finally, let's define identifiers and whitespace.  An identifier (label), is an
alphanumeric sequence not beginning with a digit.

```
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
```

### Lexing rules

The lexing rules are specified as a set of `parse` rules.  A `parse` rule has a
regular expression followed by OCaml code that defines a semantic action.  Let's
write JSON parse rule.

```
rule read = parse
| white { read lexbuf }
| newline { next_line lexbuf; read lexbuf }
| int { INT (int_of_string (Lexing.lexeme lexbuf)) }
| float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
| "true" { TRUE }
| "false" { FALSE }
| "null" { NULL }
| id { ID (Lexing.lexeme lexbuf) }
| '"' { read_string (Buffer.create 17) lexbuf }
| '{' { LEFT_BRACE }
| '}' { RIGHT_BRACE }
| '[' { LEFT_BRACK }
| ']' { RIGHT_BRACK }
| ':' { COLON }
| ',' { COMMA }
| _ { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
| eof { EOF }
```

The OCaml code for the rules has a parameter called `lexbuf` that defines the
input, including the position in the input file, as well as the text that was
matched by the regular expression.  Let's skip to the third action.

```
| int { INT (int_of_string (Lexing.lexeme lexbuf)) }
```

This action specifies that when the input matches the `int` regular expression
(defined as `'-'? ['1'-'9'] ['0'-'9']*`, then the lexer should return the
expression `INT (int_of_string (Lexing.lexeme lexbuf))`.  The expression
`Lexing.lexeme lexbuf` returns the complete string matched by the regular
expression.  In this case, the string represents a number, so we use the
`int_of_string` function to convert it to a number.

Going back to the first actions, the first `white { read lexbuf }` calls the
lexer recursively.  That's, it skips the input whitespace and returns the
following token.  The action `newline { next_line lexbuf; read lexbuf }` is
similar, but we use it to advance the line number for the lexer.  Here is the
definition of the `next_line` function, which updates the line number in the
`lexbuf`.

```
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
```

There are actions for each different kind of token.  The string expressions like
`"true" { TRUE }` are used for keywords, and the special characters have actions
too, like `'{' { LEFT_BRACE }`.

Some of these patterns overlap.  For example, the regular expression `"true"` is
also matched by the `id` pattern.  `ocamllex` used the following disambiguation
when a prefix of the input is matched by more than one pattern.

* The longest match always wins.  For example, the first input `trueX: 167`
  matches the regular expression `"true"` for 4 characters, and it matches `id`
  for 5 characters.  The longer match wins, and the return value is `ID
  "trueX"`.

* If all matches have the same length, then the first action wins.  If the input
  were `true: 167`, then both `"true"` and `id` match the first 4 characters;
  `"true"` is first, so the return value is `TRUE`.
  
### Recursive rules

Unlike many other lexer generators, `ocamllex` allows the definition of multiple
lexer in the same file, and the definitions can be recursive.  In this case, we
use recursion to match string literals, using the following rule definition.

```
and read_string buf = parse
| '"' { STRING (Buffer.contents buf) }
| '\\' '/' { Buffer.add_char buf '/'; read_string buf lexbuf }
| '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
| '\\' 'b' { Buffer.add_char buf '\b'; read_string buf lexbuf }
| '\\' 'f' { Buffer.add_char buf '\012'; read_string buf lexbuf }
| '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
| '\\' 'r' { Buffer.add_char buf '\r'; read_string buf lexbuf }
| '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
| '\\' 'u' hex hex hex hex
  { let string_code = String.sub (Lexing.lexeme lexbuf) 2 4 in
    let code = int_of_string ("0x" ^ string_code) in
    add_utf8 buf code;
    read_string buf lexbuf
  }
| [^ '"' '\\']+
  { Buffer.add_string buf (Lexing.lexeme lexbuf);
    read_string buf lexbuf
  }
| _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
| eof { raise (SyntaxError ("String is not terminated")) }
```

This rule takes a `buf : Buffer.t` as an argument.  If we reach the terminating
double quote `"`, then we return the contents of the buffer as a `STRING`.


The other cases are for handling the string contents.  The action `[^ '"' '\\']+
{ ... }` matches normal input that does not contain a double-quote or backslash.
The actions beginning with a backslash `\` define what to do for escape
sequences.  In each of these cases, the final step includes a recursive call to
the lexer.

As specified by JSON, we also handle Unicode code points, `'\\' 'u' hex hex hex
hex`.  Ocaml doesn't have any built-in handling for Unicode, so in this case we
choose to represent the code point in UTF-8.  We define the following function
for adding the UTF-8 encoding to the buffer.

```ocaml
let add_utf8 buf code =
  if code <= 0x7f then
    Buffer.add_char buf (Char.chr code)
  else if code <= 0x7ff then begin
    Buffer.add_char buf (Char.chr (0b11000000 lor ((code lsr 6) land 0x3f)));
    Buffer.add_char buf (Char.chr (0b10000000 lor (code land 0x3f)))
  end else begin
    Buffer.add_char buf (Char.chr (0b11100000 lor ((code lsr 12) land 0x3f)));
    Buffer.add_char buf (Char.chr (0b10000000 lor ((code lsr 6) land 0x3f)));
    Buffer.add_char buf (Char.chr (0b10000000 lor (code land 0x3f)))
  end
```

That covers the lexer.  Next, we need to combine the lexer with the parser to
bring it all together.

## Bringing it all together

For the final part, we need to compose the lexer and parser.  As we saw in the
type definition in `parser.mli`, the parsing function expects a lexer of type
`Lexing.lexbuf -> token`, and it also expects a `lexbuf`.

```ocaml
val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Json.value option)
```

The standard lexing library `Lexing` provides a function `from_channel` to read
the input from a channel.  The following function describes the structure, where
the `Lexing.from_channel` function is used to construct a `lexbuf`, which is
passed with the lexing function `Lexer.read` to the `Parser.prog` function.
`Parsing.prog` returns `None` when it reaches end of file.  We define a function
`Json.output_value`, not shown here, to print a `Json.value`.

```ocaml
let rec parse_and_print lexbuf =
  match Parser.prog Lexer.read lexbuf with
  | Some value -> Json.output_value stdout value; parse_and_print lexbuf
  | None -> ()
  
let loop filename =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  parse_and_print lexbuf;
  close_in inx
```

This isn't quite right yet -- we need to handle parsing errors.  Currently there
are two errors, `Parser.Error` and `Lexer.SyntaxError`.  A simple solution when
encountering an error is to print the error and give up.

```ocaml
let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
      Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
      None
  | Parser.Error ->
      Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
      None

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value -> Json.output_value stdout value; parse_and_print lexbuf
  | None -> ()
```

The "give up on the first error" approach is easy to implement but isn't
very friendly.  In general, error handling can be pretty intricate, and we won't
discuss it here.  However, the menhir parser defines additional mechanisms you
can use to try and recover from errors, described in it its reference manual.

Here is an example of a successful run on the following input file.

```
$ cat test1.json
true
false
null
[1, 2]
"Hello\r\n\t\b\\\/\u12345"
{ field1: "Hello",
  field2: 17e13,
  field3: [1, 2, 3],
  field4: { fieldA: 1, fieldB: "Hello" }
}

$ ./test test1.json
true
false
null
[1, 2]
"Hello
       \/ሴ5"
{ field1: "Hello", field2: 170000000000000.000000, field3: [1, 2, 3], field4: { fieldA: 1, fieldB: "Hello" } }
```

With our simple error handling scheme, errors are fatal.

```ocaml
$ cat test2.json
{ name: "Chicago",
  zips: [12345,
}
{ name: "New York",
  zips: [10004]
}
$ ./test test2.json
test2.json:3:2: syntax error
```
