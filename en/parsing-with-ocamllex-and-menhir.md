# Parsing with OCamllex and Menhir

OCaml provides lexer and parser generators modeled on lex and yacc.  Similar
tools are available in a variety of languages, and with them you can parse a
variety of kinds of input, including common web formats or full blown programming
languages.

In this chapter, you'll learn how to:

* convert input into a structured token stream using `ocamllex`.
* parse a token stream into an OCaml data structure using `menhir`.
* handle simple errors in the input data.
* use all these tools to build a parser for the JSON format.

## Basic lexing and parsing concepts

Let's be more precise about some of these terms.  By _parsing_, we mean reading a
textual input into a form that is easier for a program to manipulate.

For example, suppose we want to read a file containing a value in JSON format.
JSON has a variety of values, including numbers, strings, arrays, and objects,
and each of these has a textual representation.  For example, the following
text represents an object containing a string labeled `title`, and an array
containing two objects, each with a name and array of zip codes.

```frag
((typ json)(name parsing/example.json))
```

The input text is represented as a sequence of characters.  Manipulating it in
that form would be really hard, so what we want is to give it a structured type
that is easier for our programs to manipulate.  For our example, we'll use the
following type to represent JSON _abstract syntax_.

```frag
((typ ocaml)(name parsing/json.ml)(part 0))
```

### Lexical Analysis

The overall objective of _parsing_ is to convert the text input into a value of type
`value`.  This is normally done in two phases.

First, _lexical_ analysis (or lexing, for short) is used to convert the text
input into a sequence of tokens, or words.  For example, the JSON input would
be tokenized into a sequence of tokens like the following.  In most cases (and
in this example), lexical analysis will choose to omit white space from the
token stream.

```frag
((typ ocaml)(name parsing/tokens.ml))
```

### Parsing

The next step is to convert the token stream into a program value that
represents the abstract syntax tree, like the type `value` above.  This is
called _parsing_.

```frag
((typ ocaml)(name parsing/parsed_example.ml))
```

There are many techniques for lexing and parsing that have a variety of
tradeoffs in the complexity of grammars that they can express, how well
they handle malformed input, and also how resource-intensive the parsing
process is.

In the lex/yacc world, lexing is specified using regular expressions, and
parsing is specified using context-free grammars.  These are concepts from
formal languages that you don't need to understand in huge detail, as the
lex/yacc tools construct the machinery for you.  For lex, this means
constructing a _finite automaton_; and for yacc, this means constructing a
_pushdown automaton_.

Parsing is a broad and often intricate topic, and our purpose here is not to
teach all of the ins and outs of yacc and lex, but to show how to use these
tools in OCaml.  There are online resources for the theoretical background, and
most experience you may have using lex/yacc in other languages will also apply
in OCaml.  However, there are differences, and we'll try to point out the
larger ones here.

For illustration, let's continue with the JSON example.  For lexing, we'll use
`ocamllex`, and for parsing, we'll use Menhir, which is the modern alternative
to `ocamlyacc`.

<note>
<title>Menhir _vs_ `ocamlyacc`</title>

Menhir is an alternative parser generator that is generally superior to
the venerable `ocamlyacc`, which dates back quite a few years.  Menhir
is mostly compatible with `ocamlyacc` grammars, and so you can usually
just switch to Menhir and expect older code to work (with some minor
differences described in the Menhir manual).

The biggest advantage of Menhir is that its error messages are generally
more human-comprehensible, and the parsers that it generates are fully
reentrant and can be parameterized in OCaml modules more easily.  We
recommend that any new code you develop should use Menhir instead of
`ocamlyacc`.

Menhir isn't distributed directly with OCaml, but is available through OPAM by
running `opam install menhir`.

</note>

## Defining a JSON parser with Menhir

The process of building a parser is interleaved between constructing the lexer
and parser; you will have to do them simultaneously.  The first step is to
define the set of tokens that will be produced by the lexer.  For various
reasons, the tokens are specified by the parser (to specify what it expects as
input), so we'll start with the parser first.

A parser file has suffix `.mly` (we'll use the name `parser.mly`) and it
contains several parts in the following sequence:

```frag
((typ ocamlsyntax)(name parsing/yacc.syntax))
```
  
The `%%` are section separators; they have to be on a line by themselves.  The
declarations include token and type specifications, precedence directives, and
other things, but we start by declaring the tokens.

### Token declarations

A token is declared using the syntax `%token <`_type_`>` _uid_, where the
`<type>` is optional, and _uid_ is a capitalized identifier.  For JSON, we need
tokens for numbers, strings, identifiers, and punctuation.  To start, let's
define just the tokens in the `parser.mly` file.  For technical reasons, we need
to include a `%start` declaration.  For now, we'll include just a dummy grammar
specification `exp: { () }` (we'll replace this when we implement the grammar
below).

```frag
((typ ocaml)(name parsing/partial_parser.mly))
```

The `<`_type_`>` specifications mean that a token carries a value.  The `INT`
token carries an integer value with it, `FLOAT` has a `float` value, etc.  Most
of the remaining tokens, like `TRUE`, `FALSE`, the punctuation, aren't associated
with any value, so we omit the `<`_type_`>` specification.

Compile this file with `menhir`.  It will issue multiple warnings about unused
tokens because we haven't actually defined a grammar yet.  It's ok to ignore
the warnings for now.

```frag
((typ console)(name parsing/build_partial_json_parser.out))
```

The `menhir` tool is a parser generator, meaning it generates the code to
perform parsing from the `parser.mly` description.  The `parser.ml` contains an
automaton implementation, and is generally difficult to read.  However, the
`parser.mli` contains declarations that we need to build a lexer.

```frag
((typ ocaml)(name parsing/partial_parser.mli))
```

### Specifying the grammar rules

The grammar itself is specified using a set of rules, where a rule contains a
set of productions.  Abstractly, a production looks like the following.

```
symbol: [ id1 = ] symbol1; [ id2 = ] symbol2; ...; [ idN = ] symbolN
   { OCaml code }
```

A production can be interpreted as follows: given values `id1`, ..., `idN` for
the input symbols `symbol1`, ..., `symbolN`; the OCaml code computes a value
for the target `symbol`.  That's all quite abstract, so let's get down to
defining productions for parsing real JSON values.

The beginning of the parser has the same token rules as before, and also
defines a proper entry point via the `prog` rule.

```frag
((typ ocaml)(name parsing/parser.mly)(part 0))
```

Once that's in place, we can add the main production for a JSON value.

```frag
((typ ocaml)(name parsing/parser.mly)(part 1))
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

```frag
((typ ocaml)(name parsing/parser.mly)(part 2))
```

The rule `rev_object_fields` is defined recursively.  It has either one
key/value field, or it is a sequence of fields, followed by a `COMMA` and one
more field definition.

The `rev_` prefix is intended to point out that the fields are returned in
reverse order.  Why would we do that?  One reason is that the `menhir` parser
generator is left-recursive, which means that the constructed pushdown automaton
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
You will have to choose your technique according to circumstances.

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
a `value option`, using `None` to represent end-of-file.  Here's the last part
with the array rules fille din.

```frag
((typ ocaml)(name parsing/parser.mly)(part 3))
```

That's it.  We can compile this with `menhir`, which will now no longer complain
about unused symbols.

```frag
((typ console)(name parsing/build_json_parser.out))
```

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

```frag
((typ ocaml)(name parsing/lexer.mll)(part 1))
```

Floating-point numbers are similar, but we deal with decimal points and
exponents.  We can use multiple let-definitions for the different parts.

```frag
((typ ocaml)(name parsing/lexer.mll)(part 2))
```

The `digits` expression has a `+` symbol, meaning that `digits` has one or more
occurrences of digits in the range 0-9.  A fractional part `frac` has a decimal
point followed by some digits; an exponent `exp` begins with an `e` followed by
some digits; and a `float` has an integer part, and one or both of a `frac` and
`exp` part.  The vertical bar is a choice; the expression `(frac | exp | frac
exp)` is either a `frac`, or an `exp`, or a `frac` followed by an `exp`.

Finally, let's define identifiers and whitespace.  An identifier (label), is an
alphanumeric sequence not beginning with a digit.

```frag
((typ ocaml)(name parsing/lexer.mll)(part 3))
```

### Lexing rules

The lexing rules are specified as a set of `parse` rules.  A `parse` rule has a
regular expression followed by OCaml code that defines a semantic action.  Let's
write JSON parse rule.

```frag
((typ ocaml)(name parsing/lexer.mll)(part 3))
```

The OCaml code for the rules has a parameter called `lexbuf` that defines the
input, including the position in the input file, as well as the text that was
matched by the regular expression.  Let's skip to the third action.

```frag
((typ ocaml)(name parsing/lexer_int_fragment.mll))
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

```frag
((typ ocaml)(name parsing/lexer.mll)(part 0))
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

```frag
((typ ocaml)(name parsing/lexer.mll)(part 5))
```

This rule takes a `buf : Buffer.t` as an argument.  If we reach the terminating
double quote `"`, then we return the contents of the buffer as a `STRING`.

The other cases are for handling the string contents.  The action `[^ '"' '\\']+
{ ... }` matches normal input that does not contain a double-quote or backslash.
The actions beginning with a backslash `\` define what to do for escape
sequences.  In each of these cases, the final step includes a recursive call to
the lexer.

That covers the lexer.  Next, we need to combine the lexer with the parser to
bring it all together.

## Bringing it all together

For the final part, we need to compose the lexer and parser.  As we saw in the
type definition in `parser.mli`, the parsing function expects a lexer of type
`Lexing.lexbuf -> token`, and it also expects a `lexbuf`.

```frag
((typ ocaml)(name parsing/prog.mli))
```

The standard lexing library `Lexing` provides a function `from_channel` to read
the input from a channel.  The following function describes the structure, where
the `Lexing.from_channel` function is used to construct a `lexbuf`, which is
passed with the lexing function `Lexer.read` to the `Parser.prog` function.
`Parsing.prog` returns `None` when it reaches end of file.  We define a function
`Json.output_value`, not shown here, to print a `Json.value`.

```frag
((typ ocaml)(name parsing-test/test.ml)(part 1))
```

This isn't quite right yet -- we need to handle parsing errors.  Currently there
are two errors, `Parser.Error` and `Lexer.SyntaxError`.  A simple solution when
encountering an error is to print the error and give up.

```frag
((typ ocaml)(name parsing-test/test.ml)(part 0))
```

The "give up on the first error" approach is easy to implement but isn't
very friendly.  In general, error handling can be pretty intricate, and we won't
discuss it here.  However, the menhir parser defines additional mechanisms you
can use to try and recover from errors, described in it its reference manual.

Here's a test input file we can use to test the code we just wrote.

```frag
((typ json)(name parsing-test/test1.json))
```

Now build and run the example using this file, and you you can see the full
parser in action.

```frag
((typ console)(name parsing-test/build_test.out))
```

With our simple error handling scheme, errors are fatal.

```frag
((typ console)(name parsing-test/run_broken_test.out))
```
