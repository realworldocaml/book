# Parsing with OCamllex and Menhir

Many programming tasks start with the interpretion of some form of
structured textual data.  _Parsing_ is the process of converting such
data into data structures that are easy to program against.  For
simple formats, it's often enough to parse the data in an ad-hoc way,
_e.g._, by breaking up the data into lines, and then using regular
expressions for breaking those lines down into their component pieces.

But this simplistic approach tends to fall down when parsing more
complicated data, particularly data with the kind of recursive
structure you find in full-blown programming languages or flexible
data formats like JSON and XML.  Parsing such formats accurately and
efficiently while providing useful error messages is a complex task.

Often, you can find an existing parsing library that handles these
issues for you, as we did in [xref](#handling-json-data).  But there
are tools to help simplify the task when you do need to write a
parser, in the form of _parser generators_.  A parser generator takes
a specification of the data format that you want to parse, and uses
that to generate a parser.

Parser generators have a long history, including venerable tools like
`lex` and `yacc` that date back to the early 1970's.  OCaml has its
own alternatives, including `ocamllex`, which replaces `yacc`, and
`ocamlyacc` and `menhir`, which are replacements for `yacc`.  We'll
explore these tools in the course of walking through the
implementation of a parser for the JSON format.

Parsing is a broad and often intricate topic, and our purpose here is
not to teach all of the theoretical issues, but to provide a pragmatic
introduction of how to build a parser in OCaml.

<note>
<title>Menhir _vs_ `ocamlyacc`</title>

Menhir is an alternative parser generator that is generally superior
to the venerable `ocamlyacc`, which dates back quite a few years.
Menhir is mostly compatible with `ocamlyacc` grammars, and so you can
usually just switch to Menhir and expect older code to work (with some
minor differences described in the Menhir manual).

The biggest advantage of Menhir is that its error messages are
generally more human-comprehensible, and the parsers that it generates
are fully reentrant and can be parameterized in OCaml modules more
easily.  We recommend that any new code you develop should use Menhir
instead of `ocamlyacc`.

Menhir isn't distributed directly with OCaml, but is available through
OPAM by running `opam install menhir`.

</note>

## Lexing and parsing

Parsing is traditionally broken down into two parts: _lexical
analysis_, which is a kind of simplified parsing phase that converts a
stream of characters into a stream of logical tokens; and full-on
parsing, which involves converting a stream of tokens into the final
representation, which is often in the form of a tree-like
data-structure called an _abstract syntax-tree_, or AST.  

It's confusing that the term parsing is applied to both the overall
process of converting textual data to structured data, and also more
specifically to the second phase of converting a stream of tokens to
an AST, so from here on in, we'll use the term parsing to refer only
to this second phase.

Let's consider lexing and parsing in the context of the JSON format.
As discussed in [xref](#handling-json-data), JSON has a variety of
kinds of values, including numbers, strings, arrays, and objects, each
with its own textual representation.  For example, the following text
represents an object containing a string labeled `title`, and an array
containing two objects, each with a name and array of zip codes.

```frag
((typ json)(name parsing/example.json))
```

We could use a type like the following for specifying a token.

```frag
((typ ocaml)(name parsing/manual_token_type.ml))
```

Note that this representation loses some information about the
original text.  For example, white space is not represented.  It's
common, and indeed useful, for the token stream to forget some details
of the original text that are not required for understanding its
meaning.

If we converted the above example into a list of these tokens, it
would look something like this.

```frag
((typ ocaml)(name parsing/tokens.ml))
```

This kind of representation is easier to work with than the original
text, since it gets rid of some unimportant syntactic details and adds
useful structure.  But it's still a good deal more low-level than the
simple AST we used for representing JSON data in
[xref](#handling-json-data), shown below.

```frag
((typ ocaml)(name parsing/json.ml)(part 0))
```

The above type is effectively an AST, and the job of the parser we'll
write will be to convert a token stream into a value of this type, as
shown below.

```frag
((typ ocaml)(name parsing/parsed_example.ml))
```

## Defining a JSON parser with Menhir

The process of building a lexer and a parser are interconnected.  The
first step is to define the set of tokens which are what the lexer and
the parser use to communicate.  With `menhir` and `ocamllex`, the set
of tokens is in our case specified along with the parser.

A parser-specification file has suffix `.mly` and contains several
parts in the following sequence:

```frag
((typ ocamlsyntax)(name parsing/yacc.syntax))
```
  
The `%%` are section separators; they have to be on a line by
themselves.  The declarations include token and type specifications,
precedence directives, and other output directives.  We'll start by
just declaring the tokens.

### Token declarations

A token is declared using the syntax `%token <`_type_`>` _uid_, where
the `<type>` is optional, and _uid_ is a capitalized identifier.  For
JSON, we need tokens for numbers, strings, identifiers, and
punctuation.  To start, let's define just the tokens in the
`parser.mly` file.  For technical reasons, we need to include a
`%start` declaration.  For now, we'll include just a dummy grammar
specification `exp: { () }`, which we'll replace this when we
implement the grammar below.

```frag
((typ ocaml)(name parsing/partial_parser.mly))
```

The `<`_type_`>` specifications mean that a token carries a value.  The `INT`
token carries an integer value with it, `FLOAT` has a `float` value, and both
`ID` and `STRING` carry a `string` value.  The remaining tokens, such as
`TRUE`, `FALSE` or the punctuation, aren't associated with any value and so we
can omit the `<`_type_`>` specification.

If we compile this file with `menhir` (by running `menhir
partial_parser.mly`), we'll see multiple warnings about unused tokens
because we haven't actually defined a grammar yet.  It's ok to ignore
the warnings for now.

The `menhir` tool is a parser generator, meaning it generates the code
to perform parsing from the `parser.mly` description, in the form of
two files, `parser.ml` and `parser.mli`.  The implementation contained
in `parser.ml` is difficult to read, but `parser.mli` exposes a more
comprehensible interface, the only important part of which for now is
the token type that will be generated by the lexer and consumed by the
parser.

```frag
((typ ocaml)(name parsing/partial_parser.mli))
```

### Specifying the grammar rules

The grammar itself is specified using a set of rules, where a rule contains a
set of productions.  Abstractly, a production looks like the following.

```frag
((typ ocamlsyntax)(name parsing/production.syntax))
```

A production can be interpreted as follows: given values `id1...idN` for the
input symbols `symbol1...symbolN`, the OCaml code computes a value for the
target `symbol`.  That's all quite abstract, so let's get down to defining
productions for parsing real JSON values.

The beginning of the JSON parser has the same token rules as before. This time,
we also defines a proper entry point via the `prog` rule.

```frag
((typ ocaml)(name parsing/parser.mly)(part 0))
```

The `%start` declaration is mandatory for every grammar, and the symbols become
the name of a function whose signature is published in the `mli` signature that
is generated after calling `menhir`.  Each start symbol also must have an OCaml
type associated with it.  We've defined this separately via the `%type` keyword
in our example above, but it can also be directly written into the `%start`
declaration.  For more complex grammars, you can improve the quality of error
messages by adding extra `%type` rules for other non-starting symbols too.

Once that's in place, we can add the main production for a JSON value.

```frag
((typ ocaml)(name parsing/parser.mly)(part 1))
```

We can read it like this: a JSON `value` is either an object bracketed by curly
braces, or an array bracketed by square braces. or a string, integer, float,
etc.  In each of the productions, the right hand side specifies the expected
sequence.

For example, an association list starts with a `LEFT_BRACE` token, contains
some optional object field values (to be defined in another rule), and ends
with a `RIGHT_BRACE`.  The returned value is an `Assoc obj` polymorphic
variant, where `obj` is the sequence of object fields.  Note that we've left
out bindings for `LEFT_BRACE` and `RIGHT_BRACE`, because their tokens don't
have values.

### Parsing lists and sequences

Next, let's define the JSON object fields.  In the following rules, the
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
reverse order.  Why would we do that?  One reason is that the Menhir parser
generator is left-recursive, which means that the constructed pushdown automaton
uses less stack space with left-recursive definitions.  The following
right-recursive rule accepts the same input, but during parsing it requires
linear stack space to read object field definitions.

```frag
((typ ocaml)(name parsing/right_rec_rule.ml))
```

Alternatively, we could keep the left-recursive definition and simply construct
the returned value in left-to-right order.  This is fine, though less efficient.
You will have to choose your technique according to circumstances.

```frag
((typ ocaml)(name parsing/quadratic_rule.ml))
```

Assembling lists like this is a pretty common requirement in most realistic
grammars, and the above rules (while useful for illustrating how parsing works)
are rather verbose.  Menhir features an extended standard library of built-in
rules to simplify this handling.  These rules are detailed in the Menhir manual,
but include optional values, pairs of values with optional separators, 
and lists of elements (also with optional separators).

A complete version of the JSON grammar using these more succinct Menhir rules
is shown below.  Notice the use of `separated_list` to parse both JSON objects
and lists with one rule.

```frag
((typ ocaml)(name parsing/short_parser.mly))
```

We can now compile this with Menhir, which will now no longer complain about
unused symbols.  The best way to invoke Menhir is using `corebuild` with the
`-use-menhir` flag: this tells the build system to switch to using `menhir`
instead of `ocamlyacc` to handle files with the `.mly` suffix.

```frag
((typ console)(name parsing-test/build_json_parser.out))
```

## Defining a lexer with ocamllex

For the next part, we need to define a lexer to tokenize the input text, meaning
that we break the input into a sequence of words or tokens.  For this, we'll
define a lexer using `ocamllex`.  In this case, the specification is placed in a
file with a `.mll` suffix (we'll use the name `lexer.mll`).  A lexer file has
several parts in the following sequence.

```frag
((typ ocamlsyntax)(name parsing/lex.syntax))
```

### Let-definitions for regular expressions

The OCaml code for the header and trailer is optional.  The let-definitions are
used to ease the definition of regular expressions by defining utility
functions.  They are optional, but very useful.  To get started, let's define a
utility function that can track the location of tokens across line breaks.

```frag
((typ ocaml)(name parsing/lexer.mll)(part 0))
```

The `Lexing` module defines a `lexbuf` structure that holds all of the lexer
state, including the current location within the source file.  The `next_line`
function simply accesses the `lex_curr_p` field that holds the current location
and updates its line number.  This is intended to be called from within the
lexing regular expressions that we'll define next.

To get started with our rules, we know that we'll need to match numbers and
strings, so let's define names for the regular expressions that specify their
form.

```frag
((typ ocaml)(name parsing/lexer.mll)(part 1))
```

An integer is a sequence of digits, optionally preceded by a minus sign.
Leading zeroes are not allowed.  The question mark means that the preceding
symbol `-` is optional.  The square brackets ['1'-'9'] define a character
range, meaning that the first digit of the integer should be 1-9.  The final
range `['0'-'9']*` includes the star `*`, which means zero-or-more occurrences of
the characters 0-9.  Read formally then, an `int` has an optional minus sign,
followed by a digit in the range 1-9, followed by zero or more digits in the
range 0-9.

Floating-point numbers are similar, but we deal with decimal points and
exponents.  We can use multiple let-definitions for the different parts.

```frag
((typ ocaml)(name parsing/lexer.mll)(part 2))
```

The `digits` expression defines a single character regexp in the range 0-9.  A
fractional part `frac` has a compulsary decimal point followed by some optional
digits; an exponent `exp` begins with an `e` followed by some digits; and a
`float` has an integer part, and one, both or none of a `frac` and `exp` part. 

Finally, let's define identifiers and whitespace.  An identifier (label), is an
alphanumeric sequence not beginning with a digit.

```frag
((typ ocaml)(name parsing/lexer.mll)(part 3))
```


### Lexing rules

The lexing rules are specified as a set of `parse` rules.  A `parse` rule has a
regular expression followed by OCaml code that defines a semantic action.  Let's
write the rule for JSON next.

```frag
((typ ocaml)(name parsing/lexer.mll)(part 4))
```

The rules are structured very similarly to pattern matches, except that the
variants are replaced by regular expressions on the left hand side.  The right
hand side clause is the parsed OCaml return value of that rule.  The OCaml code
for the rules has a parameter called `lexbuf` that defines the input, including
the position in the input file, as well as the text that was matched by the
regular expression.

The first `white { read lexbuf }` calls the lexer recursively.  That is, it
skips the input whitespace and returns the following token.  The action
`newline { next_line lexbuf; read lexbuf }` is similar, but we use it to
advance the line number for the lexer using the utility function that we
defined at the top of the file.  Let's skip to the third action.

```frag
((typ ocaml)(name parsing/lexer_int_fragment.mll))
```

This action specifies that when the input matches the `int` regular expression,
then the lexer should return the expression `INT (int_of_string (Lexing.lexeme
lexbuf))`.  The expression `Lexing.lexeme lexbuf` returns the complete string
matched by the regular expression.  In this case, the string represents a
number, so we use the `int_of_string` function to convert it to a number.

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
lexers in the same file, and the definitions can be recursive.  In this case, we
use recursion to match string literals using the following rule definition.

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

<note>
<title>Handling Unicode</title>

We've glossed over an important detail here: parsing Unicode characters to
handle the full spectrum of the world's writing systems.  OCaml has several
third-party solutions to handling Unicode, with varying degrees of flexibility
and complexity.

* [Camomile](http://camomile.sourceforge.net) supports the full spectrum
of Unicode character types, conversion from around 200 encodings, and collation
and locale-sensitive case mappings.
* [Ulex](http://www.cduce.org/ulex) is a lexer generator for Unicode that can
serve as a Unicode-aware replacement for `ocamllex`.
* [Uutf](http://erratique.ch/software/uutf) is a non-blocking streaming
Unicode codec for OCaml, available as a standalone library.  It is accompanied
by the [Uunf](http://erratique.ch/software/uunf) text normalization and 
[Uucd](http://erratique.ch/software/uucd) Unicode character database libraries.
There is also a robust parser for [JSON](http://erratique.ch/software/jsonm) available that illustrates the use of Uutf in your own libraries.

All of these libraries are available via OPAM under their respective names.

</note>

## Bringing it all together

For the final part, we need to compose the lexer and parser.  As we saw in the
type definition in `parser.mli`, the parsing function expects a lexer of type
`Lexing.lexbuf -> token`, and it also expects a `lexbuf`.

```frag
((typ ocaml)(name parsing/prog.mli))
```

Before we start with the lexing, let's first define some functions to handle
parsing errors.  There are currently two errors: `Parser.Error` and
`Lexer.SyntaxError`.  A simple solution when encountering an error is to print
the error and give up, which we do below.

```frag
((typ ocaml)(name parsing-test/test.ml)(part 0))
```

The "give up on the first error" approach is easy to implement but isn't very
friendly.  In general, error handling can be pretty intricate, and we won't
discuss it here.  However, the Menhir parser defines additional mechanisms you
can use to try and recover from errors. These are described in detail in its
reference [manual](http://gallium.inria.fr/~fpottier/menhir/).

The standard lexing library `Lexing` provides a function `from_channel` to read
the input from a channel.  The following function describes the structure, where
the `Lexing.from_channel` function is used to construct a `lexbuf`, which is
passed with the lexing function `Lexer.read` to the `Parser.prog` function.
`Parsing.prog` returns `None` when it reaches end of file.  We define a function
`Json.output_value`, not shown here, to print a `Json.value`.

```frag
((typ ocaml)(name parsing-test/test.ml)(part 1))
```

Here's a test input file we can use to test the code we just wrote.

```frag
((typ json)(name parsing-test/test1.json))
```

Now build and run the example using this file, and you you can see the full
parser in action.

```frag
((typ console)(name parsing-test/build_test.out))
```

With our simple error handling scheme, errors are fatal and cause the program
to terminate with a non-zero exit code.

```frag
((typ console)(name parsing-test/run_broken_test.out))
```

That wraps up our parsing tutorial.  As an aside, notice that the JSON
polymorphic variant type that we defined in this chapter is actually
structurally compatible with the Yojson representation explained earlier in
[xref](#handling-json-data).  That means that you can take this parser and use
it with the helper functions in Yojson to build more sophisticated
applications.
