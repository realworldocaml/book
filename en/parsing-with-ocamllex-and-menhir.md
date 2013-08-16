# Parsing with OCamllex and Menhir

Many programming tasks start with the interpretion of some form of
structured textual data.  _Parsing_ is the process of converting such
data into data structures that are easy to program against.  For
simple formats, it's often enough to parse the data in an ad-hoc way,
say, by breaking up the data into lines, and then using regular
expressions for breaking those lines down into their component pieces.

But this simplistic approach tends to fall down when parsing more
complicated data, particularly data with the kind of recursive
structure you find in full-blown programming languages or flexible
data formats like JSON and XML.  Parsing such formats accurately and
efficiently while providing useful error messages is a complex task.

Often, you can find an existing parsing library that handles these
issues for you.  But there are tools to help simplify the task when
you do need to write a parser, in the form of _parser generators_.  A
parser generator creates a parser from a specification of the data
format that you want to parse, and uses that to generate a parser.

Parser generators have a long history, including tools like <command>lex</command> and
`yacc` that date back to the early 1970's.  OCaml has its own
alternatives, including <command>ocamllex</command>, which replaces <command>lex</command>, and
<command>ocamlyacc</command> and <command>menhir</command>, which are replacements for `yacc`.  We'll
explore these tools in the course of walking through the
implementation of a parser for the JSON serialization format that we
discussed in [xref](#handling-json-data).

Parsing is a broad and often intricate topic, and our purpose here is
not to teach all of the theoretical issues, but to provide a pragmatic
introduction of how to build a parser in OCaml.

<note>
<title>Menhir _vs_ <command>ocamlyacc</command></title>

Menhir is an alternative parser generator that is generally superior
to the venerable <command>ocamlyacc</command>, which dates back quite a few years.
Menhir is mostly compatible with <command>ocamlyacc</command> grammars, and so you can
usually just switch to Menhir and expect older code to work (with some
minor differences described in the Menhir manual).

The biggest advantage of Menhir is that its error messages are
generally more human-comprehensible, and the parsers that it generates
are fully reentrant and can be parameterized in OCaml modules more
easily.  We recommend that any new code you develop should use Menhir
instead of <command>ocamlyacc</command>.

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
Here's an example of a snippet of text that represents a JSON object
containing a string labeled `title`, and an array containing two
objects, each with a name and array of zip codes.

```frag
((typ json)(name parsing/example.json))
```

At a syntactic level, we can think of a JSON file as a series of
simple logical units, like curly braces, square brackets, commas,
colons, identifiers, numbers, and quoted strings.  Thus, we could
represent our JSON text as a sequence of tokens of the following type.

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

This representation is much richer than our token stream, capturing
the fact that JSON values can be nested inside each other, and that
JSON has a variety of value types, including numbers, strings, arrays,
and objects.  The job of the parser we'll write will be to convert a
token stream into a value of this AST type, as shown below for our
earlier JSON example.

```frag
((typ ocaml)(name parsing/parsed_example.ml))
```

## Defining a parser

A parser-specification file has suffix `.mly` and contains several
sections that are broken up by separator lines consisting of the
characters `%%` on a line by themselves.  The first section of the
file is for declarations, including token and type specifications,
precedence directives, and other output directives, and the second
section is for specifying the grammar of the language to be parsed.

We'll start by declaring the list of tokens.  A token is declared
using the syntax `%token <`_type_`>` _uid_, where the `<type>` is
optional, and _uid_ is a capitalized identifier.  For JSON, we need
tokens for numbers, strings, identifiers, and punctuation.

```frag
((typ ocaml)(name parsing/parser.mly)(part 0))
```

The `<`_type_`>` specifications mean that a token carries a value.
The `INT` token carries an integer value with it, `FLOAT` has a
`float` value, and `STRING` carries a `string` value.  The
remaining tokens, such as `TRUE`, `FALSE` or the punctuation, aren't
associated with any value and so we can omit the `<`_type_`>`
specification.

### Describing the grammar

The next thing we need to do is to specify the grammar of a JSON expression.
<command>menhir</command>, like many parser generators, expresses grammars as
_context free grammars_.  (More precisely, <command>menhir</command> supports
LR(1) grammars, but we will ignore that technical distinction here.) You can
think of a context-free grammar as a set of abstract names, called
_non-terminal symbols_, along with a collection of rules for transforming a
non-terminal symbol into a sequence of tokens and non-terminal symbols. A
sequence of tokens is parsable by a grammar if you can apply the grammar's
rules to produce a series of transformations, starting at a distinguished
_start symbol_, that produces the token sequence in question.

We'll start describing the JSON grammar by declaring the start symbol
to be the non-terminal symbol `prog`, and by declaring that when
parsed, a `prog` value should be converted into an OCaml value of type
`Json.value option`.  We then end the declaration section of the
parser with a `%%`.

```frag
((typ ocaml)(name parsing/parser.mly)(part 1))
```

Once that's in place, we can start specifying the productions.  In
<command>menhir</command>, productions are organized into _rules_, where each rule
lists all the possible productions for a given non-terminal.  Here,
for example, is the rule for `prog`.

```frag
((typ ocaml)(name parsing/parser.mly)(part 2))
```

The syntax for this is reminiscent of an OCaml match statement.  The
pipes separate the individual productions, and the code in
curly-braces is OCaml code that generates the OCaml value
corresponding to the production in question.  In the case of `prog`,
we have two cases: either there's an `EOF`, which means the text is
empty, and so there's no JSON value to read, and so we return the
OCaml value `None`; or we have an instance of the `value`
non-terminal, which corresponds to a well-formed JSON value, in which
case we wrap the corresponding `Json.value` in a `Some` tag.  Note
that in the `value` case, we wrote `v = value` to bind the OCaml value
that corresponds to to the variable `v`, which we can then use within
the curly-braces for that production.

Now let's consider a more complicated example, the rule for the
`value` symbol.

```frag
((typ ocaml)(name parsing/parser.mly)(part 3))
```

According to these rules, a JSON `value` is either:

- an object bracketed by curly braces,
- an array bracketed by square braces,
- a string, integer, float, bool, or null value.

In each of the productions, the OCaml code in curly-braces shows what
to transform the object in question to.  Note that we still have two
non-terminals whose definitions we depend on here, but have not yet
defined: `object_fields` and `array_values`.  We'll look at how
these are parsed next.

### Parsing sequences

The rule for `object_fields` is shown below, and is really just a thin
wrapper that reverses the list returned by the following rule for
`rev_object_fields`.  Note that the first production in
`rev_object_fields` has an empty left-hand side.  That's because what
we're matching on in this case is an empty sequence of tokens.  The
comment `/* empty */` is used to make this clear.  C-style comment
syntax is used within the body of a rule.

```frag
((typ ocaml)(name parsing/parser.mly)(part 4))
```

The rules are structured as they are because <command>menhir</command> generates
left-recursive parsers, which means that the constructed pushdown
automaton uses less stack space with left-recursive definitions.  The
following right-recursive rule accepts the same input, but during
parsing it requires linear stack space to read object field
definitions.

```frag
((typ ocaml)(name parsing/right_rec_rule.mly)(part 4))
```

Alternatively, we could keep the left-recursive definition and simply
construct the returned value in left-to-right order.  This is is even
less efficient, since the complexity of building thes list
incrementally in this way is quadratic in the length of the list.

```frag
((typ ocaml)(name parsing/quadratic_rule.mly)(part 4))
```

Assembling lists like this is a pretty common requirement in most
realistic grammars, and the above rules (while useful for illustrating
how parsing works) are rather verbose.  Menhir features an extended
standard library of built-in rules to simplify this handling.  These
rules are detailed in the Menhir manual, and include optional values,
pairs of values with optional separators, and lists of elements (also
with optional separators).

A version of the JSON grammar using these more succinct Menhir rules
is shown below.  Notice the use of `separated_list` to parse both JSON
objects and lists with one rule.

```frag
((typ ocaml)(name parsing/short_parser.mly)(part 1))
```

We can invoke <command>menhir</command> by using <command>corebuild</command> with the `-use-menhir`
flag.  This tells the build system to switch to using <command>menhir</command> instead
of <command>ocamlyacc</command> to handle files with the `.mly` suffix.

```frag
((typ console)(name parsing/build_short_parser.out))
```

## Defining a lexer

Now we can define a lexer, using <command>ocamllex</command>, to
convert our input text into a stream of tokens.  The specification of
the lexer is placed in a file with an `.mll` suffix.

### OCaml prelude 

Let's walk through the definition of a lexer section by section.  The
first section is on optional chunk of OCaml code that is bounded by a
pair of curly braces.

```frag
((typ ocaml)(name parsing/lexer.mll)(part 0))
```

This code is there to define utility functions used by later snippets
of OCaml code, and to set up the environment by opening useful
modules, and define an exception, `SyntaxError`.

We also define a utility function `next_line` for tracking the
location of tokens across line breaks.  The `Lexing` module defines a
`lexbuf` structure that holds the state of the lexer, including the
current location within the source file.  The `next_line` function
simply accesses the `lex_curr_p` field that holds the current location
and updates its line number.  

### Regular expressions

The next section of the lexing file is a collection of named regular
expressions.  These look syntactically like ordinary OCaml `let`
bindings, but really this is a specialized syntax for declaring
regular expressions.  Here's an example.

```frag
((typ ocaml)(name parsing/lexer.mll)(part 1))
```

The syntax here is something of a hybrid between OCaml syntax and
traditional regular expression syntax.  The `int` regular expression
specifies an optional leading `-`, followed by a digit from `1` to
`9`, followed by some number of digits from `0` to `9`.  The question
mark is used to indicate an optional component of a regular
expression, the square brackets are used to specify ranges, and the
`*` operator is used to indicate a (possibly empty) repetition.

Floating-point numbers are specified similarly, but we deal with
decimal points and exponents.  We make the expression easier to read
by building up a sequence of named regular expressions, rather than
creating one big and impenetrable expression.

```frag
((typ ocaml)(name parsing/lexer.mll)(part 2))
```

Finally, we define whitespace, newlines, identifiers, and hex
constants.

```frag
((typ ocaml)(name parsing/lexer.mll)(part 3))
```

### Lexing rules

The lexing rules are essentially functions that consume the data,
producing OCaml expressions that evaluate to tokens.  These OCaml
expressions can be quite complicated, using side-effects and invoking
other rules as part ofthe body of the rule.  Let's look at the `read`
rule for parsing a JSON expression.

```frag
((typ ocaml)(name parsing/lexer.mll)(part 4))
```

The rules are structured very similarly to pattern matches, except
that the variants are replaced by regular expressions on the left hand
side.  The right hand side clause is the parsed OCaml return value of
that rule.  The OCaml code for the rules has a parameter called
`lexbuf` that defines the input, including the position in the input
file, as well as the text that was matched by the regular expression.

The first `white { read lexbuf }` calls the lexer recursively.  That
is, it skips the input whitespace and returns the following token.
The action `newline { next_line lexbuf; read lexbuf }` is similar, but
we use it to advance the line number for the lexer using the utility
function that we defined at the top of the file.  Let's skip to the
third action.

```frag
((typ ocaml)(name parsing/lexer_int_fragment.mll))
```

This action specifies that when the input matches the `int` regular
expression, then the lexer should return the expression `INT
(int_of_string (Lexing.lexeme lexbuf))`.  The expression
`Lexing.lexeme lexbuf` returns the complete string matched by the
regular expression.  In this case, the string represents a number, so
we use the `int_of_string` function to convert it to a number.

There are actions for each different kind of token.  The string
expressions like `"true" { TRUE }` are used for keywords, and the
special characters have actions too, like `'{' { LEFT_BRACE }`.

Some of these patterns overlap.  For example, the regular expression
`"true"` is also matched by the `id` pattern.  <command>ocamllex</command> used the
following disambiguation when a prefix of the input is matched by more
than one pattern.

* The longest match always wins.  For example, the first input `trueX:
  167` matches the regular expression `"true"` for 4 characters, and
  it matches `id` for 5 characters.  The longer match wins, and the
  return value is `ID "trueX"`.

* If all matches have the same length, then the first action wins.  If
  the input were `true: 167`, then both `"true"` and `id` match the
  first 4 characters; `"true"` is first, so the return value is
  `TRUE`.
  
### Recursive rules

Unlike many other lexer generators, <command>ocamllex</command> allows the definition
of multiple lexers in the same file, and the definitions can be
recursive.  In this case, we use recursion to match string literals
using the following rule definition.

```frag
((typ ocaml)(name parsing/lexer.mll)(part 5))
```

This rule takes a `buf : Buffer.t` as an argument.  If we reach the
terminating double quote `"`, then we return the contents of the
buffer as a `STRING`.

The other cases are for handling the string contents.  The action `[^
'"' '\\']+ { ... }` matches normal input that does not contain a
double-quote or backslash.  The actions beginning with a backslash `\`
define what to do for escape sequences.  In each of these cases, the
final step includes a recursive call to the lexer.

That covers the lexer.  Next, we need to combine the lexer with the
parser to bring it all together.

<note>
<title>Handling Unicode</title>

We've glossed over an important detail here: parsing Unicode
characters to handle the full spectrum of the world's writing systems.
OCaml has several third-party solutions to handling Unicode, with
varying degrees of flexibility and complexity.

* [Camomile](http://camomile.sourceforge.net) supports the full
  spectrum of Unicode character types, conversion from around 200
  encodings, and collation and locale-sensitive case mappings.
* [Ulex](http://www.cduce.org/ulex) is a lexer generator for Unicode
  that can serve as a Unicode-aware replacement for <command>ocamllex</command>.
* [Uutf](http://erratique.ch/software/uutf) is a non-blocking
  streaming Unicode codec for OCaml, available as a standalone
  library.  It is accompanied by the
  [Uunf](http://erratique.ch/software/uunf) text normalization and
  [Uucd](http://erratique.ch/software/uucd) Unicode character database
  libraries. There is also a robust parser for
  [JSON](http://erratique.ch/software/jsonm) available that
  illustrates the use of Uutf in your own libraries.

All of these libraries are available via OPAM under their respective
names.

</note>

## Bringing it all together

For the final part, we need to compose the lexer and parser.  As we
saw in the type definition in `parser.mli`, the parsing function
expects a lexer of type `Lexing.lexbuf -> token`, and it also expects
a `lexbuf`.

```frag
((typ ocaml)(name parsing/prog.mli))
```

Before we start with the lexing, let's first define some functions to
handle parsing errors.  There are currently two errors: `Parser.Error`
and `Lexer.SyntaxError`.  A simple solution when encountering an error
is to print the error and give up, which we do below.

```frag
((typ ocaml)(name parsing-test/test.ml)(part 0))
```

The "give up on the first error" approach is easy to implement but
isn't very friendly.  In general, error handling can be pretty
intricate, and we won't discuss it here.  However, the Menhir parser
defines additional mechanisms you can use to try and recover from
errors. These are described in detail in its reference
[manual](http://gallium.inria.fr/~fpottier/menhir/).

The standard lexing library `Lexing` provides a function
`from_channel` to read the input from a channel.  The following
function describes the structure, where the `Lexing.from_channel`
function is used to construct a `lexbuf`, which is passed with the
lexing function `Lexer.read` to the `Parser.prog` function.
`Parsing.prog` returns `None` when it reaches end of file.  We define
a function `Json.output_value`, not shown here, to print a
`Json.value`.

```frag
((typ ocaml)(name parsing-test/test.ml)(part 1))
```

Here's a test input file we can use to test the code we just wrote.

```frag
((typ json)(name parsing-test/test1.json))
```

Now build and run the example using this file, and you you can see the
full parser in action.

```frag
((typ console)(name parsing-test/build_test.out))
```

With our simple error handling scheme, errors are fatal and cause the
program to terminate with a non-zero exit code.

```frag
((typ console)(name parsing-test/run_broken_test.out))
```

That wraps up our parsing tutorial.  As an aside, notice that the JSON
polymorphic variant type that we defined in this chapter is actually
structurally compatible with the Yojson representation explained
earlier in [xref](#handling-json-data).  That means that you can take
this parser and use it with the helper functions in Yojson to build
more sophisticated applications.
