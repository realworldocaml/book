# Parser Construction With Menhir: A Couple Appetizers

This post is a shameless advertisement for Menhir,
a parser generator for OCaml.
It illustrates Menhir's new input syntax,
which was introduced on November 13, 2018.
The code fragments shown below are excerpts
of valid `.mly` files.

## Ingredients

Suppose I would like to parse and evaluate our good old friends, the
arithmetic expressions. For instance, the string `"(3 + 4) * 5 - 9"`
should be accepted and evaluated to the value `26`.

I assume that I have a lexical analyzer that can chop up this string into a
stream of basic tokens, or terminal symbols. My alphabet of terminal
is the following:

```
%token<int> INT
%token PLUS MINUS TIMES DIV LPAREN RPAREN EOL
```

Based on this alphabet, I wish to define the syntax of (and obtain a parser for)
arithmetic expressions. This exercise may seem old and tired, but let me try
and see if I can add some new spice and style to it. In fact, let me do it
twice, in two slightly different ways.

So, how would you like your arithmetic expressions cooked?

## First Flavor: Hot Off the Oven, With On-The-Fly Evaluation

In this first demo, I wish to evaluate an arithmetic expression, that is, find
out which integer value it represents. Thus, I am eventually interested in
just an integer result.

```
%start<int> main
%%
```

I wish to recognize an expression followed with an end-of-line symbol:

```
let main :=
  ~ = expr; EOL; <>
```

Here, `~ = expr` is a **pun**, a shorthand for `expr = expr`. It can be read
as follows: "read an expression; evaluate it; let the variable `expr` stand
for its value".

`<>` is a **point-free semantic action**.
In general, it is a shorthand for a semantic action that builds a tuple of
the variables that have been bound earlier in the sequence.
Thus, in this case, it is a shorthand for the semantic action `{ expr }`.

It is now time to define `expr` and thereby describe the syntax and the
meaning of arithmetic expressions. To do this in a nonambiguous manner, one of
several traditional approaches is to stratify the syntax in several levels,
namely additive expressions, multiplicative expressions, and atomic
expressions. These levels are also traditionally known as *expressions*,
*terms*, and *factors*.

The topmost level is the level of additive expressions.
In other words, an expression is just an additive expression:

```
let expr ==
  additive_expr
```

This definition has no runtime cost: it makes `expr` a synonym for
`additive_expr`. In traditional Menhir speak, `expr` is an `%inline`
nonterminal symbol.

This definition introduces a useful level of indirection: if in the future I
decide to introduce a new level in the syntax of expressions, all I have to do
is update the definition of `expr`; the places where `expr` is used do not
need to be updated. In other words, the fact that "an expression is just an
additive expression" is an implementation detail, and should not be revealed.

An additive expression is a nonempty, left-associative list of multiplicative
expressions, separated with additive operators:

```
let additive_expr ==
  fold_left(additive_op, multiplicative_expr)
```

What does this mean? Well, quite obviously, the additive operators are `PLUS`
and `MINUS`, which respectively denote addition or subtraction:

```
let additive_op ==
  | PLUS;  { ( + ) }
  | MINUS; { ( - ) }
```

Furthermore, a nonempty list of elements `elem` separated by operators `op`
is: either a single element; or a (smaller) such list, followed with an
operator, followed with an element. In the second case, the operator must
be applied to the sum of the left-hand list and to the right-hand element:

```
let fold_left(op, elem) :=
  | elem
  | sum = fold_left(op, elem); ~ = op; ~ = elem; { op sum elem }
```

This is a **parameterized definition**. Because this definition is recursive,
it cannot be macro-expanded away: we cannot use `==` and must instead use
`:=`.

So much for additive expressions. This scheme can now be reproduced, one level
down: a multiplicative expression is a nonempty, left-associative list of
atomic expressions, separated with multiplicative operators.

```
let multiplicative_expr ==
  fold_left(multiplicative_op, atomic_expr)

let multiplicative_op ==
  | TIMES; { ( * ) }
  | DIV;   { ( / ) }
```

There remains to define atomic expressions. In this demo, I wish to allow
the use of `MINUS` as a unary operator.
Thus, an atomic expression shall be one of the following:
an integer literal;
an arbitrary expression between parentheses;
or an application of a unary operator to an atomic expression.

```
let atomic_expr :=
  | INT
  | delimited(LPAREN, expr, RPAREN)
  | app(unary_op, atomic_expr)
```

There is just one unary operator, `MINUS`, whose meaning is integer negation:

```
let unary_op ==
  | MINUS; { (~- ) }
```

There remains to explain `delimited(left, x, right)` and `app(f, x)`. My main
motivation for introducing these auxiliary parameterized symbols is to make
the definition of `atomic_expr` prettier.

`delimited(left, x, right)` is in fact part of Menhir's standard library,
where it is defined as follows:

```
%public let delimited(left, x, right) ==
  left; ~ = x; right; <>
  ```

`app(f, x)` recognizes the sequence `f; x`. Its value is the application
of the value of `f` to the value of `x`. It is defined as follows:

```
let app(f, x) ==
  ~ = f; ~ = x; { f x }
```

<!-- One could in fact argue that `app(f, x)` is a useful abstraction, as it
encodes the manner in which a unary operator is applied to a subexpression;
here, by prefix juxtaposition. If I wished to switch to a different syntax, I
could do so just by altering the definition of `app`. This would be
particularly useful if there were several use sites of `app` in the grammar.
-->

At this point, the arithmetic-expression parser-and-evaluator is complete.

Menhir accepts it without complaining, which means that this
grammar is in the class LR(1), therefore is **unambiguous**. From it,
Menhir generates an LR(1) parser, a deterministic pushdown automaton, whose
**performance is predictable**:
provided each semantic action
takes constant time, its time complexity is linear in the size of the input.
Compared with other parsing techniques, guaranteed unambiguity and efficiency
are two important strengths of LR(1) parsers.

## Second Flavor: As An Abstract-Syntax-and-Location Millefeuille

Let me now be more ambitious. Instead of evaluating arithmetic expressions on
the fly, let me build Abstract Syntax Trees. This opens the door to all kinds
of symbolic computation: compilation down to native code, simplification,
automatic differentiation, and so on.

In a separate file, say `syntax.ml`, I define the types of the ASTs that I
wish to build:

```
type unop =
  | OpNeg

type binop =
  | OpPlus | OpMinus | OpTimes | OpDiv

type 'a located =
  { loc: Lexing.position * Lexing.position; value: 'a }

type expr =
  raw_expr located

and raw_expr =
| ELiteral of int
| EUnOp of unop * expr
| EBinOp of expr * binop * expr
```

The types `unop` and `binop` are simple enumerated types.

In the definition of the type `raw_expr`, one recognizes three kinds of
expressions: integer literals, applications of unary operators, and
applications of binary operators. There is no data constructor for expressions
in parentheses: although parentheses are a necessary feature of the concrete
syntax, there is no need to record them in the abstract syntax.

In an abstract syntax tree, I would like every subtree to be annotated with
its location in the input text. This would be important, in a real-world
programming language implementation, in order to produce error messages that carry
a source code location.

To achieve this, I use a traditional technique: I define two types, `expr` and
`raw_expr`, in a mutually recursive manner. An expression is a raw expression
annotated with a location (a pair of a start position and an end position). A
raw expression is an integer literal, an application of a unary operator to an
expression, or an application of a binary operator to two expressions. Thus,
like a cake, an abstract syntax tree has layered structure: one layer of
location information, one layer of structural information, one layer of
location information, one layer of structural information, and so on.

Let me now move on to the description of the parser. This time, I am
eventually interested in producing an abstract syntax tree.

```
%start<Syntax.expr> main
%{ open Syntax %}
%%
```

The first few definitions are unchanged:

```
let main :=
  ~ = expr; EOL; <>

let expr ==
  additive_expr
```

This time around, I won't use a generic definition along the lines of
`fold_left(op, elem)`. It can be done, though; this is left as an exercise for
the reader! Here is a direct definition of additive expressions:

```
let additive_expr :=
  | multiplicative_expr
  | located(
      ~ = additive_expr; ~ = additive_op; ~ = multiplicative_expr; <EBinOp>
    )

let additive_op ==
  | PLUS;  { OpPlus }
  | MINUS; { OpMinus }
```

In short, an additive expression is either a multiplicative expression, or an
additive expression followed with an additive operator followed with a
multiplicative expression.

In the second production, I use three `~` patterns in order to avoid the chore
of naming the three semantic values. I again use **a point-free semantic
action**: `<EBinOp>` means that the data constructor `EBinOp` should be
applied to a tuple of the three semantic values. At the cost of greater
verbosity, one could equivalently write `e1 = additive_expr; op = additive_op;
e2 = multiplicative_expr; { EBinOp (e1, op, e2) }`.

Now, `EBinOp(e1, op, e2)` has type `raw_expr`, but I would like the semantic
value of the nonterminal symbol `additive_expr` to have type `expr`.
Therefore, I need to wrap this semantic value in a record of type `raw_expr
located`. This can be done in a lightweight and elegant manner just by
wrapping the second production with `located(...)`, where the parameterized
nonterminal symbol `located(x)` is defined once and for all as follows:

```
let located(x) ==
  ~ = x; { { loc = $loc; value = x } }
```

`located(x)` recognizes the same input as `x`,
and wraps the semantic value of type `'a` produced by `x`
in a record of type `'a located`.

One level down, multiplicative expressions
are described via the same pattern:

```
let multiplicative_expr :=
  | atomic_expr
  | located(
      ~ = multiplicative_expr; ~ = multiplicative_op; ~ = atomic_expr; <EBinOp>
    )

let multiplicative_op ==
  | TIMES; { OpTimes }
  | DIV;   { OpDiv }
```

Finally, as earlier,
an atomic expression is one of:
an expression between parentheses;
an integer literal;
an application of a unary operator to an atomic expression.

```
let atomic_expr :=
  | LPAREN; ~ = expr; RPAREN; <>
  | located(
    | ~ = INT; <ELiteral>
    | ~ = unary_op; ~ = atomic_expr; <EUnOp>
    )

let unary_op ==
  | MINUS; { OpNeg }
```

Only the last two cases in the definition of `atomic_expr` are wrapped in
 `located(...)`: in the first case, this is not necessary, as the expression
 already carries a location. Things are formulated in such a way that the
 computed locations are tight: the source code range associated with a
 parenthesized subexpression does not include the parentheses. One could of
 course easily adopt the reverse convention: this is left as another exercise
 for the reader!

## Behind The Scenes, Or: In The Kitchen

If one expands away all symbols introduced by `==`,
expands away all parameterized symbols,
and strips away all semantic actions,
one finds that the two descriptions presented above
represent the same LR(1) grammar,
therefore give rise to the same deterministic pushdown automaton.

This bare-bones grammar is printed by `menhir --only-preprocess-u`,
a useful inspection tool. It is printed in Menhir's traditional
syntax. Once manually translated to the modern syntax used in this
article, it is as follows:

```
%token DIV EOL INT LPAREN MINUS PLUS RPAREN TIMES
%start<unit> main

%%

let main :=
  additive_expr; EOL

let additive_expr :=
| multiplicative_expr
| additive_expr; PLUS; multiplicative_expr
| additive_expr; MINUS; multiplicative_expr

let multiplicative_expr :=
| atomic_expr
| multiplicative_expr; TIMES; atomic_expr
| multiplicative_expr; DIV; atomic_expr

let atomic_expr :=
| INT
| LPAREN; additive_expr; RPAREN
| MINUS; atomic_expr
```

## Spilling the Sauce: A Syntax Error

Suppose my fingers slip, and I make a syntax error in
my grammar description:

```
let main :=
  ~ = expr; EOL; <>;
```

Not to worry. Menhir's parser for `.mly` files is
a Menhir-generated parser, and produces reasonable
syntax error messages. Here, the semicolon
that follows the semantic action is invalid:

```
File "parser.mly", line 30, characters 19-20:
Error: syntax error after '<>' and before ';'.
At this point, one of the following is expected:
a bar '|' followed with an expression, or
another rule.
```

Yes, **LR(1) parsers can produce good syntax error messages**.

## References

The full source code of
[the first demo](https://gitlab.inria.fr/fpottier/menhir/blob/master/demos/calc-new-syntax-dune/parser.mly)
and
[the second demo](https://gitlab.inria.fr/fpottier/menhir/blob/master/demos/calc-ast-dune/parser.mly)
is available online.

[A summary of the changes](https://gitlab.inria.fr/fpottier/menhir/blob/master/doc/new-rule-syntax-summary.md)
between the old and new syntaxes
is also available.

The syntax of Menhir is of course also documented in the
[reference manual](http://gallium.inria.fr/~fpottier/menhir/manual.html#sec5).
