# Differences between the old and new rule syntax

This presentation of the new rule syntax is meant to be useful to a reader who
is familiar with the old rule syntax. For a direct, self-contained
presentation of the new rule syntax, please consult Menhir's manual.

## Rules

A rule begins with `let`.

|                            |         |                          |
|----------------------------|---------|--------------------------|
| `foo: ...`                 | becomes | `let foo := ...`         |
| `%inline foo: ...`         | becomes | `let foo == ...`         |
| `%public foo: ...`         | becomes | `%public let foo := ...` |
| `%public %inline foo: ...` | becomes | `%public let foo == ...` |

A rule **cannot** be terminated with a semicolon.

## Sums

Productions are separated with `|`. A leading `|` is permitted, and ignored.
For instance, the rule `let a := | A; { () }` has only one production, which
recognizes the symbol `A`. In contrast with the old syntax,two productions
**cannot** share a semantic action.

## Sequences

In a sequence `p1 = e1; e2`, the semicolon is **mandatory**.

The pattern `p1` binds the semantic values produced by `e1`.

|                 |       |                               |
|-----------------|-------|-------------------------------|
| `x = foo;`      | means | the same as in the old syntax |
| `foo;`          | means | `_ = foo;`                    |
| `~ = foo;`      | means | `foo = foo;`                  |
| `(x, y) = foo;` | means | `_xy = foo;` where the following semantic action is wrapped in `let (x, y) = _xy in ...` |

In contrast with the old syntax, when a sequence ends with a semantic action,
the semicolon that precedes the semantic action is still mandatory. For
instance, in `let literal := i = INT; { i }`, the semicolon is required.

In contrast with the old syntax, **a sequence need not end with a semantic
action**. A sequence can also end with a symbol, whose semantic value is
then implicitly returned. For instance,

|                                |       |                  |
|--------------------------------|-------|------------------|
| `foo` at the end of a sequence | means | `x = foo; { x }` |

This implies that **it becomes easy to define a symbol as a synonym for
another symbol** or for a complex expression. For instance,

|                                 |         |                   |
|---------------------------------|---------|-------------------|
| `%inline foo: xs = bar* { xs }` | becomes | `let foo == bar*` |

## Semantic actions

Traditional semantic actions, such as `{ (x, y) }`, remain available.

In addition, so-called **point-free semantic actions** appear. They take the
form of a single OCaml identifier between angle brackets. This identifier,
which should denote a function or a data constructor, is implicitly
**applied** to a tuple of the variables that have been bound earlier in the
sequence. If this identifier is omitted, the identity function is
assumed. Thus,

|                                                     |       |                                    |
|-----------------------------------------------------|-------|--------------------------------------------------------|
| `let pair(x, y) := ~ = x; ~ = y; <Pair>`            | means | `let pair(x, y) := x = x; y = y; { Pair (x, y) }` |
| `let pair(x, y) := ~ = x; ~ = y; <>`                | means | `let pair(x, y) := x = x; y = y; { (x, y) }` |
| `let parenthesized(x) := LPAREN; ~ = x; RPAREN; <>` | means | `let parenthesized(x) := LPAREN; x = x; RPAREN; { x }` |

This often removes the need to invent names for semantic values.

`$1`, `$2`, etc. are forbidden. Semantic values must be named.
A semantic value can be named either explicitly or via a `~` pattern.
