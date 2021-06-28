# Rope

A rope is a standard data structure that represents a single string as
a tree of strings, allowing concatenation to do no work up front.

That is, a string formed by many `Rope` concatenations followed by a `to_string` needs
only copy each input to the output once, whereas a string expression looking like
`a ^ b ^ c ^ ... ^ z` must create an intermediate string for every
concatenation, and will copy the original data into and out of short-lived
temporary strings many times.

On the other hand, because `String.concat [ s1; s2; s3; ... ]` allocates a single
string and copies the inputs into it, `Rope` is no improvement over that usage.
`Rope` becomes useful when the construction of the sequence of strings is more
complex -- a good example is prettyprinting an expression language, where you need to
parenthesize subexpressions (appending a short string at both ends) and handle infix
binary operators (appending two long strings both made up of many parts, with a short
string in between).

Any operations that would produce a `Rope` longer than `String.max_length` raise
instead. They are not marked with `_exn` on their names since (at least on 64-bit)
this number is far in excess of the size of your memory, so isn't likely to come up in
practice.

A more fully-featured implementation of the same idea is available in the `zed`
library.
