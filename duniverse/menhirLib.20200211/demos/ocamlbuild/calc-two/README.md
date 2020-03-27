This tiny program reads arithmetic expressions from the standard input
channel. Each expression is expected to be complete when the current line
ends. Its value is then displayed on the standard output channel.

In this version, there is a single lexer, but there are two parsers, one for
expressions in algebraic (that is, infix) notation, one for expressions in
reverse Polish (that is, postfix) notation. One of the two parsers is selected
at runtime via a command line switch.

This demo illustrates how to build two parsers that share a single set of
tokens (see `tokens.mly`) and that share some productions (see `common.mly`).
