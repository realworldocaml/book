# GADTs

GADTs, which stands for generalized algebraic data types, is one of
OCaml's more mysterious features. The name itself doesn't do much to
tell you what it's for, and a lot of the material out there that
explains GADTs does so in a way that doesn't make it clear what the
practical use-cases are.

This chapter is meant to fill that gap, to serve as a practical
introduction to GADTs, and to show you why you should care, and along
the way, to demystify them.

At their core, GADTs are a more expressive version of the now familiar
variant types that were introduced in
[Variants](variants.html#variants){data-type=xref}.  The big
difference with GADTs is that they provide you with more precision at
the type level.  In particular, they allow your program to learn more
type information about the values it's manipulating through the
process of pattern matching.

To make this more concrete, let's walk through a concrete example of
where you might want more type precision, and show how GADTs can
provide it.

## A simple example
