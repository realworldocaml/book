This tiny program reads arithmetic expressions from the standard input
channel. Each expression is expected to be complete when the current line
ends. Its value is then displayed on the standard output channel. This
code is adapted from ocamlyacc's documentation.

We compile the parser using Menhir's code back-end. For an example of using
Menhir's table back-end, see the demos `calc-incremental` and
`calc-inspection`.
