This tiny program reads arithmetic expressions from the standard input
channel. Each expression is expected to be complete when the current line
ends. Its value is then displayed on the standard output channel. This
code is adapted from ocamlyacc's documentation.

The difference between this demo and `calc` is that in this case, we make use
of token aliases to make the grammar look slightly nicer.
