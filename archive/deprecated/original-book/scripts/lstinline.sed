s,\\hbox{\(\\lstinline/[^/]*/\)},\1,g
s,\\lstinline/[^/]*/,\\hbox{\\codestart&\\codeend},g
s,\\codestart\\hbox{\\codestart,\\codestart,g
s,\\codeend}\\codeend,\\codeend,g
s,^\\begin{ocaml},\\codestart&,
s,\\end{ocaml}$,&\\codeend,
