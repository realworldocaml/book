s,\\lstinline+\([^+]*\)+,`\1`,g
s,\\lstinline/\([^/]*\)/,`\1`,g
s,\\lstinline!\([^!]*\)!,`\1`,g
s,\\verb!\([^!]*\)!,`\1`,g
s,\\hbox{\([^}]*\)},\1,g
s,.*begin.ocaml.*,```ocaml,
s,.*end.ocaml.*,```,
