" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal expandtab
setlocal indentkeys+=0=and,0=class,0=constraint,0=done,0=else,0=end,0=exception,0=external,0=if,0=in,0=include,0=inherit,0=initializer,0=let,0=method,0=open,0=then,0=type,0=val,0=with,0;;,0>\],0\|\],0>},0\|,0},0\],0)
setlocal nolisp
setlocal nosmartindent
setlocal indentexpr=GetOcpIndent(v:lnum)

" Comment formatting
if !exists("no_ocaml_comments")
 if (has("comments"))
   setlocal comments=sr:(*,mb:*,ex:*)
   setlocal fo+=cqor
 endif
endif

" Only define the function once.
if exists("*GetOcpIndent")
 finish
endif

" Indents are cached for the current buffer; they are only re-used when
" indenting lines in sequence and the buffer was unchanged.
let s:indents = []
let s:buffer = -1
let s:tick = -1
let s:lnum = -1

function! GetOcpIndent(lnum)
  if s:buffer == bufnr('') && s:tick == b:changedtick && s:lnum < a:lnum && match(getline(s:lnum + 1, a:lnum - 1),'.') == -1
    " Only use cache if there are only blank lines in-between
    call remove(s:indents, 0, a:lnum - s:lnum - 1)
  else
    " Compute indentation from current line on
    let cmdline = "ocp-indent --numeric --indent-empty --lines " . a:lnum . '-'
    let s:indents = split(system(cmdline, getline('1','$')))
    let s:buffer = bufnr('')
    let s:tick = b:changedtick
  endif
  let s:lnum = a:lnum

  return s:indents[0]
endfunction
