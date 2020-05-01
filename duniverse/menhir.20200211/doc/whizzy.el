(whizzy-add-configuration
 ".*\.\\(tex\\|sty\\)"
 '((whizzy-master . "main.tex"))
)
(whizzy-add-configuration
 "main\.tex" 
 '((whizzy . "section -advi \"advi -geometry 1270x1024 -fullwidth -html Start-Document\" -dvicopy dvicopy" ))
)
