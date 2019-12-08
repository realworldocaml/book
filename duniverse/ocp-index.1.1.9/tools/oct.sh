oct() {
  ocp-index complete --format "%k %p: %t
%d" "$@"
}

_oct() {
    COMPREPLY=( $(ocp-index complete --color=never --format "%q" "${COMP_WORDS[COMP_CWORD]}") )
}

complete -F _oct oct
