#!/bin/sh -ue

# Checks that the required emacs packages are installed and gives direction
# for emacs configuration

eldir=

check_eldir () ( [ -e "$eldir/ocp-index.el" ] )

if [ $# -ge 1 ]; then
    eldir=$1; shift
else
    eldir=/usr/local/share/emacs/site-lisp
    check_eldir || eldir=/usr/share/emacs/site-lisp
    check_eldir || {
        $(which opam >/dev/null) &&
        eldir=$(opam config var share)/emacs/site-lisp
    }
fi

[ $# -eq 0 ] || ( echo "USAGE: $0 [ocp-index.el install dir]" >&2; exit 1 )

if ! check_eldir; then
    echo "Could not find ocp-index.el, please specify the directory where" >&2
    echo "it was installed." >&2
    exit 2
fi

echo "== Emacs configuration =="
echo "To setup tuareg-mode (or caml-mode) to use ocp-index for completion,"
echo "please add the following to your .emacs :"
echo
if [ "${eldir#/usr/}" = "$eldir" ]; then
    echo "  (add-to-list 'load-path \"$eldir\")"
fi
echo "  (require 'ocp-index)"
if ! emacs --batch --eval "(require 'auto-complete)" 2>/dev/null; then
    echo
    echo "WARNING: you do not appear to have 'auto-complete.el' installed,"
    echo "         and it is required for completion in emacs. Check your"
    echo "         distribution for a package named 'auto-complete-el' or"
    echo "         similar."
fi
