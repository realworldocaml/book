;; comment_heavy_example.scm
((this is included)
 ; (this is commented out
 (this stays)
 #; (all of this is commented
     out (even though it crosses lines.))
  (and #| block delimiters #| which can be nested |#
     will comment out
    an arbitrary multi-line block))) |#
   now we're done
   ))
