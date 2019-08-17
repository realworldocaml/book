(this is a list)

(this is another list and (this is a nested list))

(
  "\
    This is a multi-line \
    string with embedded

    newlines."

  "This string contains decimal \255, hex \xff codes, \
   and other \\ \n escapes."

  A# # ## #x|
)

; Line comment

#; (
  S-expression comment
)

#| #| Nested |# block comment "|#" |#

#| "" |#
#| ""|#
#|"" |#
#|""|#

#| "asdf" "asdf" |#

(something #| ; |# () "something else")

