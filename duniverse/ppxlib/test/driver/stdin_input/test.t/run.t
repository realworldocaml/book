The driver can read from stdin. Both when the input is source code...


  $ ../identity_driver.exe -impl - << EOF
  > let a = 1
  > EOF
  let a = 1

...and when the input is a binary AST.

  $ cat binary_ast | ../identity_driver.exe -impl -
  let b = 2
