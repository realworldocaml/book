The cookie flag is taken into account, both by the main standalone

  $ echo "[@@@print_cookie_x]" > impl.ml
  $ ./print_cookie_driver.exe -cookie x=1 impl.ml
  Value of cookie x: 1

...and by the `-as-ppx` standalone

  $ ocaml -ppx './print_cookie_driver.exe --as-ppx -cookie x=1' impl.ml
  Value of cookie x: 1
