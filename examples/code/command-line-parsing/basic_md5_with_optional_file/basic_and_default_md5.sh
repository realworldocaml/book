### build
  $ jbuilder build basic_md5_with_optional_file.exe
      ocamlopt basic_md5_with_optional_file.exe
  ld: warning: directory not found for option '-L/opt/local/lib'
  $ jbuilder build basic_md5_with_default_file.exe
      ocamlopt basic_md5_with_default_file.exe
  ld: warning: directory not found for option '-L/opt/local/lib'
### run
  $ cat /etc/passwd | ./_build/default/basic_md5_with_optional_file.exe
  b88d621596b7e61337e832f7841066a9
  $ cat /etc/passwd | ./_build/default/basic_md5_with_default_file.exe
  b88d621596b7e61337e832f7841066a9
