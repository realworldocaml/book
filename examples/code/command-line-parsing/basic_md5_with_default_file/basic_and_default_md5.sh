### build
  $ jbuilder build basic_md5_with_optional_file.exe
  $ jbuilder build basic_md5_with_default_file.exe
### run
  $ cat /etc/passwd | ./_build/default/basic_md5_with_optional_file.exe
  27bf1f2dbadd4cae84f1da4dfe8b5cb3
  $ cat /etc/passwd | ./_build/default/basic_md5_with_default_file.exe
  27bf1f2dbadd4cae84f1da4dfe8b5cb3
