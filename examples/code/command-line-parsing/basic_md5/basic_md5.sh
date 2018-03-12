### build
  $ jbuilder build basic_md5.exe
      ocamlopt basic_md5.exe
  ld: warning: directory not found for option '-L/opt/local/lib'
### run
  $ ./_build/default/basic_md5.exe ./_build/default/basic_md5.exe
  755e1de2f36cfffd870269161df6a3f2
### get help
  $ ./_build/default/basic_md5.exe
  Error parsing command line.  Run with -help for usage information.
  missing anonymous argument: FILENAME
@@ exit 1
### get version
  $ ./_build/default/basic_md5.exe -version
  1.0
  $ ./_build/default/basic_md5.exe -build-info
  RWO
