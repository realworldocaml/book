The three context fields can be accessed in a rewriter, both from within an implementation file

  $ echo "let x = 0" > file.ml
  $ standalone_print_ctxt file.ml | egrep 'tool_name|input_name|file_path'
  tool_name: ppx_driver
  input_name: file.ml
  file_path: file.ml

and from within an interface file

  $ echo "val x : int" > file.mli
  $ standalone_print_ctxt file.mli | egrep 'tool_name|input_name|file_path'
  tool_name: ppx_driver
  input_name: file.mli
  file_path: file.mli

In most cases, the input name and the file path coincide. But there are some exceptions, such as
1. empty files

  $ touch empty_file.ml
  $ standalone_print_ctxt empty_file.ml | egrep 'input_name|file_path'
  input_name: empty_file.ml
  file_path: 

2. files with directives pointing to other files

  $ cat > directive.ml << EOF
  > # 1 "file.ml"
  > let y = 0
  > EOF
  $ standalone_print_ctxt directive.ml | egrep 'input_name|file_path'
  input_name: directive.ml
  file_path: file.ml

3. using `map_structure` (or `map_signature`)

  $ echo "let x = 0" | map_structure_print_ctxt | egrep 'input_name|file_path'
  input_name: _none_
  file_path: lexbuf_pos_fname
