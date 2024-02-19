
(rule
 (alias cal_add_days)
 (locks /global)
 (deps
  (source_tree ./correct/cal_add_days)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/cal_add_days")))

(alias
 (name runtest)
 (deps (alias cal_add_days)))

(rule
 (alias cal_add_interactive)
 (locks /global)
 (deps
  (source_tree ./correct/cal_add_interactive)
  (package core)
  (package ppx_jane))
 (action
  (system "dune build @all @runtest --root ./correct/cal_add_interactive")))

(alias
 (name runtest)
 (deps (alias cal_add_interactive)))

(rule
 (alias cal_add_interactive2)
 (locks /global)
 (deps
  (source_tree ./correct/cal_add_interactive2)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/cal_add_interactive2")))

(alias
 (name runtest)
 (deps (alias cal_add_interactive2)))

(rule
 (alias cal_add_sub_days)
 (locks /global)
 (deps
  (source_tree ./correct/cal_add_sub_days)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/cal_add_sub_days")))

(alias
 (name runtest)
 (deps (alias cal_add_sub_days)))

(rule
 (alias md5)
 (locks /global)
 (deps
  (source_tree ./correct/md5)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/md5")))

(alias
 (name runtest)
 (deps (alias md5)))

(rule
 (alias md5_as_filename)
 (locks /global)
 (deps
  (source_tree ./correct/md5_as_filename)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/md5_as_filename")))

(alias
 (name runtest)
 (deps (alias md5_as_filename)))

(rule
 (alias md5_let_syntax)
 (locks /global)
 (deps
  (source_tree ./correct/md5_let_syntax)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/md5_let_syntax")))

(alias
 (name runtest)
 (deps (alias md5_let_syntax)))

(rule
 (alias md5_let_syntax2)
 (locks /global)
 (deps
  (source_tree ./correct/md5_let_syntax2)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/md5_let_syntax2")))

(alias
 (name runtest)
 (deps (alias md5_let_syntax2)))

(rule
 (alias md5_multiarg)
 (locks /global)
 (deps
  (source_tree ./correct/md5_multiarg)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/md5_multiarg")))

(alias
 (name runtest)
 (deps (alias md5_multiarg)))

(rule
 (alias md5_sequence)
 (locks /global)
 (deps
  (source_tree ./correct/md5_sequence)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/md5_sequence")))

(alias
 (name runtest)
 (deps (alias md5_sequence)))

(rule
 (alias md5_succinct)
 (locks /global)
 (deps
  (source_tree ./correct/md5_succinct)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/md5_succinct")))

(alias
 (name runtest)
 (deps (alias md5_succinct)))

(rule
 (alias md5_with_custom_arg)
 (locks /global)
 (deps
  (source_tree ./correct/md5_with_custom_arg)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/md5_with_custom_arg")))

(alias
 (name runtest)
 (deps (alias md5_with_custom_arg)))

(rule
 (alias md5_with_default_file)
 (locks /global)
 (deps
  (source_tree ./correct/md5_with_default_file)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/md5_with_default_file")))

(alias
 (name runtest)
 (deps (alias md5_with_default_file)))

(rule
 (alias md5_with_flags)
 (locks /global)
 (deps
  (source_tree ./correct/md5_with_flags)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/md5_with_flags")))

(alias
 (name runtest)
 (deps (alias md5_with_flags)))

(rule
 (alias md5_with_optional_file)
 (locks /global)
 (deps
  (source_tree ./correct/md5_with_optional_file)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/md5_with_optional_file")))

(alias
 (name runtest)
 (deps (alias md5_with_optional_file)))
