
(rule
 (alias github)
 (deps
  (source_tree ./correct/github))
 (action
  (run dune build @all @runtest --root ./correct/github)))

(alias
 (name runtest)
 (deps (alias github)))

(rule
 (alias github_org_info)
 (deps
  (source_tree ./correct/github_org_info)
  (package core)
  (package yojson)
  (package atdgen)
  (package shell))
 (action
  (run dune build @all @runtest --root ./correct/github_org_info)))

(alias
 (name runtest)
 (deps (alias github_org_info)))

(rule
 (alias list_excerpt)
 (deps
  (source_tree ./correct/list_excerpt))
 (action
  (run dune build @all @runtest --root ./correct/list_excerpt)))

(alias
 (name runtest)
 (deps (alias list_excerpt)))

(rule
 (alias parse_book)
 (deps
  (source_tree ./correct/parse_book)
  (package core)
  (package yojson))
 (action
  (run dune build @all @runtest --root ./correct/parse_book)))

(alias
 (name runtest)
 (deps (alias parse_book)))

(rule
 (alias read_json)
 (deps
  (source_tree ./correct/read_json)
  (package core)
  (package yojson))
 (action
  (run dune build @all @runtest --root ./correct/read_json)))

(alias
 (name runtest)
 (deps (alias read_json)))

(rule
 (alias yojson_basic)
 (deps
  (source_tree ./correct/yojson_basic))
 (action
  (run dune build @all @runtest --root ./correct/yojson_basic)))

(alias
 (name runtest)
 (deps (alias yojson_basic)))

(rule
 (alias yojson_basic_simple)
 (deps
  (source_tree ./correct/yojson_basic_simple))
 (action
  (run dune build @all @runtest --root ./correct/yojson_basic_simple)))

(alias
 (name runtest)
 (deps (alias yojson_basic_simple)))

(rule
 (alias yojson_safe)
 (deps
  (source_tree ./correct/yojson_safe))
 (action
  (run dune build @all @runtest --root ./correct/yojson_safe)))

(alias
 (name runtest)
 (deps (alias yojson_safe)))
