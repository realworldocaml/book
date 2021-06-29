Logical OR is expanded so that the operands can be instrumented individually.

  $ bash ../test.sh <<'EOF'
  > let _ = true || false
  > let _ = true or false
  > EOF
  let _ =
    if true then (
      ___bisect_visit___ 0;
      true)
    else if false then (
      ___bisect_visit___ 1;
      true)
    else false
  
  let _ =
    if true then (
      ___bisect_visit___ 2;
      true)
    else if false then (
      ___bisect_visit___ 3;
      true)
    else false


If the right operand is also a logical OR, the instrumentation is "associative"
rather than nested.

  $ bash ../test.sh <<'EOF'
  > let _ = true || true || false
  > let _ = true or true or false
  > EOF
  let _ =
    if true then (
      ___bisect_visit___ 0;
      true)
    else if true then (
      ___bisect_visit___ 1;
      true)
    else if false then (
      ___bisect_visit___ 2;
      true)
    else false
  
  let _ =
    if true then (
      ___bisect_visit___ 3;
      true)
    else if true then (
      ___bisect_visit___ 4;
      true)
    else if false then (
      ___bisect_visit___ 5;
      true)
    else false


Recursive instrumentation of subexpressions.

  $ bash ../test.sh <<'EOF'
  > let _ = (bool_of_string "true") || (bool_of_string "false")
  > let _ = (bool_of_string "true") or (bool_of_string "false")
  > EOF
  let _ =
    if ___bisect_post_visit___ 3 (bool_of_string "true") then (
      ___bisect_visit___ 0;
      true)
    else if ___bisect_post_visit___ 2 (bool_of_string "false") then (
      ___bisect_visit___ 1;
      true)
    else false
  
  let _ =
    if ___bisect_post_visit___ 7 (bool_of_string "true") then (
      ___bisect_visit___ 4;
      true)
    else if ___bisect_post_visit___ 6 (bool_of_string "false") then (
      ___bisect_visit___ 5;
      true)
    else false


Function calls on the right in tail position remain in tail position. Any
would-be surrounding instrumentation is suppressed.

  $ bash ../test.sh <<'EOF'
  > let f _ = (bool_of_string "true") || (bool_of_string "false")
  > let g _ =
  >   (bool_of_string "true") or ((bool_of_string [@ocaml.tailcall]) "false")
  > EOF
  let f _ =
    ___bisect_visit___ 2;
    if ___bisect_post_visit___ 1 (bool_of_string "true") then (
      ___bisect_visit___ 0;
      true)
    else bool_of_string "false"
  
  let g _ =
    ___bisect_visit___ 5;
    if ___bisect_post_visit___ 4 (bool_of_string "true") then (
      ___bisect_visit___ 3;
      true)
    else (bool_of_string [@ocaml.tailcall]) "false"


Surrounding instrumentation is still generated when the second function is a
well-known trivial function.

  $ bash ../test.sh <<'EOF'
  > let f _ = (bool_of_string "true") || (true <> false)
  > EOF
  let f _ =
    ___bisect_visit___ 3;
    if ___bisect_post_visit___ 2 (bool_of_string "true") then (
      ___bisect_visit___ 0;
      true)
    else if true <> false then (
      ___bisect_visit___ 1;
      true)
    else false
