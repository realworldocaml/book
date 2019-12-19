open Mirage_kv

let key = Alcotest.testable Key.pp Key.equal

let path_v () =
  let check s e =  Alcotest.(check string) s e Key.(to_string @@ v s) in
  check "/foo/bar" "/foo/bar";
  check "/foo"     "/foo";
  check "/"        "/";
  check "foo/bar"  "/foo/bar";
  check ""         "/"

let path_add () =
  let check p b exp =
    let f = p ^ "/" ^ b in
    let vp = Key.v p in
    Alcotest.(check string) f exp Key.(to_string @@ vp / b);
    Alcotest.(check key)    f vp  Key.(parent @@ vp / b);
    Alcotest.(check string) f b   Key.(basename @@ vp / b)
  in
  let check_exn p b =
    try
      let _ = Key.(v p / b) in
      Alcotest.failf "%s is not a valid segment, should fail" b
    with Failure _ -> ()
  in
  check ""         "bar"  "/bar";
  check "/"        "foo"  "/foo";
  check "/foo"     "bar"  "/foo/bar";
  check "/foo/bar" "toto" "/foo/bar/toto";
  check_exn "" "foo/bar"

let path_append () =
  let check x y =
    let f = x ^ "/" ^ y in
    let vf = Key.v f in
    Alcotest.(check key)    f vf                Key.(v x // v y);
    Alcotest.(check string) x Key.(basename vf) Key.(basename @@ v y)
  in
  check ""         "/foo/bar";
  check "/foo"     "bar";
  check "/foo/bar" "/toto/foox/ko"

let () = Alcotest.run "mirage-kv" [
    "path", [
      "Path.v"      , `Quick, path_v;
      "Path.add_seg", `Quick, path_add;
      "Path.append" , `Quick, path_append;
    ]
  ]
