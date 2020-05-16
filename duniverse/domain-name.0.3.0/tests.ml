let n_of_s = Domain_name.of_string_exn

let host =
  let module M = struct
    type t = [ `host ] Domain_name.t
    let pp = Domain_name.pp
    let equal = Domain_name.equal ~case_sensitive:false
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let service =
  let module M = struct
    type t = [ `service ] Domain_name.t
    let pp = Domain_name.pp
    let equal = Domain_name.equal ~case_sensitive:false
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let p_msg =
  let module M = struct
    type t = [ `Msg of string ]
    let pp ppf (`Msg m) = Fmt.string ppf m
    let equal (`Msg _) (`Msg _) = true
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let is_domain x = match Domain_name.of_string x with
  | Ok _ -> true | Error _ -> false

let is_host x = match Domain_name.host x with
  | Ok _ -> true | Error _ -> false

let is_service x = match Domain_name.service x with
  | Ok _ -> true | Error _ -> false

let longest_label = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijk"
let longest_prefix =
  let d a b = a ^ "." ^ b in
  d longest_label (d longest_label longest_label)

let basic_preds () =
  Alcotest.(check bool "root is_hostname" true (is_host Domain_name.root)) ;
  Alcotest.(check bool "foo is a hostname" true (is_host (n_of_s "foo"))) ;
  Alcotest.(check bool ".foo is no domain" false (is_domain ".foo")) ;
  Alcotest.(check bool "bar is a hostname" true (is_host (n_of_s "bar"))) ;
  Alcotest.(check bool "foo.bar is a hostname" true (is_host (n_of_s "foo.bar"))) ;
  Alcotest.(check bool "longest label is domain name" true (is_domain longest_label)) ;
  Alcotest.(check bool "longest label + a is not domain name" false (is_domain (longest_label ^ "a"))) ;
  Alcotest.(check bool "ll.ll.ll.ll[:-2] is domain name" true
              (is_domain (longest_prefix ^ "." ^ (String.sub longest_label 0 61)))) ;
  Alcotest.(check bool "ll.ll.ll.ll[:-1] is not a domain name" false
              (is_domain (longest_prefix ^ "." ^ (String.sub longest_label 0 62)))) ;
  Alcotest.(check bool "foo._bar is not a hostname" false (is_host (n_of_s "foo._bar"))) ;
  Alcotest.(check bool "2foo.bar is a hostname" true (is_host (n_of_s "2foo.bar"))) ;
  Alcotest.(check bool "f2.bar is a hostname" true (is_host (n_of_s "f2.bar"))) ;
  Alcotest.(check bool "-f2.bar is not a hostname" false (is_host (n_of_s "-f2.bar"))) ;
  Alcotest.(check bool "f2.23 is not a hostname" false (is_host (n_of_s "f2.23"))) ;
  Alcotest.(check bool "42.23b is a hostname" true (is_host (n_of_s "42.23b"))) ;
  Alcotest.(check bool "'bar.foo is not a hostname" false (is_host (n_of_s "'bar.foo"))) ;
  Alcotest.(check bool "root is no service" false (is_service Domain_name.root)) ;
  Alcotest.(check bool "_tcp.foo is no service" false
              (is_service (n_of_s "_tcp.foo"))) ;
  Alcotest.(check bool "_._tcp.foo is no service" false
              (is_service (n_of_s "_._tcp.foo"))) ;
  Alcotest.(check bool "foo._tcp.foo is no service" false
              (is_service (n_of_s "foo._tcp.foo"))) ;
  Alcotest.(check bool "f_oo._tcp.foo is no service" false
              (is_service (n_of_s "f_oo._tcp.foo"))) ;
  Alcotest.(check bool "foo_._tcp.foo is no service" false
              (is_service (n_of_s "foo_._tcp.foo"))) ;
  Alcotest.(check bool "_xmpp-server._tcp.foo is a service" true
              (is_service (n_of_s "_xmpp-server._tcp.foo"))) ;
  Alcotest.(check bool "_xmpp-server._tcp2.foo is no service" false
              (is_service (n_of_s "_xmpp-server._tcp2.foo"))) ;
  Alcotest.(check bool "_xmpp_server._tcp.foo is no service" false
              (is_service (n_of_s "_xmpp_server._tcp.foo"))) ;
  Alcotest.(check bool "_xmpp-server-server._tcp.foo is no service" false
              (is_service (n_of_s "_xmpp-server-server._tcp.foo"))) ;
  Alcotest.(check bool "_443._tcp.foo is a service" true
              (is_service (n_of_s "_443._tcp.foo"))) ;
  let foo = n_of_s "foo" in
  Alcotest.(check bool "foo is no subdomain of foo.bar" false
              (Domain_name.is_subdomain ~subdomain:foo ~domain:(n_of_s "foo.bar"))) ;
  Alcotest.(check bool "foo is a subdomain of foo" true
              (Domain_name.is_subdomain ~subdomain:foo ~domain:foo)) ;
  Alcotest.(check bool "bar.foo is a subdomain of foo" true
              (Domain_name.is_subdomain ~subdomain:(n_of_s "bar.foo") ~domain:foo))

let case () =
  Alcotest.(check bool "foo123.com and Foo123.com are equal" true
              (Domain_name.equal (n_of_s "foo123.com") (n_of_s "Foo123.com"))) ;
  Alcotest.(check bool "foo123.com and Foo123.com are not equal if case" false
              (Domain_name.equal ~case_sensitive:true
                 (n_of_s "foo123.com") (n_of_s "Foo123.com"))) ;
  Alcotest.(check bool "foo-123.com and com are not equal" false
              (Domain_name.equal (n_of_s "foo-123.com") (n_of_s "com"))) ;
  Alcotest.(check bool "foo123.com and Foo123.com are equal if case _and_ canonical used on second"
              true
              Domain_name.(equal ~case_sensitive:true
                 (n_of_s "foo123.com") (canonical (n_of_s "Foo123.com")))) ;
  Alcotest.(check bool "foo123.com and Foo123.com are not equal if case _and_ canonical used on first"
              false
              Domain_name.(equal ~case_sensitive:true
                 (canonical (n_of_s "foo123.com")) (n_of_s "Foo123.com"))) ;
  Alcotest.(check bool "foo123.com and Foo123.com are equal if case _and_ canonical used on both"
              true
              Domain_name.(equal ~case_sensitive:true
                 (canonical (n_of_s "foo123.com")) (canonical (n_of_s "Foo123.com"))))

let p_name = Alcotest.testable Domain_name.pp Domain_name.equal

let basic_name () =
  let lll = String.sub longest_label 0 61
  and llt = String.sub longest_label 0 62
  in
  Alcotest.(check bool "prepend '_foo' to root is not valid hostname"
              false (is_host (Domain_name.prepend_label_exn Domain_name.root "_foo"))) ;
  Alcotest.(check bool "host (of_strings [ '_foo' ; 'bar' ]) is not valid"
              false (is_host (Domain_name.of_strings_exn [ "_foo" ; "bar" ]))) ;
  Alcotest.(check (result p_name p_msg) "of_string 'foo.bar' is valid"
              (Ok (n_of_s "foo.bar")) (Domain_name.of_string "foo.bar")) ;
  Alcotest.(check bool "host (of_string 'foo.bar') is valid"
              true (is_host (Domain_name.of_string_exn "foo.bar"))) ;
  Alcotest.(check p_name "of_array 'foo.bar' is good"
              (n_of_s "foo.bar") (Domain_name.of_array [| "bar" ; "foo" |])) ;
  Alcotest.(check bool "host (of_array 'foo.bar') is good"
              true (is_host (Domain_name.of_array [| "bar" ; "foo" |]))) ;
  Alcotest.(check bool "host (prepend (ll[:-2]) (ll ^ ll ^ ll)) is valid"
              true (is_host (Domain_name.prepend_label_exn (n_of_s longest_prefix) lll))) ;
  Alcotest.(check (result p_name p_msg) "prepend '' root is invalid"
              (Error (`Msg "")) (Domain_name.prepend_label Domain_name.root "")) ;
  Alcotest.(check (result p_name p_msg) "prepend ll^a root is invalid"
              (Error (`Msg "")) (Domain_name.prepend_label Domain_name.root (longest_label ^ "a"))) ;
  Alcotest.(check (result p_name p_msg) "prepend ll (ll ^ ll ^ ll) is invalid"
              (Error (`Msg "")) (Domain_name.prepend_label (n_of_s longest_prefix) longest_label)) ;
  Alcotest.(check (result p_name p_msg) "prepend ll[:-1] (ll ^ ll ^ ll) is invalid"
              (Error (`Msg "")) (Domain_name.prepend_label (n_of_s longest_prefix) llt)) ;
  Alcotest.(check (result p_name p_msg) "concat 'foo.bar' 'baz.barf' is good"
              (Ok (n_of_s "foo.bar.baz.barf"))
              (Domain_name.append (n_of_s "foo.bar") (n_of_s "baz.barf"))) ;
  let r = Domain_name.prepend_label_exn (n_of_s longest_prefix) lll in
  Alcotest.(check (result p_name p_msg) "concat ll[:-2] lp is good"
              (Ok r)
              (Domain_name.append (n_of_s lll) (n_of_s longest_prefix))) ;
  Alcotest.(check (result p_name p_msg) "concat ll[:-1] lp is bad"
              (Error (`Msg ""))
              (Domain_name.append (n_of_s llt) (n_of_s longest_prefix)))

let fqdn () =
  Alcotest.(check bool "of_string_exn example.com = of_string_exn example.com."
              true
              (Domain_name.equal (n_of_s "example.com") (n_of_s "example.com."))) ;
  Alcotest.(check bool "of_strings_exn ['example' ; 'com'] = of_strings_exn ['example' ; 'com' ; '']"
              true
              Domain_name.(equal
                             (of_strings_exn [ "example" ; "com" ])
                             (of_strings_exn [ "example" ; "com" ; "" ])))

let fqdn_around () =
  let d = n_of_s "foo.com." in
  Alcotest.(check bool "of_string (to_string (of_string 'foo.com.')) works"
              true Domain_name.(equal d (of_string_exn (to_string d)))) ;
  Alcotest.(check bool "of_string (to_string ~trailing:true (of_string 'foo.com.')) works"
              true Domain_name.(equal d (of_string_exn (to_string ~trailing:true d))))

let drop_labels () =
  let res = n_of_s "foo.com" in
  Alcotest.(check p_name "dropping 1 label from www.foo.com is foo.com"
              res
              (Domain_name.drop_label_exn (Domain_name.of_string_exn "www.foo.com"))) ;
  Alcotest.(check p_name "dropping 2 labels from www.bar.foo.com is foo.com"
              res
              (Domain_name.drop_label_exn ~amount:2 (Domain_name.of_string_exn "www.bar.foo.com"))) ;
  Alcotest.(check p_name "dropping 1 label from the back www.foo.com is www.foo"
              (Domain_name.of_string_exn "www.foo")
              (Domain_name.drop_label_exn ~rev:true (Domain_name.of_string_exn "www.foo.com"))) ;
  Alcotest.(check p_name "prepending 1 and dropping 1 label from foo.com is foo.com"
              res
              (Domain_name.drop_label_exn (Domain_name.prepend_label_exn (Domain_name.of_string_exn "foo.com") "www"))) ;
  Alcotest.(check p_name "prepending 1 and dropping 1 label from foo.com is foo.com"
              res
              (Domain_name.drop_label_exn (Domain_name.prepend_label_exn (Domain_name.of_string_exn "foo.com") "www"))) ;
  Alcotest.(check (result p_name p_msg)
              "dropping 10 labels from foo.com leads to error"
              (Error (`Msg ""))
              (Domain_name.drop_label ~amount:10 (Domain_name.of_string_exn "foo.com")))

let get_and_count_and_find_label () =
  Alcotest.(check int "count labels of root is 0" 0
              Domain_name.(count_labels root));
  Alcotest.(check (result string p_msg) "get_label 0 of root is Error"
              (Error (`Msg ""))
              Domain_name.(get_label root 0));
  Alcotest.(check (result string p_msg) "get_label 1 of root is Error"
              (Error (`Msg ""))
              Domain_name.(get_label root 1));
  Alcotest.(check (result string p_msg) "get_label 2 of root is Error"
              (Error (`Msg ""))
              Domain_name.(get_label root 2));
  Alcotest.(check (result string p_msg) "get_label -1 of root is Error"
              (Error (`Msg ""))
              Domain_name.(get_label root (-1)));
  Alcotest.(check (option int) "find_label root '' is none"
              None Domain_name.(find_label root (fun _ -> true)));
  Alcotest.(check (option int) "find_label root 'a' is none"
              None Domain_name.(find_label root (equal_label "a")));
  let n = n_of_s "www.example.com" in
  Alcotest.(check int "count labels of www.example.com is 3" 3
              (Domain_name.count_labels n));
  Alcotest.(check (result string p_msg) "get_label 0 of n is Ok www"
              (Ok "www")
              (Domain_name.get_label n 0));
  Alcotest.(check (result string p_msg) "get_label 1 of n is Ok example"
              (Ok "example")
              (Domain_name.get_label n 1));
  Alcotest.(check (result string p_msg) "get_label 2 of n is Ok com"
              (Ok "com")
              (Domain_name.get_label n 2));
  Alcotest.(check (result string p_msg) "get_label 3 of n is Error"
              (Error (`Msg ""))
              (Domain_name.get_label n 3));
  Alcotest.(check (result string p_msg) "get_label ~rev:true 0 of n is Ok com"
              (Ok "com")
              (Domain_name.get_label ~rev:true n 0));
  Alcotest.(check (result string p_msg) "get_label ~rev:true 1 of n is Ok example"
              (Ok "example")
              (Domain_name.get_label ~rev:true n 1));
  Alcotest.(check (result string p_msg) "get_label ~rev:true 2 of n is Ok www"
              (Ok "www")
              (Domain_name.get_label ~rev:true n 2));
  Alcotest.(check (result string p_msg) "get_label ~rev:true 3 of n is Error"
              (Error (`Msg ""))
              (Domain_name.get_label ~rev:true n 3));
  Alcotest.(check (option int) "find_label www.example.com is Some 0"
              (Some 0) Domain_name.(find_label n (fun _ -> true)));
  Alcotest.(check (option int) "find_label www.example.com 'a' is none"
              None Domain_name.(find_label n (equal_label "a")));
  Alcotest.(check (option int) "find_label www.example.com 'w' is none"
              None Domain_name.(find_label n (equal_label "w")));
  Alcotest.(check (option int) "find_label www.example.com 'www' is Some 0"
              (Some 0) Domain_name.(find_label n (equal_label "www")));
  Alcotest.(check (option int) "find_label www.example.com 'WWW' is Some 0"
              (Some 0) Domain_name.(find_label n (equal_label "WWW")));
  Alcotest.(check (option int) "find_label www.example.com 'WWW' is None (case)"
              None
              Domain_name.(find_label n (equal_label ~case_sensitive:true "WWW")));
  let n' = Domain_name.of_string_exn "www.www.www" in
  Alcotest.(check (option int) "find_label www.www.www 'www' is 0"
              (Some 0) Domain_name.(find_label n' (equal_label "www")));
  Alcotest.(check (option int) "find_label ~back:true www.www.www 'www' is 2"
              (Some 2) Domain_name.(find_label ~rev:true n' (equal_label "www")))

let tests = [
  "basic predicates", `Quick, basic_preds ;
  "basic name stuff", `Quick, basic_name ;
  "case", `Quick, case ;
  "fqdn", `Quick, fqdn ;
  "fqdn around", `Quick, fqdn_around ;
  "drop labels", `Quick, drop_labels ;
  "get and count and find labels", `Quick, get_and_count_and_find_label ;
]

let suites = [
  "domain names", tests ;
]

let () = Alcotest.run "domain name tests" suites
