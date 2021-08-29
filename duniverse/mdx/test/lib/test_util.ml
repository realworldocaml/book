module String = struct
  let test_english_conjonction =
    let make_test ~name ~input ~expected () =
      let test_name = Printf.sprintf "String.english_conjonction: %s" name in
      let test_fun () =
        let actual = Mdx.Util.String.english_conjonction input in
        Alcotest.(check string test_name expected actual)
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"single" ~input:[ "a" ] ~expected:"a" ();
      make_test ~name:"two" ~input:[ "a"; "b" ] ~expected:"a and b" ();
      make_test ~name:"three" ~input:[ "a"; "b"; "c" ] ~expected:"a, b and c" ();
      make_test ~name:"more" ~input:[ "a"; "b"; "c"; "d" ]
        ~expected:"a, b, c and d" ();
    ]
end

let suite = ("Util", String.test_english_conjonction)
