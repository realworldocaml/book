(*
   Demonstate use of [%expect]
   to match a single line of text with 0|1|2 leading & trailing NLs.

   Starting with..

   {[
     let%expect_test _ =
       let module M = struct
         let () = print_string "hello";          [%expect{||}]
         let () = print_string "hello\n";        [%expect{||}]
         let () = print_string "hello\n\n";      [%expect{||}]
         let () = print_string "\nhello";        [%expect{||}]
         let () = print_string "\nhello\n";      [%expect{||}]
         let () = print_string "\nhello\n\n";    [%expect{||}]
         let () = print_string "\n\nhello";      [%expect{||}]
         let () = print_string "\n\nhello\n";    [%expect{||}]
         let () = print_string "\n\nhello\n\n";  [%expect{||}]
       end in ()
   ]}

   Generate with [cp nine.ml.corrected nine.ml] the following [%expect]... *)

let%expect_test _ =
  let module M = struct
    let () = print_string "hello";          [%expect {| hello |}]
    let () = print_string "hello\n";        [%expect {|
                                              hello
                                            |}]
    let () = print_string "hello\n\n";      [%expect {|
                                              hello

                                            |}]
    let () = print_string "\nhello";        [%expect {|

                                              hello|}]
    let () = print_string "\nhello\n";      [%expect {|

                                              hello
                                            |}]
    let () = print_string "\nhello\n\n";    [%expect {|

                                              hello

                                            |}]
    let () = print_string "\n\nhello";      [%expect {|


                                              hello|}]
    let () = print_string "\n\nhello\n";    [%expect {|


                                              hello
                                            |}]
    let () = print_string "\n\nhello\n\n";  [%expect {|


                                              hello

                                            |}]
  end in ()
