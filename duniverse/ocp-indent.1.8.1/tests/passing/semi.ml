let f () =
  print_endline "a"
  ;
  print_endline "b"

let f () = toto
         ; blah

let f () =
  { a = 3
  ; b = 4
  ;
  }

module A = struct

  type x =
    { a: int
    ; b: int
    ;
    }

end
