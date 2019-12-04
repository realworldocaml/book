type x =
  { foo : int
  ; bar : int
  }

let x =
  { x with
    foo = 3
  ; bar = 5
  }

let x =
  { (* blah blah blah *)
    foo = 3
  ; bar = 5
  }
;;

let x =
  [{ x with
     foo = 3
   ; bar = 5
   }]

let x =
  [{ (* blah blah blah *)
    foo = 3
  ; bar = 5
  }]
;;

let x =
  { M.x with
    M.
    foo = 3
  }
;;

let x =
  { x with
    M.
    foo = 3
  }
;;

let x =
  { M.
    foo = 3
  }
;;

let _ =
  { foo with
    Bar.
    field1 = value1
  ; field2 = value2
  }
;;
let _ =
  { foo
    with Bar.
      field1 = value1
    ; field2 = value2
  }
;;

(* multicomponent record module pathname *)
let _ =
  { A.B.
    a = b
  ; c = d
  }
;;

type t =
  { a
    : something_lengthy list list
      [@default String.Map.empty]
  }

type t =
  { a
    : Something_lengthy.t list list
      [@default String.Map.empty]
  }

type t =
  { a
    : something_lengthy list
        list
  }

type t =
  { a
    : Something_lengthy.t list
        list
  }

type t =
  { a
    : Something_lengthy.t
        list
  }
