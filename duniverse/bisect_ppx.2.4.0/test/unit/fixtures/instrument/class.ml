(* Default value. *)
class default = fun ?(foo = ()) () ->
  object
  end

(* Application. *)
class applied =
  default ~foo:(print_endline "foo") (print_endline "bar")

(* Let. *)
class let_ =
  let foo = print_endline "foo" in
  default foo

(* Expression in val is not in tail position. *)
class val_ =
  object
    val foo =
      print_endline "foo"
  end

(* Method. *)
class method_1 =
  object
    method foo =
      print_endline "foo"
  end

(* Method with additional arguments. *)
class method_2 =
  object
    method foo () =
      print_endline "foo"
  end

(* Method with polymorphic type. *)
let helper = raise

class method_3 =
  object
    method foo : 'a. 'a =
      helper Exit
  end

(* Method with polymorphic type and additional arguments. *)
class method_4 =
  object
    method foo : 'a. 'a -> unit = fun _ ->
      ()
  end

class initializer_ =
  object
    initializer (print_endline "foo")
  end
