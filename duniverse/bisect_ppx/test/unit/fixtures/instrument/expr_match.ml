let f x =
  match x with
  | 0 -> print_endline "abc"
  | 1 -> print_endline "def"
  | _ -> print_endline "ghi"

let f = function
  | 0 -> print_endline "abc"
  | 1 -> print_endline "def"
  | _ -> print_endline "ghi"

let f x =
  match x with
  | 0 -> print_string "abc"; print_newline ()
  | 1 -> print_string "def"; print_newline ()
  | _ -> print_string "ghi"; print_newline ()

let f = function
  | 0 -> print_string "abc"; print_newline ()
  | 1 -> print_string "def"; print_newline ()
  | _ -> print_string "ghi"; print_newline ()

type t =
  | Foo
  | Bar

let f x =
  match x with
  | Foo -> print_string "foo"; print_newline ()
  | Bar -> print_string "bar"; print_newline ()

let f = function
  | Foo -> print_string "foo"; print_newline ()
  | Bar -> print_string "bar"; print_newline ()

let f x =
  (function
  | Foo -> "foo"
  | Bar -> "bar")
  x
  |> print_string;
  print_newline ()

let f x =
  match x with
  | Foo -> print_endline "foo"
  | Bar ->
    match x with
    | Foo -> print_endline "foobar"
    | Bar -> print_endline "barbar"

let f x =
  match x with
  | Foo | Bar -> print_endline "foo"

let f x =
  match x with
  | (Foo, _) | (Bar, _) -> print_endline "foo"

let f x =
  match x with
  | (Foo | Bar), (Foo | Bar) -> print_endline "foo"

let f x =
  match x with
  | 'a'..'z' -> print_endline "foo"
  | _ -> print_endline "bar"

let f x =
  match x with
  | `A -> print_endline "foo"
  | `B -> print_endline "bar"

type u = [ `A | `B ]

let f x =
  match x with
  | #u -> print_endline "foo"

module type S = sig end

let f x =
  match x with
  | (module X : S) -> print_endline "foo"

let f x =
  match x with
  | Foo | Bar as y -> y

let f x =
  match x with
  | (Foo | Bar)::_ -> print_endline "foo"
  | [] -> print_endline "bar"

let f x =
  match x with
  | `A _ -> print_endline "foo"
  | `B (Foo | Bar) -> print_endline "bar"

type v = {a : t; b : t}

let f x =
  match x with
  | {a = (Foo | Bar); b = (Foo | Bar)} -> print_endline "foo"

let f x =
  match x with
  | [||] -> print_endline "foo"
  | [|(Foo | Bar); (Foo | Bar); _|] -> print_endline "bar"
  | _ -> print_newline ()

let f x =
  match x with
  | lazy (Foo | Bar) -> print_endline "foo"

exception Exn of t

let f x =
  match x with
  | exception Exn (Foo | Bar) -> print_endline "foo"
  | _ -> print_endline "bar"

let f x =
  match x with
  | (Foo as x) | (Bar as x) -> x

let f x =
  match x with
  | `Foo x | `Bar x -> x

let last = function
  | [] -> None
  | _::_ as li ->
    match List.rev li with
    | last::_ -> Some last
    | _ -> assert false
      (* 'assert false' is an idiom to mark non-trivial unreachability here,
         we automatically ignore the clause *)
