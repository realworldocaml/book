(** ocaml expressions
    (http://caml.inria.fr/pub/docs/manual-ocaml/expr.html)
*)

let e =
  Array.make

let e =
  true

let e =
  (true)

let e =
  begin
    true
  end

let e =
  (true:
     bool)

let e =
  true,
  false,
  true

let e =
  Some
    true

let e =
  `_
    true

let e =
  true ::
  false ::
  true

let e =
  [ true;
    false;
    true;
  ]

let e =
  [| true;
     false;
     true
  |]

let e =
  { f1 = true;
    f2 = false;
    f3 = true;
  }

let e =
  { e with f1 = true;
           f2 = false;
  }

let e =
  f
    true
    false
    true

let e =
  !?
    true

let e =
  true
  || false
     && true

let e =
  1
  mod 1
  land 1
  lor 1
  lxor 1
       lsl 1
       lsr 1
       asr 1

let e =
  re.f1

let e =
  re.f1 <-
    true

let e =
  a.(0) <-
    true

let e =
  a.[0] <-
    true

let e =
  if
    true
  then
    false
  else
    true

let e =
  while
    true
  do
    ()
  done

let e =
  for x =
      a
    to
      b
  do
    ()
  done

let e =
  true;
  false;
  true

let e =
  match
    true
  with
  | true ->
    false
  | false
    ->
    true

let e = match
    true
  with
  | true ->
    false
  | false
    ->
    true

let e =
  function
  | true ->
    false
  | false
    ->
    true

let e =
  fun
    x
    ~ lbl1
    ~ ( lbl2 : int )
    ~lbl3: _a
    ? olbl1
    ? (olbl2 : 'a list = [])
    ?olbl3: _c
    ?olbl4: ( _d : bool = false )
    ()
    when
      true
    ->
      true

let e =
  fun x ->
  fun ~ lbl1 ->
  fun ~ ( lbl2 : int ) ->
  fun ~lbl3: _a ->
  fun ? olbl1 ->
  fun ? (olbl2 : 'a list = []) ->
  fun ?olbl3: _c
    when true
    ->
  fun ?olbl4: ( _d : bool = false ) ->
  fun ()
    when
      true
    ->
      true

let e
    x
    ~ lbl1
    ~ ( lbl2 : int )
    ~lbl3: _a
    ? olbl1
    ? (olbl2 : 'a list = [])
    ?olbl3: _c
    ?olbl4: ( _d : bool = false )
    ()
  =
  true

let e =
  try
    true
  with
  | Exit ->
    true
  | _ ->
    false

let e =
  let rec
    a =
    true
  and _b =
    false
  in
  true

let e =
  new
    foo

let foo =
  object
  end

let e =
  foo#
    bar1

let e =
  (true :>
     bool)

let e =
  (true :
     bool :>
     bool)

let e =
  assert
    true

let e =
  lazy
    true

