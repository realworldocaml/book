let a =
  {
    somerecord
    with
      a = b;
      c = d;
  }

let a =
  {
    somerecord
    with a = b;
         c = d;
  }

let z =
  { recofzfzfzrd   with a = bli; bzeefe =
                                   k
                      ; efgeg = a
  }

let b =
  let z =
    {           reczfzrd                  with a = bli;
                                               bzeefe = _;
    }

let b =
  let z =
    { reczfzrd with a = bli;
                    bzeefe
    }

let lexbuf = { lexbuf with Lexing.lex_start_p = start_pos;
                           Lexing.lex_curr_p = start_pos; }

let () =
  { Foo.
    foo
  ; bar = (fun () ->
        if a then b)
  }

let () =
  { foo
  ; bar = (fun () ->
        if a then b)
  }
