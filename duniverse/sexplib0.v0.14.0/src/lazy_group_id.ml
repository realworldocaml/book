(** Why allocate a ref instead of storing the int directly?

    We generate many more sexp grammars than actually get used, so we prefer to defer the
    id until we need it. The compiler can optimize away allocations that nobody touches.
*)

type t = int Lazy.t

let create =
  let next = ref 0 in
  fun () -> lazy (
    (* As long as we don't give up the global Ocaml runtime lock by allocating, we can
       treat the read and write as atomic. See "20.12.2 Parallel execution of long-running
       C code" in the 4.09 manual. *)
    let id = !next in
    next := id + 1;
    id)

let force (t : t) = Lazy.force t

let compare a b = compare (force a) (force b)
