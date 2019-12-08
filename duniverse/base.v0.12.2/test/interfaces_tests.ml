open Base

let () =
  let module M : sig
    open Set

    type ('a, 'b) t

    include Accessors2
      with type ('a, 'b) t    := ('a, 'b) t
      with type ('a, 'b) tree := ('a, 'b) Set.Using_comparator.Tree.t
      with type ('a, 'b) named := ('a, 'b) Set.Named.t

    include Creators_generic
      with type ('a, 'b, 'c) options := ('a, 'b, 'c) With_first_class_module.t
      with type ('a, 'b) set  := ('a, 'b) t
      with type ('a, 'b) t    := ('a, 'b) t
      with type ('a, 'b) tree := ('a, 'b) Set.Using_comparator.Tree.t
  end = struct
    type 'a elt = 'a
    type _ cmp
    include Set
    let of_tree _ = assert false
    let to_tree _ = assert false
  end in ()

let () =
  let module M : sig
    open Map

    type ('a, 'b, 'c) t

    include Accessors3
      with type ('a, 'b, 'c) t    := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Map.Using_comparator.Tree.t

    include Creators_generic
      with type ('a, 'b, 'c) options := ('a, 'b, 'c) With_first_class_module.t
      with type ('a, 'b, 'c) t    := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Map.Using_comparator.Tree.t
  end = struct
    type 'a key = 'a
    include Map
    let of_tree _ = assert false
    let to_tree _ = assert false
  end
  in
  ()
