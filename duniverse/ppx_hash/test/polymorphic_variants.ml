type a = [ `A ] [@@deriving hash]

type ab =
  [ `A
  | `B
  ]
[@@deriving hash]

type abc_v1 =
  [ ab
  | `C
  ]
[@@deriving hash]

type abc_v2 =
  [ `A
  | `B
  | `C
  ]
[@@deriving hash]

(* Check that the same polymorphic variant has the same hash regardless of which
   type it's in. This property is relied on internally to implement hashing of types like
   [abc_v1] (see above).
   We could potentially use this property to justify support of open polymorphic
   variants in the future. *)
let%expect_test "`A hashes the same regardless of which type it's in" =
  let h1 = [%hash: a] `A in
  let h2 = [%hash: ab] `A in
  let h3 = [%hash: abc_v1] `A in
  let h4 = [%hash: abc_v2] `A in
  let h5 = [%hash: [ ab | `C ]] `A in
  assert (h1 = h2);
  assert (h2 = h3);
  assert (h3 = h4);
  assert (h4 = h5)
;;

let%expect_test "Up to polymorphic variant functions behave exactly the same as fully \
                 bounded polymorphic variant functions"
  =
  let module H : sig
    val h1 : [%hash: [ `A | `B ]]
    val h2 : [%hash: [< `A | `B ]]
    val h3 : [%hash: [< `A | `B > `A ]]
  end = struct
    let h1 = [%hash: [ `A | `B ]]
    let h2 = [%hash: [< `A | `B ]]
    let h3 = [%hash: [< `A | `B > `A ]]
  end
  in
  let open H in
  assert (h1 `A = h2 `A);
  assert (h1 `A = h3 `A);
  assert (h1 `B = h2 `B);
  assert (h1 `B = h3 `B)
;;
