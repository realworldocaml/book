open! Import

module type S = Indexed_container.S1 with type 'a t = 'a list

module This_list : S = struct
  include List

  include Indexed_container.Make (struct
      type 'a t = 'a list

      let fold = List.fold
      let iter = `Custom List.iter
      let length = `Custom List.length
      let foldi = `Define_using_fold
      let iteri = `Define_using_fold
    end)
end

module That_list : S = List

let examples = [ []; [ 1 ]; [ 2; 3 ]; [ 4; 5; 1 ]; List.init 8 ~f:(fun i -> i * i) ]

module type Output = sig
  type t [@@deriving compare, sexp_of]
end

module Int_list = struct
  type t = int list [@@deriving compare, sexp_of]
end

module Int_pair_option = struct
  type t = (int * int) option [@@deriving compare, sexp_of]
end

module Int_option = struct
  type t = int option [@@deriving compare, sexp_of]
end

let check (type a) here examples ~actual ~expect (module Output : Output with type t = a)
  =
  List.iter examples ~f:(fun example ->
    let actual = actual example in
    let expect = expect example in
    require
      here
      (Output.compare actual expect = 0)
      ~if_false_then_print_s:(lazy [%message (expect : Output.t)]);
    print_s [%sexp (actual : Output.t)])
;;

let%expect_test "foldi" =
  let f i acc elt = if i % 2 = 0 then elt :: acc else acc in
  check
    [%here]
    examples
    (module Int_list)
    ~actual:(fun list -> This_list.foldi list ~init:[] ~f)
    ~expect:(fun list -> That_list.foldi list ~init:[] ~f);
  [%expect {|
    ()
    (1)
    (2)
    (1 4)
    (36 16 4 0) |}]
;;

let%expect_test "findi" =
  let check f =
    check
      [%here]
      examples
      (module Int_pair_option)
      ~actual:(fun list -> This_list.findi list ~f)
      ~expect:(fun list -> That_list.findi list ~f)
  in
  check (fun i _elt -> i = 0);
  [%expect {|
    ()
    ((0 1))
    ((0 2))
    ((0 4))
    ((0 0)) |}];
  check (fun _i elt -> elt = 1);
  [%expect {|
    ()
    ((0 1))
    ()
    ((2 1))
    ((1 1)) |}]
;;

let%expect_test "find_mapi" =
  let f i elt = if elt = 1 then Some ((i * 100) + elt) else None in
  check
    [%here]
    examples
    (module Int_option)
    ~actual:(fun list -> This_list.find_mapi list ~f)
    ~expect:(fun list -> That_list.find_mapi list ~f);
  [%expect {|
    ()
    (1)
    ()
    (201)
    (101) |}]
;;

let%expect_test "iteri" =
  let go iteri =
    let acc = ref [] in
    iteri ~f:(fun i elt -> acc := i :: elt :: !acc);
    !acc
  in
  check
    [%here]
    examples
    (module Int_list)
    ~actual:(fun list -> go (This_list.iteri list))
    ~expect:(fun list -> go (That_list.iteri list));
  [%expect
    {|
    ()
    (0 1)
    (1 3 0 2)
    (2 1 1 5 0 4)
    (7 49 6 36 5 25 4 16 3 9 2 4 1 1 0 0) |}]
;;

let bool_examples =
  [ []
  ; [ true ]
  ; [ false ]
  ; [ false; false ]
  ; [ true; false ]
  ; [ false; true ]
  ; [ true; true ]
  ]
;;

let%expect_test "for_alli" =
  let f _i elt = elt in
  check
    [%here]
    bool_examples
    (module Bool)
    ~actual:(fun list -> This_list.for_alli list ~f)
    ~expect:(fun list -> That_list.for_alli list ~f);
  [%expect {|
    true
    true
    false
    false
    false
    false
    true |}]
;;

let%expect_test "existsi" =
  let f _i elt = elt in
  check
    [%here]
    bool_examples
    (module Bool)
    ~actual:(fun list -> This_list.existsi list ~f)
    ~expect:(fun list -> That_list.existsi list ~f);
  [%expect {|
    false
    true
    false
    false
    true
    true
    true |}]
;;

let%expect_test "counti" =
  let f _i elt = elt in
  check
    [%here]
    bool_examples
    (module Int)
    ~actual:(fun list -> This_list.counti list ~f)
    ~expect:(fun list -> That_list.counti list ~f);
  [%expect {|
    0
    1
    0
    0
    1
    1
    2 |}]
;;
