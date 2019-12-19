
type _ key =
  | I : int key
  | S : string key

let pp_m : type a . Format.formatter -> a key -> a -> unit = fun ppf k v ->
  match k, v with
  | I, x -> Fmt.pf ppf "I %d" x
  | S, s -> Fmt.pf ppf "S %s" s

let eq_m : type a. a key -> a -> a -> bool = fun k v v' ->
  match k, v, v' with
  | I, x, y -> x = y
  | S, s, t -> String.equal s t

module K = struct
  type 'a t = 'a key

  let compare : type a b. a t -> b t -> (a, b) Gmap.Order.t = fun t t' ->
    let open Gmap.Order in
    match t, t' with
    | I, I -> Eq | I, _ -> Lt | _, I -> Gt
    | S, S -> Eq
end

module M = Gmap.Make(K)

let m_check =
  let module M = struct
    type t = M.t
    let pp ppf m = M.iter (fun (M.B (k, v)) -> pp_m ppf k v) m
    let equal a b = M.equal { f = eq_m } a b
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let b_check =
  let module M = struct
    type t = M.b
    let pp ppf (M.B (k, v)) = pp_m ppf k v
    let equal (M.B (k, v)) (M.B (k', v')) = match K.compare k k' with
      | Gmap.Order.Eq -> eq_m k v v'
      | _ -> false
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let empty () =
  Alcotest.(check bool "empty map is empty" true (M.is_empty M.empty));
  Alcotest.(check bool "mem on empty map doesn't have A" false (M.mem I M.empty));
  Alcotest.(check (option int) "find on empty map doesn't have A" None
              (M.find I M.empty));
  Alcotest.(check (option string) "find on empty map doesn't have B" None
              (M.find S M.empty));
  Alcotest.(check (option b_check) "min binding is none" None
              (M.min_binding M.empty));
  Alcotest.(check (option b_check) "max binding is none" None
              (M.max_binding M.empty));
  Alcotest.(check (option b_check) "any binding is none" None
              (M.any_binding M.empty));
  Alcotest.(check (list b_check) "bindings is empty" []
              (M.bindings M.empty))

let basic () =
  let m = M.singleton I 5 in
  Alcotest.(check bool "non-empty map is not empty" false (M.is_empty m));
  Alcotest.(check int "non-empty map has cardinal 1" 1 (M.cardinal m));
  Alcotest.(check bool "non-empty map has member I" true (M.mem I m));
  Alcotest.(check (option int) "non-empty map finds I" (Some 5) (M.find I m));
  Alcotest.check m_check "singleton and add are equivalent" m (M.add I 5 M.empty);
  Alcotest.(check bool "removing I from map makes it empty" true
              (M.is_empty (M.remove I m)));
  Alcotest.(check bool "removing S from map makes it not empty" false
              (M.is_empty (M.remove S m)));
  Alcotest.check m_check "add overwrites" (M.singleton I 10) (M.add I 10 m);
  Alcotest.(check (option m_check) "add_unless_bound does not overwrite" None
              (M.add_unless_bound I 10 m));
  Alcotest.check m_check "update updates" (M.singleton I 20)
    (M.update I (fun _ -> Some 20) m);
  Alcotest.(check (option b_check) "min_binding is I 5" (Some (M.B (I, 5)))
              (M.min_binding m));
  Alcotest.(check (option b_check) "max_binding is I 5" (Some (M.B (I, 5)))
              (M.max_binding m));
  Alcotest.(check (option b_check) "any_binding is I 5" (Some (M.B (I, 5)))
              (M.any_binding m));
  Alcotest.(check (list b_check) "bindings is [ I 5 ]" [ M.B (I, 5) ]
              (M.bindings m))

let bad_eq_false : type a. a key -> a -> a -> bool = fun _ _ _ -> false
let bad_eq_true : type a. a key -> a -> a -> bool = fun _ _ _ -> true

let eq () =
  let m = M.singleton I 5 in
  Alcotest.(check bool "m equal is ok" true (M.equal { f = eq_m } m m));
  Alcotest.(check bool "m equal is ok with singleton" true
              (M.equal { f = eq_m } m (M.singleton I 5)));
  Alcotest.(check bool "m equal is false" false
              (M.equal { f = eq_m } m M.empty));
  Alcotest.(check bool "m equal is false" false
              (M.equal { f = eq_m } m (M.singleton S "foo")));
  Alcotest.(check bool "m equal is false" false
              (M.equal { f = eq_m } m (M.singleton I 10)));
  Alcotest.(check bool "m equal is false" false
              (M.equal { f = eq_m } m (M.add S "foo" (M.singleton I 10))));
  Alcotest.(check bool "m bad equal is always false" false
              (M.equal { f = bad_eq_false } m m));
  Alcotest.(check bool "m bad equal is always true" true
              (M.equal { f = bad_eq_true } m m))

let preds () =
  let m = M.singleton I 5 in
  let m' = M.add S "foobar" m in
  let m'' = M.singleton I 10 in
  let p (M.B (k, v)) = match k with I -> v = 5 | _ -> false in
  Alcotest.(check bool "for_all works" true (M.for_all p m));
  Alcotest.(check bool "for_all works m'" false (M.for_all p m'));
  Alcotest.(check bool "for_all works m''" false (M.for_all p m''));
  Alcotest.(check bool "exists works" true (M.exists p m));
  Alcotest.(check bool "exists works m'" true (M.exists p m'));
  Alcotest.(check bool "exists works m''" false (M.exists p m''));
  Alcotest.check m_check "filter works" m (M.filter p m);
  Alcotest.check m_check "filter works m'" m (M.filter p m');
  Alcotest.check m_check "filter works m''" M.empty (M.filter p m'')

let map () =
  let m = M.singleton I 5 in
  let map : type a . a key -> a -> a = fun k _v ->
    match k with
    | I -> 100
    | S -> "Foo"
  in
  Alcotest.check m_check "mapped m is equal as expected"
    (M.singleton I 100) (M.map { f = map } m);
  Alcotest.check m_check "mapped m is equal as expected"
    (M.add S "Foo" (M.singleton I 100))
    (M.map { f = map } (M.add S "barf" m))

let l_wins : type a . a key -> a -> a -> a option = fun _ v _ -> Some v
let r_wins : type a . a key -> a -> a -> a option = fun _ _ v' -> Some v'
let no_wins : type a . a key -> a -> a -> a option = fun _ _ _ -> None

let union () =
  let m = M.add I 100 (M.singleton S "foo") in
  Alcotest.check m_check "union map left wins is good" m
    (M.union { f = l_wins } m (M.singleton S "bar"));
  Alcotest.check m_check "union map right wins is good"
    (M.add I 100 (M.singleton S "bar"))
    (M.union { f = r_wins } m (M.singleton S "bar"));
  Alcotest.check m_check "union map right wins is good"
    (M.singleton I 100)
    (M.union { f = no_wins } m (M.singleton S "bar"))

let tests = [
  "empty gmap", `Quick, empty ;
  "basic gmap", `Quick, basic ;
  "equality", `Quick, eq ;
  "predicates", `Quick, preds ;
  "map", `Quick, map ;
  "union", `Quick, union ;
]

let () = Alcotest.run "gmap tests" [ "gmap suite", tests ]
