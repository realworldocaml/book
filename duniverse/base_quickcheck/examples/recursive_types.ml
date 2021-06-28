open! Base
open Base_quickcheck

module Binary_tree : sig
  type t =
    | Leaf
    | Node of t * t
  [@@deriving quickcheck]
end = struct
  type t =
    | Leaf
    | Node of t * t
  [@@deriving hash]

  let quickcheck_generator =
    let open Generator.Let_syntax in
    Generator.recursive_union [ Generator.return Leaf ] ~f:(fun self ->
      [ (let%map l = self
         and r = self in
         Node (l, r))
      ])
  ;;

  (* Observers can be derived from hash functions for most types. *)
  let quickcheck_observer = Observer.of_hash_fold hash_fold_t

  (* A good strategy for shrinking a recursive node is to try replacing the node with its
     recursive children, and also try pointwise-shrinking all fields of the node. *)
  let quickcheck_shrinker =
    let rec f = function
      | Leaf -> Sequence.empty
      | Node (l, r) ->
        Sequence.round_robin
          [ Sequence.singleton l
          ; Sequence.singleton r
          ; Sequence.map (f l) ~f:(fun l -> Node (l, r))
          ; Sequence.map (f r) ~f:(fun r -> Node (l, r))
          ]
    in
    Shrinker.create f
  ;;
end

module N_ary_tree : sig
  type 'a t = Node of 'a * 'a t list [@@deriving quickcheck]
end = struct
  type 'a t = Node of 'a * 'a t list

  let quickcheck_generator quickcheck_generator_key =
    (* In order to bound recursion depth, we rely on the fact that the [list] generator
       always generates elements at strictly smaller sizes than the list itself. *)
    Generator.fixed_point (fun quickcheck_generator ->
      [%quickcheck.generator: key * t list]
      |> Generator.map ~f:(fun (key, list) -> Node (key, list)))
  ;;

  let quickcheck_observer quickcheck_observer_key =
    (* For polymorphic types, we cannot directly derive an observer from a hash function.
       So we use [Observer.fixed_point] instead. *)
    Observer.fixed_point (fun quickcheck_observer ->
      [%quickcheck.observer: key * t list]
      |> Observer.unmap ~f:(fun (Node (key, list)) -> key, list))
  ;;

  let quickcheck_shrinker quickcheck_shrinker_key =
    (* We can define a simple shrinker using [Shrinker.fixed_point]. It won't include the
       strategy above of replacing a recursive node with its children. *)
    Shrinker.fixed_point (fun quickcheck_shrinker ->
      [%quickcheck.shrinker: key * t list]
      |> Shrinker.map
           ~f:(fun (key, list) -> Node (key, list))
           ~f_inverse:(fun (Node (key, list)) -> key, list))
  ;;
end
