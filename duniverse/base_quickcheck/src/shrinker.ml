open! Base

module T : sig
  type 'a t

  val atomic : _ t
  val create : ('a -> 'a Sequence.t) -> 'a t
  val shrink : 'a t -> 'a -> 'a Sequence.t
end = struct
  type 'a t = 'a -> 'a Sequence.t

  let atomic _ = Sequence.empty
  let create = Fn.id
  let shrink = Fn.id
end

include T

let map t ~f ~f_inverse = create (fun x -> Sequence.map ~f (shrink t (f_inverse x)))
let of_lazy lazy_t = create (fun x -> Sequence.of_lazy (lazy (shrink (force lazy_t) x)))

let fixed_point of_shrinker =
  let rec lazy_t = lazy (of_shrinker (of_lazy lazy_t)) in
  of_lazy lazy_t
;;

let both fst_t snd_t =
  create (fun (fst, snd) ->
    Sequence.round_robin
      [ Sequence.map (shrink fst_t fst) ~f:(fun fst -> fst, snd)
      ; Sequence.map (shrink snd_t snd) ~f:(fun snd -> fst, snd)
      ])
;;

let unit = atomic
let bool = atomic
let char = atomic
let int = atomic
let int32 = atomic
let int63 = atomic
let int64 = atomic
let nativeint = atomic
let float = atomic

let option value_t =
  create (function
    | None -> Sequence.empty
    | Some value ->
      Sequence.append
        (Sequence.singleton None)
        (Sequence.map ~f:Option.return (shrink value_t value)))
;;

let list elt_t =
  fixed_point (fun list_t ->
    create (function
      | [] -> Sequence.empty
      | head :: tail ->
        Sequence.round_robin
          [ Sequence.singleton tail
          ; Sequence.map (shrink elt_t head) ~f:(fun head -> head :: tail)
          ; Sequence.map (shrink list_t tail) ~f:(fun tail -> head :: tail)
          ]))
;;

let string = map (list char) ~f:String.of_char_list ~f_inverse:String.to_list

let sexp =
  fixed_point (fun shrinker ->
    create (function
      | Sexp.Atom _ -> Sequence.empty
      | Sexp.List l ->
        let shrink_list =
          shrink (list shrinker) l |> Sequence.map ~f:(fun l -> Sexp.List l)
        in
        let shrink_tree = Sequence.of_list l in
        Sequence.round_robin [ shrink_list; shrink_tree ]))
;;

let either fst_t snd_t =
  create (fun either ->
    match (either : _ Either.t) with
    | First fst -> Sequence.map (shrink fst_t fst) ~f:Either.first
    | Second snd -> Sequence.map (shrink snd_t snd) ~f:Either.second)
;;

let result ok_t err_t =
  map
    (either ok_t err_t)
    ~f:(function
      | First ok -> Ok ok
      | Second err -> Error err)
    ~f_inverse:(function
      | Ok ok -> First ok
      | Error err -> Second err)
;;

let map_tree_using_comparator ~comparator key_t data_t =
  create (fun tree ->
    let alist = Map.Using_comparator.Tree.to_alist tree in
    let drop_keys =
      Sequence.map (Sequence.of_list alist) ~f:(fun (k, _) ->
        Map.Using_comparator.Tree.remove ~comparator tree k)
    in
    let shrink_keys =
      Sequence.round_robin
        (List.map alist ~f:(fun (key, data) ->
           let tree = Map.Using_comparator.Tree.remove ~comparator tree key in
           Sequence.filter_map (shrink key_t key) ~f:(fun smaller_key ->
             match
               Map.Using_comparator.Tree.add
                 ~comparator
                 tree
                 ~key:smaller_key
                 ~data
             with
             | `Ok tree -> Some tree
             | `Duplicate -> None)))
    in
    let shrink_data =
      Sequence.round_robin
        (List.map alist ~f:(fun (key, data) ->
           Sequence.map (shrink data_t data) ~f:(fun smaller_data ->
             Map.Using_comparator.Tree.set ~comparator tree ~key ~data:smaller_data)))
    in
    Sequence.round_robin [ drop_keys; shrink_keys; shrink_data ])
;;

let set_tree_using_comparator ~comparator elt_t =
  create (fun tree ->
    let list = Set.Using_comparator.Tree.to_list tree in
    let drop_elts =
      Sequence.map (Sequence.of_list list) ~f:(fun elt ->
        Set.Using_comparator.Tree.remove ~comparator tree elt)
    in
    let shrink_elts =
      Sequence.round_robin
        (List.map list ~f:(fun elt ->
           let tree = Set.Using_comparator.Tree.remove ~comparator tree elt in
           Sequence.filter_map (shrink elt_t elt) ~f:(fun smaller_elt ->
             match Set.Using_comparator.Tree.mem ~comparator tree smaller_elt with
             | true -> None
             | false ->
               Some (Set.Using_comparator.Tree.add tree ~comparator smaller_elt))))
    in
    Sequence.round_robin [ drop_elts; shrink_elts ])
;;

let map_t key_t data_t =
  create (fun map_t ->
    let comparator = Map.comparator map_t in
    let t =
      map
        (map_tree_using_comparator ~comparator key_t data_t)
        ~f:(Map.Using_comparator.of_tree ~comparator)
        ~f_inverse:Map.Using_comparator.to_tree
    in
    shrink t map_t)
;;

let set_t elt_t =
  create (fun set_t ->
    let comparator = Set.comparator set_t in
    let t =
      map
        (set_tree_using_comparator ~comparator elt_t)
        ~f:(Set.Using_comparator.of_tree ~comparator)
        ~f_inverse:Set.Using_comparator.to_tree
    in
    shrink t set_t)
;;
