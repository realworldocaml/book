open! Base
include Observer0

let unmap t ~f = create (fun x ~size ~hash -> observe t (f x) ~size ~hash)
let of_hash_fold f = create (fun x ~size:_ ~hash -> f hash x)
let of_lazy lazy_t = create (fun x ~size ~hash -> observe (force lazy_t) x ~size ~hash)

let fixed_point wrap =
  let rec lazy_t = lazy (wrap (of_lazy lazy_t)) in
  of_lazy lazy_t
;;

let unit = opaque
let bool = of_hash_fold Bool.hash_fold_t
let char = of_hash_fold Char.hash_fold_t
let int = of_hash_fold Int.hash_fold_t
let int32 = of_hash_fold Int32.hash_fold_t
let int63 = of_hash_fold Int63.hash_fold_t
let int64 = of_hash_fold Int64.hash_fold_t
let nativeint = of_hash_fold Nativeint.hash_fold_t
let float = of_hash_fold Float.hash_fold_t
let string = of_hash_fold String.hash_fold_t
let sexp = of_hash_fold Sexp.hash_fold_t
let bigstring = of_hash_fold (Bigarray_helpers.Array1.hash_fold hash_fold_char)
let float32_vec = of_hash_fold (Bigarray_helpers.Array1.hash_fold hash_fold_float)
let float64_vec = of_hash_fold (Bigarray_helpers.Array1.hash_fold hash_fold_float)
let float32_mat = of_hash_fold (Bigarray_helpers.Array2.hash_fold hash_fold_float)
let float64_mat = of_hash_fold (Bigarray_helpers.Array2.hash_fold hash_fold_float)

let either fst_t snd_t =
  create (fun either ~size ~hash ->
    match (either : _ Either.t) with
    | First fst -> observe fst_t fst ~size ~hash:(hash_fold_int hash 1)
    | Second snd -> observe snd_t snd ~size ~hash:(hash_fold_int hash 2))
;;

let result ok_t err_t =
  unmap (either ok_t err_t) ~f:(function
    | Ok ok -> First ok
    | Error err -> Second err)
;;

let both fst_t snd_t =
  create (fun (fst, snd) ~size ~hash ->
    let hash = observe fst_t fst ~size ~hash in
    let hash = observe snd_t snd ~size ~hash in
    hash)
;;

let option value_t =
  unmap (either opaque value_t) ~f:(function
    | None -> First ()
    | Some value -> Second value)
;;

let list elt_t =
  create (fun list ~size ~hash ->
    let random = Splittable_random.State.of_int (Hash.get_hash_value hash) in
    let length = List.length list in
    let sizes =
      Generator.sizes ~min_length:length ~max_length:length ()
      |> Generator.generate ~size ~random
    in
    List.fold2_exn list sizes ~init:(hash_fold_int hash 0) ~f:(fun hash elt size ->
      observe elt_t elt ~size ~hash:(hash_fold_int hash 1)))
;;

let fn dom rng =
  create (fun f ~size ~hash ->
    let random = Splittable_random.State.of_int (Hash.get_hash_value hash) in
    let sizes =
      (* Empirically, doubling the size when generating the list of inputs gives us much
         better coverage of the space of functions. *)
      Generator.generate (Generator.sizes ()) ~size:(size * 2) ~random
    in
    List.fold sizes ~init:hash ~f:(fun hash size ->
      let x = Generator.generate dom ~size ~random in
      observe rng (f x) ~size ~hash))
;;

let map_tree key_obs data_obs =
  unmap (list (both key_obs data_obs)) ~f:Map.Using_comparator.Tree.to_alist
;;

let set_tree elt_obs = unmap (list elt_obs) ~f:Set.Using_comparator.Tree.to_list

let map_t key_obs data_obs =
  unmap (map_tree key_obs data_obs) ~f:Map.Using_comparator.to_tree
;;

let set_t elt_obs = unmap (set_tree elt_obs) ~f:Set.Using_comparator.to_tree
