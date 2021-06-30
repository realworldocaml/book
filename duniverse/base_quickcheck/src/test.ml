open! Base
include Test_intf

module Config = struct
  module Seed = struct
    type t =
      | Nondeterministic
      | Deterministic of string
    [@@deriving sexp_of]
  end

  module Potentially_infinite_sequence = struct
    type 'a t = 'a Sequence.t

    let sexp_of_t sexp_of_elt sequence =
      let prefix, suffix = Sequence.split_n sequence 100 in
      let prefix = List.map prefix ~f:sexp_of_elt in
      let suffix =
        match Sequence.is_empty suffix with
        | true -> []
        | false -> [ [%message "..."] ]
      in
      Sexp.List (prefix @ suffix)
    ;;
  end

  type t =
    { seed : Seed.t
    ; test_count : int
    ; shrink_count : int
    ; sizes : int Potentially_infinite_sequence.t
    }
  [@@deriving fields, sexp_of]
end

let default_config : Config.t =
  { seed = Deterministic "an arbitrary but deterministic string"
  ; test_count = 10_000
  ; shrink_count = 10_000
  ; sizes = Sequence.cycle_list_exn (List.range 0 ~start:`inclusive 30 ~stop:`inclusive)
  }
;;

let lazy_nondeterministic_state = lazy (Random.State.make_self_init ())

let initial_random_state ~config =
  match Config.seed config with
  | Nondeterministic ->
    Splittable_random.State.create (force lazy_nondeterministic_state)
  | Deterministic string -> Splittable_random.State.of_int (String.hash string)
;;

let one_size_per_test ~(config : Config.t) =
  Sequence.unfold ~init:(config.sizes, 0) ~f:(fun (sizes, number_of_size_values) ->
    match number_of_size_values >= config.test_count with
    | true -> None
    | false ->
      (match Sequence.next sizes with
       | Some (size, remaining_sizes) ->
         Some (size, (remaining_sizes, number_of_size_values + 1))
       | None ->
         raise_s
           [%message
             "Base_quickcheck.Test.run: insufficient size values for test count"
               ~test_count:(config.test_count : int)
               (number_of_size_values : int)]))
;;

let shrink_error ~shrinker ~config ~f input error =
  let rec loop ~shrink_count ~alternates input error =
    match shrink_count with
    | 0 -> input, error
    | _ ->
      let shrink_count = shrink_count - 1 in
      (match Sequence.next alternates with
       | None -> input, error
       | Some (alternate, alternates) ->
         (match f alternate with
          | Ok () -> loop ~shrink_count ~alternates input error
          | Error error ->
            let alternates = Shrinker.shrink shrinker alternate in
            loop ~shrink_count ~alternates alternate error))
  in
  let shrink_count = Config.shrink_count config in
  let alternates = Shrinker.shrink shrinker input in
  loop ~shrink_count ~alternates input error
;;

let input_sequence ~config ~examples ~generator =
  let random = initial_random_state ~config in
  Sequence.append
    (Sequence.of_list examples)
    (one_size_per_test ~config
     |> Sequence.map ~f:(fun size -> Generator.generate generator ~size ~random))
;;

let with_sample ~f ?(config = default_config) ?(examples = []) generator =
  let sequence = input_sequence ~config ~examples ~generator in
  f sequence
;;

let result (type a) ~f ?(config = default_config) ?(examples = []) m =
  let (module M : S with type t = a) = m in
  with_sample M.quickcheck_generator ~config ~examples ~f:(fun sequence ->
    match
      Sequence.fold_result sequence ~init:() ~f:(fun () input ->
        match f input with
        | Ok () -> Ok ()
        | Error error -> Error (input, error))
    with
    | Ok () -> Ok ()
    | Error (input, error) ->
      let shrinker = M.quickcheck_shrinker in
      let input, error = shrink_error ~shrinker ~config ~f input error in
      Error (input, error))
;;

let run (type a) ~f ?config ?examples (module M : S with type t = a) =
  let f x =
    Or_error.try_with_join ~backtrace:(Backtrace.Exn.am_recording ()) (fun () -> f x)
  in
  match result ~f ?config ?examples (module M) with
  | Ok () -> Ok ()
  | Error (input, error) ->
    Or_error.error_s
      [%message "Base_quickcheck.Test.run: test failed" (input : M.t) (error : Error.t)]
;;

let with_sample_exn ~f ?config ?examples generator =
  let f x = Or_error.try_with (fun () -> f x) in
  with_sample ~f ?config ?examples generator |> Or_error.ok_exn
;;

let run_exn ~f ?config ?examples testable =
  let f x =
    Or_error.try_with ~backtrace:(Backtrace.Exn.am_recording ()) (fun () -> f x)
  in
  run ~f ?config ?examples testable |> Or_error.ok_exn
;;
