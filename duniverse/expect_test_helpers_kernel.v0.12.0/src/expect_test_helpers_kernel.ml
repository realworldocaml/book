open! Core_kernel
include Expect_test_helpers_base

include (
  Expect_test_helpers_kernel_intf :
    module type of struct
    include Expect_test_helpers_kernel_intf
  end
  with module Allocation_limit := Expect_test_helpers_kernel_intf.Allocation_limit)

module Allocation_limit = struct
  include Expect_test_helpers_kernel_intf.Allocation_limit

  let is_ok t ~major_words_allocated ~minor_words_allocated =
    match t with
    | Major_words n -> major_words_allocated <= n
    | Minor_words n -> major_words_allocated = 0 && minor_words_allocated <= n
  ;;

  let show_major_words = function
    | Major_words _ -> true
    | Minor_words _ -> false
  ;;
end

module type Int63able = sig
  type t

  val to_int63 : t -> Int63.t
  val of_int63_exn : Int63.t -> t
end

let print_and_check_stable_internal
      (type a)
      ?cr
      ?hide_positions
      ?max_binable_length
      here
      (module M : Stable_without_comparator with type t = a)
      (int63able : (module Int63able with type t = a) option)
      list
  =
  let equal = [%compare.equal: M.t] in
  print_s
    ?hide_positions
    [%message
      "" ~bin_shape_digest:(Bin_prot.Shape.eval_to_digest_string M.bin_shape_t : string)];
  require_does_not_raise ?cr ?hide_positions here (fun () ->
    List.iter list ~f:(fun original ->
      let sexp = M.sexp_of_t original in
      let bin_io = Binable.to_string (module M) original in
      let int63 = Option.map int63able ~f:(fun (module I) -> I.to_int63 original) in
      print_s
        ?hide_positions
        [%message "" (sexp : Sexp.t) (bin_io : string) (int63 : Int63.t sexp_option)];
      let sexp_roundtrip = M.t_of_sexp sexp in
      require
        ?cr
        ?hide_positions
        here
        (equal original sexp_roundtrip)
        ~if_false_then_print_s:
          (lazy
            [%message
              "sexp serialization failed to round-trip"
                (original : M.t)
                (sexp : Sexp.t)
                (sexp_roundtrip : M.t)]);
      let bin_io_roundtrip = Binable.of_string (module M) bin_io in
      require
        ?cr
        ?hide_positions
        here
        (equal original bin_io_roundtrip)
        ~if_false_then_print_s:
          (lazy
            [%message
              "bin-io serialization failed to round-trip"
                (original : M.t)
                (bin_io : string)
                (bin_io_roundtrip : M.t)]);
      (match max_binable_length with
       | None -> ()
       | Some max_binable_length ->
         let bin_io_length = String.length bin_io in
         require
           ?cr
           ?hide_positions
           here
           (bin_io_length <= max_binable_length)
           ~if_false_then_print_s:
             (lazy
               [%message
                 "bin-io serialization exceeds max binable length"
                   (original : M.t)
                   (bin_io : string)
                   (bin_io_length : int)
                   (max_binable_length : int)]));
      match int63able with
      | None -> ()
      | Some (module I) ->
        let int63 = Option.value_exn int63 in
        let int63_roundtrip = I.of_int63_exn int63 in
        require
          ?cr
          ?hide_positions
          here
          (equal original int63_roundtrip)
          ~if_false_then_print_s:
            (lazy
              [%message
                "int63 serialization failed to round-trip"
                  (original : M.t)
                  (int63 : Int63.t)
                  (int63_roundtrip : M.t)])))
;;

let print_and_check_stable_type
      (type a)
      ?cr
      ?hide_positions
      ?max_binable_length
      here
      (module M : Stable_without_comparator with type t = a)
      list
  =
  print_and_check_stable_internal
    ?cr
    ?hide_positions
    ?max_binable_length
    here
    (module M)
    None
    list
;;

let print_and_check_stable_int63able_type
      (type a)
      ?cr
      ?hide_positions
      ?max_binable_length
      here
      (module M : Stable_int63able with type t = a)
      list
  =
  print_and_check_stable_internal
    ?cr
    ?hide_positions
    ?max_binable_length
    here
    (module M)
    (Some (module M))
    list
;;

let prepare_heap_to_count_minor_allocation () =
  (* We call [Gc.minor] to empty the minor heap, so that our allocation is unlikely to
     trigger a minor gc. *)
  Gc.minor ();
  (* We allocate two words in case the [Gc.minor] finishes a major gc cycle, in which case
     it requests a minor gc to occur at the next minor allocation.  We don't want the
     subsequent minor allocation to trigger a minor GC, because there is a bug in the
     OCaml runtime that double counts [Gc.minor_words] in that case. *)
  ignore (Sys.opaque_identity (ref (Sys.opaque_identity 1)) : int ref)
;;

(* We disable inlining for [require_allocation_does_not_exceed] so the GC stats and the
   call to [f] are never rearranged. *)
let[@inline never] require_allocation_does_not_exceed
     ?(cr = CR.CR)
     ?hide_positions
     allocation_limit
     here
     f
  =
  prepare_heap_to_count_minor_allocation ();
  let minor_words_before = Gc.minor_words () in
  let major_words_before = Gc.major_words () in
  (* We wrap [f ()] with [Sys.opaque_identity] to prevent the return value from being
     optimized away. *)
  let x = Sys.opaque_identity (f ()) in
  let minor_words_after = Gc.minor_words () in
  let major_words_after = Gc.major_words () in
  let major_words_allocated = major_words_after - major_words_before in
  let minor_words_allocated = minor_words_after - minor_words_before in
  require
    here
    ~cr
    ?hide_positions
    (Allocation_limit.is_ok
       allocation_limit
       ~major_words_allocated
       ~minor_words_allocated)
    ~if_false_then_print_s:
      (lazy
        (let minor_words_allocated, major_words_allocated =
           if CR.hide_unstable_output cr
           then None, None
           else if major_words_allocated > 0
                || Allocation_limit.show_major_words allocation_limit
           then Some minor_words_allocated, Some major_words_allocated
           else Some minor_words_allocated, None
         in
         [%message
           "allocation exceeded limit"
             (allocation_limit : Allocation_limit.t)
             (minor_words_allocated : int sexp_option)
             (major_words_allocated : int sexp_option)]));
  x
;;

let require_no_allocation ?cr ?hide_positions here f =
  require_allocation_does_not_exceed ?cr ?hide_positions (Minor_words 0) here f
;;

let print_and_check_comparable_sexps
      (type a)
      ?cr
      ?hide_positions
      here
      (module M : With_comparable with type t = a)
      list
  =
  let set = M.Set.of_list list in
  let set_sexp = [%sexp (set : M.Set.t)] in
  print_s [%message "Set" ~_:(set_sexp : Sexp.t)];
  let sorted_list_sexp = [%sexp (List.sort list ~compare:M.compare : M.t list)] in
  require
    ?cr
    ?hide_positions
    here
    (Sexp.equal set_sexp sorted_list_sexp)
    ~if_false_then_print_s:
      (lazy
        [%message
          "set sexp does not match sorted list sexp"
            (set_sexp : Sexp.t)
            (sorted_list_sexp : Sexp.t)]);
  let alist = List.mapi list ~f:(fun i x -> x, i) in
  let map = M.Map.of_alist_exn alist in
  let map_sexp = [%sexp (map : int M.Map.t)] in
  print_s [%message "Map" ~_:(map_sexp : Sexp.t)];
  let sorted_alist_sexp =
    [%sexp
      (List.sort alist ~compare:(fun (x, _) (y, _) -> M.compare x y) : (M.t * int) list)]
  in
  require
    ?cr
    ?hide_positions
    here
    (Sexp.equal map_sexp sorted_alist_sexp)
    ~if_false_then_print_s:
      (lazy
        [%message
          "map sexp does not match sorted alist sexp"
            (map_sexp : Sexp.t)
            (sorted_alist_sexp : Sexp.t)])
;;

let print_and_check_hashable_sexps
      (type a)
      ?cr
      ?hide_positions
      here
      (module M : With_hashable with type t = a)
      list
  =
  let hash_set = M.Hash_set.of_list list in
  let hash_set_sexp = [%sexp (hash_set : M.Hash_set.t)] in
  print_s [%message "Hash_set" ~_:(hash_set_sexp : Sexp.t)];
  let sorted_list_sexp = [%sexp (List.sort list ~compare:M.compare : M.t list)] in
  require
    ?cr
    ?hide_positions
    here
    (Sexp.equal hash_set_sexp sorted_list_sexp)
    ~if_false_then_print_s:
      (lazy
        [%message
          "hash_set sexp does not match sorted list sexp"
            (hash_set_sexp : Sexp.t)
            (sorted_list_sexp : Sexp.t)]);
  let alist = List.mapi list ~f:(fun i x -> x, i) in
  let table = M.Table.of_alist_exn alist in
  let table_sexp = [%sexp (table : int M.Table.t)] in
  print_s [%message "Table" ~_:(table_sexp : Sexp.t)];
  let sorted_alist_sexp =
    [%sexp
      (List.sort alist ~compare:(fun (x, _) (y, _) -> M.compare x y) : (M.t * int) list)]
  in
  require
    ?cr
    ?hide_positions
    here
    (Sexp.equal table_sexp sorted_alist_sexp)
    ~if_false_then_print_s:
      (lazy
        [%message
          "table sexp does not match sorted alist sexp"
            (table_sexp : Sexp.t)
            (sorted_alist_sexp : Sexp.t)])
;;

let print_and_check_container_sexps (type a) ?cr ?hide_positions here m list =
  let (module M : With_containers with type t = a) = m in
  print_and_check_comparable_sexps ?cr ?hide_positions here (module M) list;
  print_and_check_hashable_sexps ?cr ?hide_positions here (module M) list
;;
