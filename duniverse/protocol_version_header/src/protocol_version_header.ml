open! Core
module Known_protocol = Known_protocol

module Bounded_list_in_case_someone_sends_garbage_on_the_wire =
  List_with_max_len.Make (struct
    let max_len = 100
    let context = Info.of_string "Protocol_version_header"
  end)

type t = int Bounded_list_in_case_someone_sends_garbage_on_the_wire.t
[@@deriving bin_io, sexp]

let create_exn ~protocol ~supported_versions =
  Known_protocol.magic_number protocol :: supported_versions
  |> Bounded_list_in_case_someone_sends_garbage_on_the_wire.of_list_exn
;;

let get_protocol (t : t) =
  let protocols, versions =
    List.partition_map
      (t :> int list)
      ~f:(fun v ->
        match Map.find Known_protocol.by_magic_number v with
        | Some p -> First p
        | None -> Second v)
  in
  match protocols with
  | [] -> Ok (None, Int.Set.of_list versions)
  | [ p ] -> Ok (Some p, Int.Set.of_list versions)
  | _ ->
    Or_error.error_s
      [%message
        "[Protocol_version_header.negotiate]: multiple magic numbers seen."
          (protocols : Known_protocol.t list)
          (versions : int list)]
;;

let negotiate ~allow_legacy_peer ~(us : t) ~(peer : t) =
  let open Or_error.Let_syntax in
  let%bind us_protocol, us_versions = get_protocol us in
  let%bind peer_protocol, peer_versions = get_protocol peer in
  let%bind us_protocol =
    match us_protocol with
    | Some x -> return x
    | None -> error_s [%message "No magic numbers seen" (us_versions : Int.Set.t)]
  in
  let%bind peer_protocol =
    match peer_protocol with
    | Some x -> return x
    | None ->
      (* we assume peer is speaking our protocol if [allow_legacy_peer] *)
      if allow_legacy_peer
      then return us_protocol
      else (
        let peer_protocol = `Unknown in
        Or_error.error_s
          [%message
            "[Protocol_version_header.negotiate]: conflicting magic protocol numbers"
              (us_protocol : Known_protocol.t)
              (peer_protocol : [ `Unknown ])])
  in
  if not ([%compare.equal: Known_protocol.t] us_protocol peer_protocol)
  then
    Or_error.error_s
      [%message
        "[Protocol_version_header.negotiate]: conflicting magic protocol numbers"
          (us_protocol : Known_protocol.t)
          (peer_protocol : Known_protocol.t)]
  else (
    let protocol = us_protocol in
    match Set.max_elt (Set.inter us_versions peer_versions) with
    | Some version -> Ok version
    | None ->
      Or_error.error_s
        [%message
          "[Protocol_version_header.negotiate]: no shared version numbers"
            (us_versions : Int.Set.t)
            (peer_versions : Int.Set.t)
            (protocol : Known_protocol.t)])
;;

let matches_magic_prefix (t : t) ~protocol =
  let magic_number = Known_protocol.magic_number protocol in
  List.mem ~equal:Int.equal (t :> int list) magic_number
;;

let contains_magic_prefix ~protocol =
  Bin_prot.Type_class.cnv_reader (matches_magic_prefix ~protocol) bin_t.reader
;;

let any_magic_prefix =
  let f t =
    List.find Known_protocol.all ~f:(fun protocol -> matches_magic_prefix ~protocol t)
  in
  Bin_prot.Type_class.cnv_reader f bin_t.reader
;;

module Magic_prefix_bin_repr = struct
  type t = int [@@deriving bin_shape, bin_write]

  (* The bin prot representation of a protocol version header is the standard
     representation for an int list:

     | nat0 indicating size | element 0 | element 1 | ...

     [create_exn] will always put the known protocol magic number as "element 0".

     The bin size of "nat0 indicating size" and "element 0" is fixed (there are expect
     tests to make sure we never change them). *)
  let bin_size =
    bin_size_t Bounded_list_in_case_someone_sends_garbage_on_the_wire.max_len
    + Known_protocol.magic_number_bin_size
  ;;

  let bin_read_t buf ~pos_ref =
    let (_list_length : Bin_prot.Nat0.t) = Bin_prot.Read.bin_read_nat0 buf ~pos_ref in
    Bin_prot.Read.bin_read_int buf ~pos_ref
  ;;

  let bin_reader_t =
    { Bin_prot.Type_class.read = bin_read_t; vtag_read = Int.__bin_read_t__ }
  ;;
end

let any_magic_prefix_from_six_bytes =
  Bin_prot.Type_class.cnv_reader
    (fun magic_number -> (Map.find Known_protocol.by_magic_number) magic_number)
    Magic_prefix_bin_repr.bin_reader_t
;;

let any_magic_prefix_from_six_bytes_bin_size = Magic_prefix_bin_repr.bin_size

module For_test = struct
  module Make_list_with_max_len = List_with_max_len.Make
end

let%test_unit "bin sizes are not changed by accident" =
  (* Ensure the bin_size of Bounded_list_in_case_someone_sends_garbage_on_the_wire.max_len
     is always 1. This means that regardless of how long the list of versions in the
     protocol header is, the representation will be the same number of bytes. The
     representation of an int jumps to 3 bytes at the value 128. *)
  let bounded_list_bin_size =
    Int.bin_size_t Bounded_list_in_case_someone_sends_garbage_on_the_wire.max_len
  in
  assert (bounded_list_bin_size = 1);
  assert (6 = Known_protocol.magic_number_bin_size + bounded_list_bin_size);
  assert (
    any_magic_prefix_from_six_bytes_bin_size
    = Known_protocol.magic_number_bin_size + bounded_list_bin_size)
;;
