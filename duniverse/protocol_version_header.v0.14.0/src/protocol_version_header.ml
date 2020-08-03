open! Core_kernel

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

module For_test = struct
  module Make_list_with_max_len = List_with_max_len.Make
end
