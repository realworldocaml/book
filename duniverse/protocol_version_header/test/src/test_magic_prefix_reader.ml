open! Core

let read
      info
      buf
      (reader : Protocol_version_header.Known_protocol.t option Bin_prot.Type_class.reader)
  =
  let pos_ref = ref 0 in
  match reader.read buf ~pos_ref with
  | None ->
    raise_s [%message "failed to read magic prefix" (Bigstring.to_string buf : string)]
  | Some known_protocol ->
    printf
      !"%s -> known_protocol: %{sexp: Protocol_version_header.Known_protocol.t}, \
        bytes_read: %d\n"
      info
      known_protocol
      !pos_ref
;;

let test protocol_version_header =
  let buf =
    Bigstring.create (Protocol_version_header.bin_size_t protocol_version_header)
  in
  let bytes_written =
    Protocol_version_header.bin_writer_t.write buf ~pos:0 protocol_version_header
  in
  printf "bytes written to the buffer: %d\n" bytes_written;
  read "any_magic_prefix" buf Protocol_version_header.any_magic_prefix;
  read
    "any_magic_prefix_from_fixed_bytes"
    buf
    Protocol_version_header.any_magic_prefix_from_six_bytes
;;

let%expect_test "Test magic prefix reader" =
  test (Protocol_version_header.create_exn ~protocol:Krb ~supported_versions:[]);
  [%expect
    {|
    bytes written to the buffer: 6
    any_magic_prefix -> known_protocol: Krb, bytes_read: 6
    any_magic_prefix_from_fixed_bytes -> known_protocol: Krb, bytes_read: 6 |}];
  test (Protocol_version_header.create_exn ~protocol:Krb ~supported_versions:[ 1 ]);
  [%expect
    {|
    bytes written to the buffer: 7
    any_magic_prefix -> known_protocol: Krb, bytes_read: 7
    any_magic_prefix_from_fixed_bytes -> known_protocol: Krb, bytes_read: 6 |}];
  test (Protocol_version_header.create_exn ~protocol:Rpc ~supported_versions:[ 1; 2; 3 ]);
  [%expect
    {|
    bytes written to the buffer: 9
    any_magic_prefix -> known_protocol: Rpc, bytes_read: 9
    any_magic_prefix_from_fixed_bytes -> known_protocol: Rpc, bytes_read: 6 |}]
;;
