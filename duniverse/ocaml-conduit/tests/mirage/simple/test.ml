(* this is just to test that linking works properly *)

let client: Conduit_mirage.client =
  `TCP (Ipaddr.of_string_exn "127.0.0.1", 12345)

let server: Conduit_mirage.server =
  `TCP 12345

let _client () = Conduit_mirage.(connect empty) client
let _server () = Conduit_mirage.(listen empty) server
