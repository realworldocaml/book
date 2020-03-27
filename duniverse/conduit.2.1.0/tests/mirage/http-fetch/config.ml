open Mirage


let client =
  foreign ~deps:[abstract nocrypto] "Unikernel.Client" @@ console @-> stackv4 @-> job

let () =
  register
    ~libraries:[ "conduit.lwt"; "conduit.mirage"; "dns.mirage" ]
    ~packages:[ "mirage-dns"; "conduit" ]
    "conduit-client" [ client $ default_console $ generic_stackv4 default_console tap0 ]
