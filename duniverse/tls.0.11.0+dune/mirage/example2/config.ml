open Mirage

let secrets_dir = "sekrit"

let disk  = direct_kv_ro secrets_dir
and stack = generic_stackv4 default_network

let packages = [
  package "cohttp-mirage" ;
  package ~min:"0.99" "cohttp-lwt" ;
  package ~sublibs:["mirage"] "tls" ;
  package "tcpip" ;
]
let server = foreign ~deps:[abstract nocrypto] ~packages "Unikernel.Main" @@ stackv4 @-> kv_ro @-> pclock @-> job

let () =
  register "tls-server" [ server $ stack $ disk $ default_posix_clock ]
