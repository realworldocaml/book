open Mirage

let secrets_dir = "sekrit"

let build =
  try
    match Sys.getenv "BUILD" with
    | "client" -> `Client
    | _ -> `Server
  with Not_found -> `Server

let disk = generic_kv_ro secrets_dir

let stack = generic_stackv4 default_network

let packages = [
  package ~sublibs:["mirage"] "tls" ;
  package ~sublibs:["lwt"] "logs"
]

let server =
  foreign ~deps:[abstract nocrypto] ~packages "Unikernel.Server" @@ stackv4 @-> kv_ro @-> pclock @-> job

let client =
  foreign ~deps:[abstract nocrypto] ~packages "Unikernel.Client" @@ stackv4 @-> kv_ro @-> pclock @-> job

let () =
  match build with
  | `Server ->
      register "tls-server" [ server $ stack $ disk $ default_posix_clock ]
  | `Client ->
      register "tls-client" [ client $ stack $ disk $ default_posix_clock ]
