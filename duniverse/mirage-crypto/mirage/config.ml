open Mirage

let main = foreign "Unikernel.Main" (random @-> job)

let () =
  let packages = [
    package "mirage-crypto-rng" ;
    package "mirage-crypto-pk" ;
    package "mirage-crypto"
  ]
  in
  register ~packages "crypto-test" [main $ default_random]
