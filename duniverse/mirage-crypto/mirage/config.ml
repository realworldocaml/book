open Mirage

let main = foreign "Unikernel.Main" (random @-> job)

let () =
  let packages = [
    package "mirage-crypto-rng" ;
    package "mirage-crypto-pk" ;
    package "mirage-crypto" ;
    package ~min:"0.8.7" "fmt" ;
  ]
  in
  register ~packages "crypto-test" [main $ default_random]
