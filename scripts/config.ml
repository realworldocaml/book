let client_id = "1605562d79611cd6d8bc"
let client_secret = "1cd154a261b6240827b89cd3a0461790ac27be85"
let auth = Cohttp.Auth.Basic ("rwo", "Whirly2")

(* List of allowed Github users per milestone *)
let authors = ["yminsky";"jyh";"avsm";"andyoram"]
let cambridge = ["amirmc";"lpw25";"djs55";"mcclurmc"]
let from_yminsky =
  [ "naftul"                  (* nminsky@gmail.com    *)
  ; "fbsATcsDOTcornellDOTedu" (* fbs@cs.cornell.edu   *)
  ; "zdancewic"               (* stevez@cis.upenn.edu *)
  ]
let allowed_users = function
  | "alpha1" -> authors @ cambridge
  | "alpha2" -> authors @ cambridge @ from_yminsky
  |_ -> authors
