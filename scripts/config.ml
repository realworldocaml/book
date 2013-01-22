let client_id = "1605562d79611cd6d8bc"
let client_secret = "1cd154a261b6240827b89cd3a0461790ac27be85"
let auth = Cohttp.Auth.Basic ("rwo", "Whirly2")

(* List of allowed Github users per milestone *)
let authors = ["yminsky";"jyh";"avsm";"andyoram"]
let support = ["jamescotton"] 

let from_anil =
  [ "mcclurmc"                (* Mike McClurg <mike.mcclurg@citrix.com> *)
  ; "djs55"                   (* David Scott <dave.scott@citrix.com> *)
  ; "lpw25"                   (* Leo White <lpw25@cam.ac.uk> *)
  ; "arb33"                   (* arb33@cam.ac.uk *)
  ; "amirmc"                  (* Amir Chaudhry <amirmc@gmail.com> *)
  ; "heidi-ann"               (* Heidi Howard <hh360@cam.ac.uk> *)
  ; "samoht"                  (* Thomas Gazagnaire <thomas@ocamlpro.com> *)
  ; "udita12"                 (* Udita Gangwal <psxug2@nottingham.ac.uk> *)
  ; "bishneet"                (* Bishneet Kaur <psxbk3@nottingham.ac.uk> *)
  ; "mor1"                    (* Richard Mortier <richard.mortier@nottingham.ac.uk> *)
  ; "jamesbulpin"             (* James Bulpin <james.bulpin@citrix.com> *)
  ; "raphael-proust"          (* Raphael Proust <rp452@cam.ac.uk> *)
  ; "mariusaeriksen"          (* Marius Eriksen <marius@twitter.com> *)
  ; "jaked"                   (* Jake Donham <jake@donham.org> *)
]

let from_yminsky =
  [ "naftul"                  (* nminsky@gmail.com    *)
  ; "fbsATcsDOTcornellDOTedu" (* fbs@cs.cornell.edu   *)
  ; "zdancewic"               (* stevez@cis.upenn.edu *)
  ; "bolinfest"               (* mbolin@gmail.com *)
  ]

let from_email = 
  [ "JakubOboza "             (* jakub.oboza@gmail.com *)
  ; "gour"                    (* gour@atmarama.net *)
  ; "getauvi"                 (* getauvi@gmail.com *)
  ; "pdonadeo"                (* Paolo Donadeo <p.donadeo@gmail.com>  *)
  ; "braibant"                (* Thomas Braibant <thomas.braibant@gmail.com> *)
  ; "xmonader"                (* Ahmed Youssef <xmonader@gmail.com> *)
  ; "gildor478"               (* Sylvain Le Gall <sylvain@le-gall.net> *)
  ; "voila"                   (* manu delab <manu.delab@gmail.com> *)
  ; "thadmeyer"               (* Thad Meyer <tmeyer@members.fsf.org> *)
]

let internal_reviewers =
  authors @ support

let external_reviewers =
  internal_reviewers @ from_anil @ from_yminsky @ from_email

let allowed_users = function
  | "alpha1" -> internal_reviewers
  | "alpha2" -> external_reviewers
  |_ -> authors
