open! Base
open Ppxlib
open Ppx_quickcheck_expander

let (_ : Deriving.t) = Deriving.add "quickcheck" ~sig_type_decl ~str_type_decl
let (_ : Deriving.t) = Deriving.add "quickcheck.generator" ~extension:generator_extension
let (_ : Deriving.t) = Deriving.add "quickcheck.observer" ~extension:observer_extension
let (_ : Deriving.t) = Deriving.add "quickcheck.shrinker" ~extension:shrinker_extension
