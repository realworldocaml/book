open Base
open Import

let lookup_and_apply map key x = Option.apply (Map.find map key) x
