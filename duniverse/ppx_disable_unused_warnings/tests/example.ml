(* Make sure [@@@disable_unused_warnings] gets translated properly *)
[@@@disable_unused_warnings]

(* Make sure [@disable_unused_warnings] gets translated properly *)
let[@disable_unused_warnings] succ x = x + 1

(* Make sure [@@disable_unused_warnings] gets translated properly *)
let pred x = x - 1 [@@disable_unused_warnings]

(* Make sure other attributes are left untouched *)
let[@disable_unused_warnings] id x = (x [@ocaml.warning "-8"])
