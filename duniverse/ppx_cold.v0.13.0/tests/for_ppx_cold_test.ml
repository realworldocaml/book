(* Make sure [@cold] gets translated properly *)
let[@cold] succ x = x + 1

(* Make sure [@@cold] gets translated properly *)
let pred x = x - 1 [@@cold]

(* Make sure other attributes are left untouched *)
let[@cold] id x = x [@ocaml.warning "-8"]
