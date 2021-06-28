module Foo = struct
    module A = struct
        type t = A
    end
    module B = struct
        type t = B
    end
end

open (Foo : module type of Foo with module A := Foo.A)

module B = B

open Set.Make(struct type t = Foo.A.t let compare = compare end)

type u = t

module type S = sig
    module F: sig end -> sig type t end
    module X: sig end
    open F(X)
    val f: t
end

open Foo

(* Check that regular open still works as expected *)
module B' = B

