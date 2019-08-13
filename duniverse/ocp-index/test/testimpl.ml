let v = 0

module StringOrder : Map.OrderedType = struct
  type t = string
  let compare = String.compare
end

module StringMap = Map.Make(StringOrder)

module X = String

module MyString = struct type t = string end

module AAA = struct
  type t0 = int
  let z : t0 = 44
  type t1 = Cstr | Dstr
  let w' = Cstr
  module type S = sig type t end
  module M : S = struct type t = int end
  let s : MyString.t = "s"
end
module M : AAA.S = struct type t = int end

type t = int
let x : t = 3
let y : AAA.t0 = 4

let w = AAA.Cstr
type t2 = AAA.t1
