module List = List

module Enumerable = struct
  module type S = sig
    type t

    val all : t list
  end

  module type S1 = sig
    type 'a t

    val all : 'a list -> 'a t list
  end

  module type S2 = sig
    type ('a, 'b) t

    val all : 'a list -> 'b list -> ('a, 'b) t list
  end

  module type S3 = sig
    type ('a, 'b, 'c) t

    val all : 'a list -> 'b list -> 'c list -> ('a, 'b, 'c) t list
  end
end
