module M (X : sig type 'a t end) = struct
  module type S = sig
    type t
    include Typerepable.S with type t := t
    val compute : t X.t
  end

  module type S1 = sig
    type 'a t
    include Typerepable.S1 with type 'a t := 'a t
    val compute : 'a X.t -> 'a t X.t
  end

  module type S2 = sig
    type ('a, 'b) t
    include Typerepable.S2 with type ('a, 'b) t := ('a, 'b) t
    val compute : 'a X.t -> 'b X.t -> ('a, 'b) t X.t
  end

  module type S3 = sig
    type ('a, 'b, 'c) t
    include Typerepable.S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    val compute :
      'a X.t
      -> 'b X.t
      -> 'c X.t
      -> ('a, 'b, 'c) t X.t
  end

  module type S4 = sig
    type ('a, 'b, 'c, 'd) t
    include Typerepable.S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) t
    val compute :
      'a X.t
      -> 'b X.t
      -> 'c X.t
      -> 'd X.t
      -> ('a, 'b, 'c, 'd) t X.t
  end

  module type S5 = sig
    type ('a, 'b, 'c, 'd, 'e) t
    include Typerepable.S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) t
    val compute :
      'a X.t
      -> 'b X.t
      -> 'c X.t
      -> 'd X.t
      -> 'e X.t
      -> ('a, 'b, 'c, 'd, 'e) t X.t
  end
end

module type S = sig
  type 'a t
  include (module type of
            M (struct
              type 'a computation = 'a t
              type 'a t = 'a computation
            end)
          )
end
