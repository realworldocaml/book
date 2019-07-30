include (
  Base.Type_equal :
    module type of struct
    include Base.Type_equal
  end
  with module Id := Base.Type_equal.Id)

module Id = struct
  include (
    Base.Type_equal.Id :
      module type of struct
      include Base.Type_equal.Id
    end
    with module Uid := Base.Type_equal.Id.Uid)

  module Uid = struct
    module Upstream = Base.Type_equal.Id.Uid
    include Base.Type_equal.Id.Uid

    include Comparable.Extend
        (Upstream)
        (struct
          type t = Base.Type_equal.Id.Uid.t [@@deriving sexp]
        end)

    include Hashable.Make (Upstream)
  end
end
