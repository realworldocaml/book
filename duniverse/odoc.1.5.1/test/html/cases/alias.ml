module Foo__X = struct
  (** Module Foo__X documentation. This should appear in the documentation
      for the alias to this module 'X' *)
  type t = int
end

module X = Foo__X
