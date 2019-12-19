
type nonrec int         = int       [@@deriving bin_shape ~basetype:"int"]
type nonrec float       = float     [@@deriving bin_shape ~basetype:"float"]
type nonrec string      = string    [@@deriving bin_shape ~basetype:"string"]
type nonrec 'a list     = 'a list   [@@deriving bin_shape ~basetype:"list"]

module M1 = struct
  type t   = int * float u * string list
  and 'a u = { foo : 'a; bar : t list }
  [@@deriving bin_shape]
end

module M2 : sig
  type t [@@deriving bin_shape]
end = struct
  type t = int [@@deriving bin_shape]
end

module M3 = struct

  type t = int [@@deriving bin_shape]
  type u = t list [@@deriving bin_shape]

  type v = int list [@@deriving bin_shape]

end
