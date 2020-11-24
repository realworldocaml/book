open Bin_prot.Std

type t = float [@@deriving bin_io]

module M : sig
  type t = float [@@deriving bin_io]
end = struct
  type nonrec t = t [@@deriving bin_io]
end

module M1 : sig
  type t = float list [@@deriving bin_io]
end = struct
  type nonrec t = t list [@@deriving bin_io]
end

module M2 : sig
  type nonrec t = t list [@@deriving bin_io]
end = struct
  type nonrec t = t list [@@deriving bin_io]
end

module M3 : sig
  type nonrec t = [ `A of t ] [@@deriving bin_io]
end = struct
  type nonrec t = [ `A of t ] [@@deriving bin_io]
end
