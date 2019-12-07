open! Import

module type S = sig
  type 'a t [@@deriving bin_io]

  (** @open *)
  include Base.Queue.S with type 'a t := 'a t
end
