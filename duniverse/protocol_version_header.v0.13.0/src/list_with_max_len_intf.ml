open! Core_kernel

module type Config = sig
  val max_len : int
  val context : Info.t
end

module type S = sig
  type 'a t = private 'a list [@@deriving bin_io, sexp]

  (** [of_list_exn l] raises if [List.length l] is larger than the supplied [max_len]. *)
  val of_list_exn : 'a list -> 'a t
end

module type List_with_max_len = sig
  module type Config = Config
  module type S = S

  module Make (Config : Config) : S
end
