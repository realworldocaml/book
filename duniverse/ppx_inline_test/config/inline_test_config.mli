(** Configuration for running inline tests *)

(** To configure inline_test, add the following at the top of your .ml file, or in some
    import.ml:

    {[
      module Inline_test_config = struct
        include Inline_test_config
        let pre_test_hook () = ...
      end
    ]}
*)

module type S = sig
  (** Run this function at the beginning of any test *)
  val pre_test_hook : unit -> unit
end

include S
