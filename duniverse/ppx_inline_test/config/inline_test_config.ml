module type S = sig
  val pre_test_hook : unit -> unit
end

let pre_test_hook = ignore
