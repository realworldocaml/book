open! Import
open Uniform_array

module Sequence = struct
  type nonrec 'a t = 'a t
  type 'a z = 'a
  let length = length
  let get = get
  let set = set
  let create_bool ~len = create ~len false
end

include Base_for_tests.Test_blit.Test1(Sequence)(Uniform_array)
