open Core_kernel
module Deferred = Deferred0
include Ivar0

let read = Deferred.of_ivar
let fill_if_empty t v = if is_empty t then fill t v

include Binable.Of_binable1_without_uuid [@alert "-legacy"]
    (Option)
    (struct
      type nonrec 'a t = 'a t

      let to_binable t = peek t

      let of_binable = function
        | None -> create ()
        | Some a -> create_full a
      ;;
    end)
