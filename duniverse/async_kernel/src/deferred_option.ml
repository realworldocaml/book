open Core_kernel
module Deferred = Deferred1

module T = struct
  type 'a t = 'a Option.t Deferred.t
end

include T

include Monad.Make (struct
    include T

    let return a = Deferred.return (Some a)

    let bind t ~f =
      Deferred.bind t ~f:(function
        | Some a -> f a
        | None -> Deferred.return None)
    ;;

    let map t ~f = Deferred.map t ~f:(fun r -> Option.map r ~f)
    let map = `Custom map
  end)
