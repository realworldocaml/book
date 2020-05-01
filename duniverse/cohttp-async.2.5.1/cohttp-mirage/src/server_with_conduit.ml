
include Make.Server(Conduit_mirage.Flow)

let connect t =
  let listen s f = Conduit_mirage.listen t s (listen f) in
  Lwt.return listen
