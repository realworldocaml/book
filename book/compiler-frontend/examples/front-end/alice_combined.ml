module Alice : sig
  val friends : Bob.t list
end = struct
  let friends = [ Bob.name ]
end
