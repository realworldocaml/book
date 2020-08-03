type t = Default | User_defined of string

let mk = function None | Some "" -> Default | Some s -> User_defined s

let name = function Default -> "" | User_defined s -> s

type env = t

module Set = Set.Make (struct
  type t = env

  let compare = compare
end)
