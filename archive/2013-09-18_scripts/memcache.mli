type t = File of string | Directory
type cache
val cache : string -> cache Lwt.t
val lookup : cache -> string -> t option
