val format : ?std:bool -> json -> Easy_format.t
val to_string : ?std:bool -> json -> string
val to_channel : ?std:bool -> out_channel -> json -> unit
