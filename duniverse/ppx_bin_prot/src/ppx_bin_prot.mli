open Ppxlib

val bin_shape : Deriving.t
val bin_write : Deriving.t
val bin_read : Deriving.t
val bin_type_class : Deriving.t
val bin_io : Deriving.t

module For_f_sharp : sig
  val bin_write
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> structure_item list

  val bin_read
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> structure_item list
end
