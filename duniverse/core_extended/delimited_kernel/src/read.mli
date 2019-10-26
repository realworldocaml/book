(** @inline *)
include Read_intf.Root

(** Experts only. If you really think you need a function in this module,
    please talk to a delimited dev first. *)
module Expert :
  Read_intf.Expert with module On_invalid_row := On_invalid_row and type 'a t := 'a t
