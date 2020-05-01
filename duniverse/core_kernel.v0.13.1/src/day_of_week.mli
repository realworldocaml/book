(** Provides a variant type for days of the week ([Mon], [Tue], etc.) and convenience
    functions for converting these days into other formats, like seconds since the epoch.
*)

include Day_of_week_intf.Day_of_week (** @inline *)
