open! Core
open! Import
module Unix := Core_unix

include module type of struct
  include Patdiff_kernel.Compare_core
end

include Patdiff_kernel.Compare_core.S

val diff_files
  :  Configuration.t
  -> prev_file:string
  -> next_file:string
  -> [ `Different | `Same ]

val diff_dirs
  :  Configuration.t
  -> prev_dir:string
  -> next_dir:string
  -> file_filter:(string * Unix.stats -> bool) option
  -> [ `Different | `Same ]
