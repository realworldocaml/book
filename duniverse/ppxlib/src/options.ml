let perform_checks = false

(* The checks on extensions are only to get better error messages
   since the compiler will choke on unknown extensions. We disable
   them externally to make it easier to use non ppxlib based
   rewriters with ppxlib *)
let perform_checks_on_extensions = false
let perform_locations_check = false
let fail_on_duplicate_derivers = false
let diff_command = None
