open! Import

let default_print_warning _loc = ()

let about_ite_branch_ref = ref default_print_warning

let care_about_ite_branch = ref false
let about_ite_branch loc = !about_ite_branch_ref loc
