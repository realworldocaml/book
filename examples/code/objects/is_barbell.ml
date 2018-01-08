let is_barbell = function
| [Circle r1; Line _; Circle r2] when r1 = r2 -> true
| _ -> false
