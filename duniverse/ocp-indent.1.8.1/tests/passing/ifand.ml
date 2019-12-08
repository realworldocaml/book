let _ =
  if cond1
  && cond2
  then _

let _ = function
  | _ when x = 2
        && y = 3 ->
    begin if a = b
          || b = c
             && c = d then
        _
    end
