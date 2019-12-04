external ( @* ) : ('a -> 'b) -> 'a -> 'b = "%apply"
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"


module String = struct
  include String

  let is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false
end
