open! Base
open  Ppxlib
open  Ast_builder.Default

let loc = location ~start:[%here] ~end_:[%here] ~ghost:true

(* These three are okay: *)

include struct
  let _ = [%expr `x]
  let _ = ()
end

include struct
  let _ = [%type: [`x]]
  let _ = ()
end

include struct
  let _ = [%pat? `x]
  let _ = ()
end

(* These four cause the following line to jump back all the way to the left: *)

include struct
  let _ = [%stri let () = ();;]
  let _ = ()
end

include struct
  let _ = [%str let () = ();;]
  let _ = ()
end

include struct
  let _ = [%sigi: val x : int]
  let _ = ()
end

include struct
  let _ = [%sig: val x : int]
  let _ = ()
end
