module Stubs = functor (S : Cstubs_structs.TYPE) -> struct
  module Tm = struct
    type tm
    type t = tm Ctypes.structure
    let t : t S.typ = S.structure "tm"
    let tm_hour = S.(field t "tm_hour" int)
    let tm_year = S.(field t "tm_year" int)

    let () = S.seal t
  end

  module Limits = struct
    let shrt_max = S.(constant "SHRT_MAX" short)
  end
end
