open Std_internal

module Make0 (X : Named_intf.S0) : Typerepable.S
  with type t := X.t
= struct
  module M = Make_typename.Make0(X)
  let typerep_of_t =
    Typerep.Named (M.named, None)
  let typename_of_t = M.typename_of_t
end

module Make1 (X : Named_intf.S1) : Typerepable.S1
  with type 'a t := 'a X.t
= struct
  module M = Make_typename.Make1(X)
  let typerep_of_t of_p1 =
    Typerep.Named (M.named of_p1, None)
  let typename_of_t = M.typename_of_t
end

module Make2 (X : Named_intf.S2) : Typerepable.S2
  with type ('a, 'b) t := ('a, 'b) X.t
= struct
  module M = Make_typename.Make2(X)
  let typerep_of_t of_p1 of_p2 =
    Typerep.Named (M.named of_p1 of_p2, None)
  let typename_of_t = M.typename_of_t
end

module Make3 (X : Named_intf.S3) : Typerepable.S3
  with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t
= struct
  module M = Make_typename.Make3(X)
  let typerep_of_t of_p1 of_p2 of_p3 =
    Typerep.Named (M.named of_p1 of_p2 of_p3, None)
  let typename_of_t = M.typename_of_t
end

module Make4 (X : Named_intf.S4) : Typerepable.S4
  with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) X.t
= struct
  module M = Make_typename.Make4(X)
  let typerep_of_t of_p1 of_p2 of_p3 of_p4 =
    Typerep.Named (M.named of_p1 of_p2 of_p3 of_p4, None)
  let typename_of_t = M.typename_of_t
end

module Make5 (X : Named_intf.S5) : Typerepable.S5
  with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) X.t
= struct
  module M = Make_typename.Make5(X)
  let typerep_of_t of_p1 of_p2 of_p3 of_p4 of_p5 =
    Typerep.Named (M.named of_p1 of_p2 of_p3 of_p4 of_p5, None)
  let typename_of_t = M.typename_of_t
end
