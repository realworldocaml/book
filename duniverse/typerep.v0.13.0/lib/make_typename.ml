open Std_internal

module Make0 (X : Named_intf.S0) = struct
  module Name_of_x = Typename.Make0 (X)
  let typename_of_t = Name_of_x.typename_of_t
  let named =
    Typerep.Named.T0 (module struct
      type named = X.t
      type t = X.t
      let typename_of_named = Name_of_x.typename_of_t
      let typename_of_t = typename_of_t
      let witness = Type_equal.refl
    end : Typerep.Named.T0 with type t = X.t)
end

module Make1 (X : Named_intf.S1) = struct
  module Name_of_x = Typename.Make1 (X)
  let typename_of_t = Name_of_x.typename_of_t
  let named (type p1) of_p1 =
    let typename_of_t = Name_of_x.typename_of_t
      (Typerep.typename_of_t of_p1)
    in
    Typerep.Named.T1 (module struct
      type 'a named = 'a X.t
      type a = p1 let a = of_p1
      type t = p1 X.t
      let typename_of_named = Name_of_x.typename_of_t
      let typename_of_t = typename_of_t
      let witness = Type_equal.refl
    end : Typerep.Named.T1 with type t = p1 X.t)
end

module Make2 (X : Named_intf.S2) = struct
  module Name_of_x = Typename.Make2 (X)
  let typename_of_t = Name_of_x.typename_of_t
  let named (type p1) (type p2) of_p1 of_p2 =
    let typename_of_t = Name_of_x.typename_of_t
      (Typerep.typename_of_t of_p1)
      (Typerep.typename_of_t of_p2)
    in
    Typerep.Named.T2 (module struct
      type ('a, 'b) named = ('a, 'b) X.t
      type a = p1 let a = of_p1
      type b = p2 let b = of_p2
      type t = (p1, p2) X.t
      let typename_of_named = Name_of_x.typename_of_t
      let typename_of_t = typename_of_t
      let witness = Type_equal.refl
    end : Typerep.Named.T2 with type t = (p1, p2) X.t)
end

module Make3 (X : Named_intf.S3) = struct
  module Name_of_x = Typename.Make3 (X)
  let typename_of_t = Name_of_x.typename_of_t
  let named (type p1) (type p2) (type p3) of_p1 of_p2 of_p3 =
    let typename_of_t = Name_of_x.typename_of_t
      (Typerep.typename_of_t of_p1)
      (Typerep.typename_of_t of_p2)
      (Typerep.typename_of_t of_p3)
    in
    Typerep.Named.T3 (module struct
      type ('a, 'b, 'c) named = ('a, 'b, 'c) X.t
      type a = p1 let a = of_p1
      type b = p2 let b = of_p2
      type c = p3 let c = of_p3
      type t = (p1, p2, p3) X.t
      let typename_of_named = Name_of_x.typename_of_t
      let typename_of_t = typename_of_t
      let witness = Type_equal.refl
    end : Typerep.Named.T3 with type t = (p1, p2, p3) X.t)
end

module Make4 (X : Named_intf.S4) = struct
  module Name_of_x = Typename.Make4 (X)
  let typename_of_t = Name_of_x.typename_of_t
  let named (type p1) (type p2) (type p3) (type p4) of_p1 of_p2 of_p3 of_p4 =
    let typename_of_t = Name_of_x.typename_of_t
      (Typerep.typename_of_t of_p1)
      (Typerep.typename_of_t of_p2)
      (Typerep.typename_of_t of_p3)
      (Typerep.typename_of_t of_p4)
    in
    Typerep.Named.T4 (module struct
      type ('a, 'b, 'c, 'd) named = ('a, 'b, 'c, 'd) X.t
      type a = p1 let a = of_p1
      type b = p2 let b = of_p2
      type c = p3 let c = of_p3
      type d = p4 let d = of_p4
      type t = (p1, p2, p3, p4) X.t
      let typename_of_named = Name_of_x.typename_of_t
      let typename_of_t = typename_of_t
      let witness = Type_equal.refl
    end : Typerep.Named.T4 with type t = (p1, p2, p3, p4) X.t)
end

module Make5 (X : Named_intf.S5) = struct
  module Name_of_x = Typename.Make5 (X)
  let typename_of_t = Name_of_x.typename_of_t
  let named
      (type p1) (type p2) (type p3) (type p4) (type p5)
      of_p1 of_p2 of_p3 of_p4 of_p5 =
    let typename_of_t = Name_of_x.typename_of_t
      (Typerep.typename_of_t of_p1)
      (Typerep.typename_of_t of_p2)
      (Typerep.typename_of_t of_p3)
      (Typerep.typename_of_t of_p4)
      (Typerep.typename_of_t of_p5)
    in
    Typerep.Named.T5 (module struct
      type ('a, 'b, 'c, 'd, 'e) named = ('a, 'b, 'c, 'd, 'e) X.t
      type a = p1 let a = of_p1
      type b = p2 let b = of_p2
      type c = p3 let c = of_p3
      type d = p4 let d = of_p4
      type e = p5 let e = of_p5
      type t = (p1, p2, p3, p4, p5) X.t
      let typename_of_named = Name_of_x.typename_of_t
      let typename_of_t = typename_of_t
      let witness = Type_equal.refl
    end : Typerep.Named.T5 with type t = (p1, p2, p3, p4, p5) X.t)
end
