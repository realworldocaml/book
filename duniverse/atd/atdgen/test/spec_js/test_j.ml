open Spec_js.J

let make f x =
  Yojson.Safe.from_string (f x)

module M = Spec_js.Make(struct
    let r1     = make string_of_r1
    let r2     = make string_of_r2
    let r3     = make string_of_r3
    let r4     = make string_of_r4
    let r5     = make string_of_r5
    let r6     = make string_of_r6
    let r7     = make string_of_r7
    let r8     = make string_of_r8
    let j1     = make string_of_j1
    let j2     = make string_of_j2
    let j3     = make string_of_j3
    let j4     = make string_of_j4
    let o1     = make string_of_o1
    let o2     = make string_of_o2
    let t1     = make string_of_t1
    let t2     = make string_of_t2
    let v1list = make string_of_v1list
    let v2     = make string_of_v2
    let v3list = make string_of_v3list
    let ages   = make string_of_ages
  end)

let () = M.run()
