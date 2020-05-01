let make = Atdgen_codec_runtime.Encode.encode

module M = Spec_js.Make(struct
    open Spec_js.Bs

    let r1     = make write_r1
    let r2     = make write_r2
    let r3     = make write_r3
    let r4     = make write_r4
    let r5     = make write_r5
    let r6     = make write_r6
    let r7     = make write_r7
    let r8     = make write_r8
    let j1     = make write_j1
    let j2     = make write_j2
    let j3     = make write_j3
    let j4     = make write_j4
    let o1     = make write_o1
    let o2     = make write_o2
    let t1     = make write_t1
    let t2     = make write_t2
    let v1list = make write_v1list
    let v2     = make write_v2
    let v3list = make write_v3list
    let ages   = make write_ages
  end)

let () = M.run ()
