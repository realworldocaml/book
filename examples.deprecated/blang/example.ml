open Core.Std
open Bool_lang.Export

let () =
  assert
    (Bool_lang.eval
       (And [ Not (Const false)
                  ; Or [Const true; Const false]
                  ])
       (fun _ -> true)
    )
