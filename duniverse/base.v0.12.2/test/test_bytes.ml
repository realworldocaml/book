open! Import
open! Bytes

let%test_module "Blit" =
  (module Test_blit.Test
       (struct
         include Char
         let of_bool b = if b then 'a' else 'b'
       end)
       (struct
         include Bytes
         let create ~len = create len
       end)
       (Bytes))
