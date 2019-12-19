
open No_uncommon
open Cstruct

let sym     = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
let padding = int_of_char '='

let (emap, dmap) =
  let make_ht f =
    let ht = Hashtbl.create 64 in
    for i = 0 to String.length sym - 1 do
      f ht i (int_of_char sym.[i])
    done ;
    Hashtbl.find ht in
  (make_ht Hashtbl.add),
  (make_ht (fun ht i c -> Hashtbl.add ht c i))

let encode cs =

  let n   = len cs in
  let n'  = n // 3 * 4 in
  let cs' = create n' in

  let emit b1 b2 b3 i =
    BE.set_uint16 cs' i
      ((emap (b1 lsr 2 land 0x3f) lsl 8) lor
      (emap ((b1 lsl 4) lor (b2 lsr 4) land 0x3f))) ;
    BE.set_uint16 cs' (i + 2)
      ((emap ((b2 lsl 2) lor (b3 lsr 6) land 0x3f) lsl 8) lor
      (emap (b3 land 0x3f))) in

  let rec enc j = function
    | i when i = n     -> ()
    | i when i = n - 1 ->
        emit (get_uint8 cs i) 0 0 j
    | i when i = n - 2 ->
        emit (get_uint8 cs i) (get_uint8 cs (i + 1)) 0 j
    | i ->
        emit (get_uint8 cs i) (get_uint8 cs (i + 1)) (get_uint8 cs (i + 2)) j ;
        enc (j + 4) (i + 3)

  and fix = function
    | 0 -> ()
    | i -> set_uint8 cs' (n' - i) padding ; fix (i - 1) in

  enc 0 0 ;
  fix ((3 - n mod 3) mod 3) ;
  cs'

let decode cs =

  let n   = len cs in
  let n'  = (n / 4) * 3 in
  let cs' = create n' in

  let emit a b c d i =
    let x = (a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
    BE.set_uint16 cs' i (x lsr 8) ;
    set_uint8 cs' (i + 2) (x land 0xff) in

  let rec dec j = function
    | i when i = n -> 0
    | i ->
        let a = dmap (get_uint8 cs i)
        and b = dmap (get_uint8 cs (i + 1))
        and (d, pad) =
          let x = get_uint8 cs (i + 3) in try (dmap x, 0) with
            | Not_found when x = padding -> (0, 1) in
        let (c, pad) =
          let x = get_uint8 cs (i + 2) in try (dmap x, pad) with
            | Not_found when x = padding && pad = 1 -> (0, 2) in

        emit a b c d j ;
        match pad with
        | 0 -> dec (j + 3) (i + 4)
        | _ when i + 4 <> n -> raise Not_found
        | _ -> pad
  in
  try let pad = dec 0 0 in Some (sub cs' 0 (n' - pad))
  with Invalid_argument _ | Not_found -> None


let is_base64_char c =
  try ( ignore @@ dmap (int_of_char c) ; true )
  with Not_found -> false

