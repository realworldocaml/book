(** [Uncommon] is a [Common], now with less name clashes. *)

let kasprintf k fmt =
  Format.(kfprintf (fun _ -> k (flush_str_formatter ())) str_formatter fmt)

let invalid_arg fmt = kasprintf invalid_arg ("Nocrypto: " ^^ fmt)
let failwith fmt = kasprintf failwith ("Nocrypto: " ^^ fmt)

let (//) x y =
  if y < 1 then raise Division_by_zero else
    if x > 0 then 1 + ((x - 1) / y) else 0 [@@inline]

let imin (a : int) b = if a < b then a else b
let imax (a : int) b = if a < b then b else a

let (&.) f g = fun h -> f (g h)

let id x = x

let rec until p f = let r = f () in if p r then r else until p f

module Option = struct

  let get_or f x = function None -> f x | Some y -> y

  let (>>=) a fb = match a with Some x -> fb x | _ -> None
  let (>>|) a f = match a with Some x -> Some (f x) | _ -> None

  let v_map ~def ~f = function
    | Some x -> f x
    | None   -> def

  let get ~def = function
    | Some x -> x
    | None   -> def

  let map ~f = function
    | Some x -> Some (f x)
    | None   -> None

  let cond ~f = function
    | Some x -> ignore (f x)
    | None   -> ()
end

type 'a iter = ('a -> unit) -> unit

let iter1 a     f = f a
let iter2 a b   f = f a; f b
let iter3 a b c f = f a; f b; f c

let string_fold ~f ~z str =
  let st = ref z in
  ( String.iter (fun c -> st := f !st c) str  ; !st )

(* The Sexplib hack... *)
module Z = struct
  include Z

  let two   = ~$2
  let three = ~$3

  let pp = pp_print

  open Sexplib.Conv
  let sexp_of_t z = sexp_of_string (Z.to_string z)
  let t_of_sexp s = Z.of_string (string_of_sexp s)
end

module Cs = struct

  open Cstruct

  let empty = create 0

  let null cs = len cs = 0

  let (<+>) = append

  let ct_eq cs1 cs2 =
    let rec go ok i = function
      | n when n >= 8 ->
          go (LE.(get_uint64 cs1 i = get_uint64 cs2 i) && ok) (i + 8) (n - 8)
      | n when n >= 4 ->
          go (LE.(get_uint32 cs1 i = get_uint32 cs2 i) && ok) (i + 4) (n - 4)
      | n when n >= 2 ->
          go (LE.(get_uint16 cs1 i = get_uint16 cs2 i) && ok) (i + 2) (n - 2)
      | 1             -> (get_uint8 cs1 i = get_uint8 cs2 i) && ok
      | _             -> ok
    in
    let n1 = len cs1 and n2 = len cs2 in
    go true 0 (imin n1 n2) && n1 = n2

  let ct_find_uint8 ?(off=0) ~f cs =
    let rec go acc i = function
      | 0 -> acc
      | n ->
          let acc = match (acc, f (get_uint8 cs i)) with
            | (None, true) -> Some i
            | _            -> acc in
          go acc (succ i) (pred n) in
    go None off (len cs - off)

  let clone ?(off = 0) ?len cs =
    let len = match len with None -> cs.len - off | Some x -> x in
    let cs' = create_unsafe len in
    ( blit cs off cs' 0 len ; cs' )

  let xor_into src dst n =
    if n > imin (len src) (len dst) then
      invalid_arg "Uncommon.Cs.xor_into: buffers to small (need %d)" n
    else No_native.xor_into src.buffer src.off dst.buffer dst.off n

  let xor cs1 cs2 =
    let len = imin (len cs1) (len cs2) in
    let cs  = clone ~len cs2 in
    ( xor_into cs1 cs len ; cs )

  let create ?(init=0x00) n =
    let cs = create_unsafe n in ( memset cs init ; cs )

  let is_prefix cs0 cs = cs0.len <= cs.len && equal cs0 (sub cs 0 cs0.len)

  let set_msb bits cs =
    if bits > 0 then
      let n = len cs in
      let rec go width = function
        | i when i = n     -> ()
        | i when width < 8 ->
            set_uint8 cs i (get_uint8 cs i lor (0xff lsl (8 - width)))
        | i ->
            set_uint8 cs i 0xff ; go (width - 8) (succ i) in
      go bits 0

  let split2 cs l =
    (sub cs 0 l, sub cs l (len cs - l))

  let split3 cs l1 l2 =
    let l12 = l1 + l2 in
    (sub cs 0 l1, sub cs l1 l2, sub cs l12 (len cs - l12))

  let rpad cs size x =
    let l = len cs and cs' = Cstruct.create_unsafe size in
    if size < l then invalid_arg "Uncommon.Cs.rpad: size < len";
    blit cs 0 cs' 0 l ;
    memset (sub cs' l (size - l)) x ;
    cs'

  let lpad cs size x =
    let l = len cs and cs' = Cstruct.create_unsafe size in
    if size < l then invalid_arg "Uncommon.Cs.lpad: size < len";
    blit cs 0 cs' (size - l) l ;
    memset (sub cs' 0 (size - l)) x ;
    cs'

  let of_bytes, of_int32s, of_int64s =
    let aux k set xs =
      let cs = Cstruct.create_unsafe @@ List.length xs * k in
      List.iteri (fun i x -> set cs (i * k) x) xs;
      cs
    in
    (aux 1 set_uint8, aux 4 BE.set_uint32, aux 8 BE.set_uint64)

  let b x =
    let cs = Cstruct.create_unsafe 1 in ( set_uint8 cs 0 x ; cs )

  let rec shift_left_inplace cs = function
    | 0 -> ()
    | bits when bits mod 8 = 0 ->
        let off = bits / 8 in
        blit cs off cs 0 (cs.len - off) ;
        memset (shift cs (cs.len - off)) 0x00
    | bits when bits < 8 ->
        let foo = 8 - bits in
        for i = 0 to cs.len - 2 do
          let b1 = get_uint8 cs i
          and b2 = get_uint8 cs (i + 1) in
          set_uint8 cs i ((b1 lsl bits) lor (b2 lsr foo))
        done ;
        set_uint8 cs (cs.len - 1) @@ get_uint8 cs (cs.len - 1) lsl bits
    | bits ->
        shift_left_inplace cs (8 * (bits / 8)) ;
        shift_left_inplace cs (bits mod 8)

  let rec shift_right_inplace cs = function
    | 0 -> ()
    | bits when bits mod 8 = 0 ->
        let off = bits / 8 in
        blit cs 0 cs off (cs.len - off) ;
        memset (sub cs 0 off) 0x00
    | bits when bits < 8 ->
        let foo = 8 - bits in
        for i = cs.len - 1 downto 1 do
          let b1 = get_uint8 cs i
          and b2 = get_uint8 cs (i - 1) in
          set_uint8 cs i ((b2 lsl foo) lor (b1 lsr bits))
        done ;
        set_uint8 cs 0 @@ get_uint8 cs 0 lsr bits
    | bits ->
        shift_right_inplace cs (8 * (bits / 8));
        shift_right_inplace cs (bits mod 8)

  let of_hex str =
    let hexdigit = function
      | 'a' .. 'f' as x -> int_of_char x - 87
      | 'A' .. 'F' as x -> int_of_char x - 55
      | '0' .. '9' as x -> int_of_char x - 48
      | x               -> invalid_arg "of_hex: `%c'" x
    in
    let whitespace = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false in
    match
      string_fold
      ~f:(fun (cs, i, acc) -> function
          | char when whitespace char -> (cs, i, acc)
          | char ->
              match (acc, hexdigit char) with
              | (None  , x) -> (cs, i, Some (x lsl 4))
              | (Some y, x) -> set_uint8 cs i (x lor y) ; (cs, succ i, None))
      ~z:(create_unsafe (String.length str), 0, None)
      str
    with
    | (_ , _, Some _) -> invalid_arg "of_hex: dangling nibble"
    | (cs, i, _     ) -> sub cs 0 i

  let (lsl) cs bits =
    let cs' = clone cs in
    shift_left_inplace cs' bits ; cs'

  and (lsr) cs bits =
    let cs' = clone cs in
    shift_right_inplace cs' bits ; cs'

  and (lxor) cs1 cs2 = xor cs1 cs2

end

module Array = struct
  include Array
  let mem x arr =
    let rec scan = function
      | -1 -> false
      | n  -> arr.(n) = x || scan (pred n) in
    scan (Array.length arr - 1)
end

module List = struct
  include List
  let find_opt p xs = try Some (find p xs) with Not_found -> None
end

let bracket ~init ~fini f =
  let a = init () in
  match f a with
  | exception exn -> fini a; raise exn
  | res           -> fini a; res

let pp_xd_gen getu8 len ?(address=true) ?(ascii=false) ?(w=16) () ppf =
  let open Format in
  let rec blocks f off xs =
    let n = len xs - off in
    f off (min n w) xs; if n > w then blocks f (off + w) xs in
  let line ppf off n xs =
    let iter f = for i = 0 to n - 1 do f i (off + i |> getu8 xs) done
    and spaces n = for _ = 1 to n do pp_print_char ppf ' ' done in
    if off > 0 then pp_print_cut ppf ();
    if address then fprintf ppf "%04x:  " off;
    iter (fun i u8 ->
      pp_print_string ppf
        (if i > 0 then if i mod 8 = 0 then "  " else " " else "");
      fprintf ppf "%02x" u8);
    if ascii then
      let llen x = x * 2 + (x - 1 |> max 0) + (x - 1) / 8 in
      spaces (2 + llen w - llen n);
      iter (fun _ u8 -> fprintf ppf "%c"
        (if u8 >= 0x20 && u8 < 0x7f then Char.chr u8 else '.'));
      spaces (w - n)
  in fprintf ppf "@[<v>%a@]" (fun ppf -> blocks (line ppf) 0)

let xd  = Cstruct.(pp_xd_gen get_uint8 len)
let xdb = Bytes.(pp_xd_gen (fun b i -> Char.code (get b i)) length)

(* Random stuff needed for other modules because deps. *)
module Boot = struct

  (* Should be thrown be generators and live in Rng, but Rng needs to
   * instantiate Fortuna for the latter can't depend on the former. *)
  exception Unseeded_generator
end
