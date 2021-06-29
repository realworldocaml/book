open! Import
module Array = Array0

let stage = Staged.stage

module T = struct
  type t = bytes [@@deriving_inline sexp, sexp_grammar]

  let t_of_sexp = (bytes_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t)
  let sexp_of_t = (sexp_of_bytes : t -> Ppx_sexp_conv_lib.Sexp.t)

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "bytes" ]
      ; ggid = "\146e\023\249\235eE\139c\132W\195\137\129\235\025"
      ; types = [ "t", Implicit_var 0 ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ bytes_sexp_grammar ]
      ; generic_group = _the_generic_group
      ; origin = "bytes.ml.T"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  [@@@end]

  include Bytes0

  let module_name = "Base.Bytes"
  let pp fmt t = Caml.Format.fprintf fmt "%S" (to_string t)
end

include T

module To_bytes = Blit.Make (struct
    include T

    let create ~len = create len
  end)

include To_bytes
include Comparator.Make (T)
include Comparable.Validate (T)
include Pretty_printer.Register_pp (T)

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! Bytes_replace_polymorphic_compare
module To_string = Blit.Make_to_string (T) (To_bytes)

module From_string =
  Blit.Make_distinct
    (struct
      type t = string

      let length = String.length
    end)
    (struct
      type nonrec t = t

      let create ~len = create len
      let length = length
      let unsafe_blit = unsafe_blit_string
    end)

let invariant (_ : t) = ()

let init n ~f =
  if Int_replace_polymorphic_compare.( < ) n 0
  then Printf.invalid_argf "Bytes.init %d" n ();
  let t = create n in
  for i = 0 to n - 1 do
    unsafe_set t i (f i)
  done;
  t
;;

let of_char_list l =
  let t = create (List.length l) in
  List.iteri l ~f:(fun i c -> set t i c);
  t
;;

let to_list t =
  let rec loop t i acc =
    if Int_replace_polymorphic_compare.( < ) i 0
    then acc
    else loop t (i - 1) (unsafe_get t i :: acc)
  in
  loop t (length t - 1) []
;;

let to_array t = Array.init (length t) ~f:(fun i -> unsafe_get t i)
let map t ~f = map t ~f
let mapi t ~f = mapi t ~f

let fold =
  let rec loop t ~f ~len ~pos acc =
    if Int_replace_polymorphic_compare.equal pos len
    then acc
    else loop t ~f ~len ~pos:(pos + 1) (f acc (unsafe_get t pos))
  in
  fun t ~init ~f -> loop t ~f ~len:(length t) ~pos:0 init
;;

let foldi =
  let rec loop t ~f ~len ~pos acc =
    if Int_replace_polymorphic_compare.equal pos len
    then acc
    else loop t ~f ~len ~pos:(pos + 1) (f pos acc (unsafe_get t pos))
  in
  fun t ~init ~f -> loop t ~f ~len:(length t) ~pos:0 init
;;

let tr ~target ~replacement s =
  for i = 0 to length s - 1 do
    if Char.equal (unsafe_get s i) target then unsafe_set s i replacement
  done
;;

let tr_multi ~target ~replacement =
  if Int_replace_polymorphic_compare.( = ) (String.length target) 0
  then stage ignore
  else if Int_replace_polymorphic_compare.( = ) (String.length replacement) 0
  then invalid_arg "tr_multi: replacement is the empty string"
  else (
    match Bytes_tr.tr_create_map ~target ~replacement with
    | None -> stage ignore
    | Some tr_map ->
      stage (fun s ->
        for i = 0 to length s - 1 do
          unsafe_set s i (String.unsafe_get tr_map (Char.to_int (unsafe_get s i)))
        done))
;;

let between t ~low ~high = low <= t && t <= high
let clamp_unchecked t ~min ~max = if t < min then min else if t <= max then t else max

let clamp_exn t ~min ~max =
  assert (min <= max);
  clamp_unchecked t ~min ~max
;;

let clamp t ~min ~max =
  if min > max
  then
    Or_error.error_s
      (Sexp.message
         "clamp requires [min <= max]"
         [ "min", T.sexp_of_t min; "max", T.sexp_of_t max ])
  else Ok (clamp_unchecked t ~min ~max)
;;

let contains ?pos ?len t char =
  let pos, len =
    Ordered_collection_common.get_pos_len_exn () ?pos ?len ~total_length:(length t)
  in
  let last = pos + len in
  let rec loop i =
    Int_replace_polymorphic_compare.( < ) i last
    && (Char.equal (get t i) char || loop (i + 1))
  in
  loop pos
;;

(* Include type-specific [Replace_polymorphic_compare] at the end, after
   including functor application that could shadow its definitions. This is
   here so that efficient versions of the comparison functions are exported by
   this module. *)
include Bytes_replace_polymorphic_compare
