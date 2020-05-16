open! Import
module Array = Array0
module String = String0
include Char0

module T = struct
  type t = char [@@deriving_inline compare, hash, sexp]
  let compare = (compare_char : t -> t -> int)
  let (hash_fold_t :
         Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_char
  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = hash_char in fun x -> func x
  let t_of_sexp = (char_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t)
  let sexp_of_t = (sexp_of_char : t -> Ppx_sexp_conv_lib.Sexp.t)
  [@@@end]

  let to_string t = String.make 1 t

  let of_string s =
    match String.length s with
    | 1 -> s.[0]
    | _ -> failwithf "Char.of_string: %S" s ()
  ;;
end

include T

include Identifiable.Make (struct
    include T

    let module_name = "Base.Char"
  end)

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! Char_replace_polymorphic_compare

let all = Array.init 256 ~f:unsafe_of_int |> Array.to_list

let is_lowercase = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_print = function
  | ' ' .. '~' -> true
  | _ -> false
;;

let is_whitespace = function
  | '\t' | '\n' | '\011' (* vertical tab *) | '\012' (* form feed *) | '\r' | ' ' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

(* Writing these out, instead of calling [is_alpha] and [is_digit], reduces
   runtime by approx. 30% *)
let is_alphanum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false
;;

let get_digit_unsafe t = to_int t - to_int '0'

let get_digit_exn t =
  if is_digit t
  then get_digit_unsafe t
  else failwithf "Char.get_digit_exn %C: not a digit" t ()
;;

let get_digit t = if is_digit t then Some (get_digit_unsafe t) else None

module O = struct
  let ( >= ) = ( >= )
  let ( <= ) = ( <= )
  let ( = ) = ( = )
  let ( > ) = ( > )
  let ( < ) = ( < )
  let ( <> ) = ( <> )
end

(* Include type-specific [Replace_polymorphic_compare] at the end, after
   including functor application that could shadow its definitions. This is
   here so that efficient versions of the comparison functions are exported by
   this module. *)
include Char_replace_polymorphic_compare
