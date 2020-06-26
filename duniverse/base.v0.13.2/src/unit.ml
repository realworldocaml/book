open! Import

module T = struct
  type t = unit [@@deriving_inline enumerate, hash, sexp]
  let all = ([()] : t list)
  let (hash_fold_t :
         Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    hash_fold_unit
  and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = hash_unit in fun x -> func x
  let t_of_sexp = (unit_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t)
  let sexp_of_t = (sexp_of_unit : t -> Ppx_sexp_conv_lib.Sexp.t)
  [@@@end]

  let compare _ _ = 0

  let of_string = function
    | "()" -> ()
    | _ -> failwith "Base.Unit.of_string: () expected"
  ;;

  let to_string () = "()"
  let module_name = "Base.Unit"
end

include T
include Identifiable.Make (T)

let invariant () = ()
