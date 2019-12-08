open! Import

let r = ref [ "Base.Sexp.pp_hum" ]

let all () = !r

let register p = r := p :: !r

module type S = sig
  type t
  val pp : Formatter.t -> t -> unit
end

module Register_pp (M : sig
    include S
    val module_name : string
  end) = struct
  include M
  let () = register (M.module_name ^ ".pp")
end

module Register (M : sig
    type t
    val module_name : string
    val to_string : t -> string
  end) = Register_pp (struct
    include M
    let pp formatter t = Caml.Format.pp_print_string formatter (M.to_string t)
  end)
