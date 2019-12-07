module Stable = struct
  open Core_kernel.Core_kernel_stable
  open Sexplib.Type

  module Date_selector = struct
    module V1 = struct
      type t =
        | GT of Date.V1.t
        | LT of Date.V1.t
        | Between of Date.V1.t * Date.V1.t
        | On of Date.V1.t
        [@@deriving bin_io, sexp]

      let t_of_sexp sexp =
        let module Date = Core_kernel.Date in
        match sexp with
        | Atom _ as d -> On (Date.t_of_sexp d)
        | List [Atom ">"; Atom _ as d]      -> GT (Date.t_of_sexp d)
        | List [Atom ">="; Atom _ as d]     -> GT (Date.add_days (Date.t_of_sexp d) (-1))
        | List [Atom "<"; Atom _ as d]      -> LT (Date.t_of_sexp d)
        | List [Atom "<="; Atom _ as d]     -> LT (Date.add_days (Date.t_of_sexp d) (1))
        | List [Atom _ as d1; Atom "><"; Atom _ as d2]
        | List [Atom "><"; Atom _ as d1; Atom _ as d2]
        | List [Atom _ as d1; Atom _ as d2] ->
          (* The basic cases (GT, LT etc.) are being matched here, since
             they are lists of two atoms. Here the check whether the first
             atom is a date is done with try-with. *)
          begin
            try
              Between ((Date.t_of_sexp d1), (Date.t_of_sexp d2))
            with _ -> t_of_sexp sexp
          end
        | _ -> t_of_sexp sexp
    end
    module Current = V1
  end

  module String_selector = struct
    module Regexp = struct
      module V1 = struct
        module T = struct
          (* This type is stable in spite of using the Re2's non-stable type because
             bin_io and sexp conversion functions are explicitly defined below. *)
          type t = string * Re.re

          let to_string (s, _) = s
          let of_regexp s = s, Re.Perl.compile_pat s
          let of_string s = of_regexp s
        end
        include T
        include Binable.Of_stringable.V1(T)

        let t_of_sexp sexp =
          let open Core_kernel in
          let fail () =
            of_sexp_error "expected string bounded with / on both sides" sexp
          in
          match sexp with
          | List _ -> of_sexp_error "expected Atom" sexp
          | Atom s ->
            if String.length s < 2 then fail ()
            else if s.[0] = '/' && s.[String.length s - 1] = '/' then
              let s = String.sub s ~pos:1 ~len:(String.length s - 2) in
              of_regexp s
            else fail ()

        let sexp_of_t (s, _) = Sexp.V1.Atom ("/" ^ s ^ "/")
      end
      module Current = V1
    end

    module V1 = struct
      type t =
        | Equal of string list
        | Matches of Regexp.V1.t list
        | Mixed of [ `Regexp of Regexp.V1.t | `Literal of string ] list
        [@@deriving bin_io, sexp]

      let t_of_sexp sexp =
        let parse_atom a =
          match a with
          | List _ -> assert false
          | Atom s ->
            if Core_kernel.(String.length s >= 1 && s.[0] = '/')
            then `Regexp (Regexp.V1.t_of_sexp a)
            else `Literal s
        in
        try
          match sexp with
          | Atom _ as a ->
            begin match parse_atom a with
            | `Regexp r -> Matches [r]
            | `Literal s -> Equal [s]
            end
          | List l ->
            Mixed
              (Core_kernel.List.map l ~f:(fun sexp ->
                match sexp with
                | List _ -> Core_kernel.of_sexp_error "expected Atom" sexp
                | Atom _ as a -> parse_atom a))
        with
        | e -> try t_of_sexp sexp with _ -> raise e
    end
    module Current = V1
  end

  module String_list_selector = struct
    module V1 = struct
      type t = string list [@@deriving bin_io, sexp]

      let t_of_sexp sexp =
        match sexp with
        | Sexp.V1.Atom s -> [s]
        | _ -> t_of_sexp sexp
    end
    module Current = V1
  end

end


open Core_kernel



module type Selector = sig
  type selector
  type value

  val eval : selector -> value -> bool
end

module Date_selector = struct
  include Stable.Date_selector.Current

  type selector = t
  type value = Date.t

  let eval t d =
    match t with
    | GT gtd           -> Date.(>) d gtd
    | LT ltd           -> Date.(<) d ltd
    | Between (d1, d2) -> Date.(>=) d d1 && Date.(<=) d d2
    | On ond           -> Date.(=) d ond
end

module String_selector = struct
  module Regexp : sig
    type t = Stable.String_selector.Regexp.Current.t
    [@@deriving bin_io, sexp]

    val of_regexp : string -> t
    val to_string : t -> string
    val matches   : t -> string -> bool
    val to_regexp : t -> Re.re
  end = struct
    include Stable.String_selector.Regexp.Current

    let to_regexp (_, p) = p
    let matches (_, rex) s =
      Re.execp rex s
  end

  include Stable.String_selector.Current

  type selector = t
  type value = String.t

  let eval t s =
    match t with
    | Equal el -> Option.is_some (List.find el ~f:(fun e -> e = s))
    | Matches ml -> Option.is_some (List.find ml ~f:(fun rex -> Regexp.matches rex s))
    | Mixed ml ->
      Option.is_some (List.find ml ~f:(function
        | `Regexp rex -> Regexp.matches rex s
        | `Literal l -> l = s))
end

module String_list_selector = struct
  include Stable.String_list_selector.Current

  type selector = t
  type value = string

  let eval t s =
    match List.find t ~f:(fun m -> m = s) with
    | None   -> false
    | Some _ -> true
end
