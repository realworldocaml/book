open Import

val with_output : label option -> binary:bool -> f:(out_channel -> 'a) -> 'a

module Kind : sig
  type t = Intf | Impl

  val of_filename : string -> t option
  val describe : t -> string
  val equal : t -> t -> bool
end

module Intf_or_impl : sig
  type t = Intf of signature | Impl of structure

  val map : t -> Ast_traverse.map -> t
  val map_with_context : t -> 'a Ast_traverse.map_with_context -> 'a -> t
  val kind : t -> Kind.t
end

module Ast_io : sig
  type input_version

  type t = {
    input_name : string;
    input_version : input_version;
    ast : Intf_or_impl.t;
  }

  type read_error =
    | Not_a_binary_ast
    | Unknown_version of string * input_version
    (* The input contains a binary AST for an unknown version of
        OCaml. The first argument is the unknown magic number. *)
    | Source_parse_error of Location.Error.t * input_version
    | System_error of Location.Error.t * input_version

  type input_source = Stdin | File of string
  type input_kind = Possibly_source of Kind.t * string | Necessarily_binary

  val read : input_source -> input_kind:input_kind -> (t, read_error) result
  val write : out_channel -> t -> add_ppx_context:bool -> unit

  module Read_bin : sig
    type ast = Intf of signature | Impl of structure
    type t

    val read_binary : string -> (t, string) result
    val get_ast : t -> ast
    val get_input_name : t -> string
  end
end

module System : sig
  val run_preprocessor :
    pp:string ->
    input:string ->
    output:string ->
    (unit, string * Ast_io.input_version) result
end
