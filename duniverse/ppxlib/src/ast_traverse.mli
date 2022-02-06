(** AST traversal classes *)

open! Import

(** To use these classes, inherit from them and override the methods
    corresponding to the types from [Parsetree] you want to process. For
    instance to collect all the string constants in a structure:

    {[
      let string_constants_of =
        object
          inherit [string list] Ast_traverse.fold as super

          method! expression e acc =
            let acc = super#expression e acc in
            match e.pexp_desc with
            | Pexp_constant (Const_string (s, _)) -> s :: acc
            | _ -> acc

          method! pattern p acc =
            let acc = super#pattern p acc in
            match p.ppat_desc with
            | Ppat_constant (Const_string (s, _)) -> s :: acc
            | _ -> acc
        end

      let string_constants_of_structure = string_constants_of#structure
    ]} *)

class map :
  object
    inherit Ppxlib_traverse_builtins.map

    inherit Ast.map
  end

class iter :
  object
    inherit Ppxlib_traverse_builtins.iter

    inherit Ast.iter
  end

class ['acc] fold :
  object
    inherit ['acc] Ppxlib_traverse_builtins.fold

    inherit ['acc] Ast.fold
  end

class ['acc] fold_map :
  object
    inherit ['acc] Ppxlib_traverse_builtins.fold_map

    inherit ['acc] Ast.fold_map
  end

class ['ctx] map_with_context :
  object
    inherit ['ctx] Ppxlib_traverse_builtins.map_with_context

    inherit ['ctx] Ast.map_with_context
  end

class map_with_path : [string] map_with_context

class map_with_expansion_context : [Expansion_context.Base.t] map_with_context

class virtual ['res] lift :
  object
    inherit ['res] Ppxlib_traverse_builtins.lift

    inherit ['res] Ast.lift
  end

class sexp_of :
  object
    inherit [Sexp.t] Ppxlib_traverse_builtins.std_lifters

    inherit [Sexp.t] Ast.lift
  end

val sexp_of : sexp_of
