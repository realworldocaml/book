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

val enter_value : (expression, string loc) Attribute.t
val enter_module : (module_expr, string loc) Attribute.t
val do_not_enter_value_binding : (value_binding, unit) Attribute.t
val do_not_enter_value_description : (value_description, unit) Attribute.t
val do_not_enter_module_binding : (module_binding, unit) Attribute.t
val do_not_enter_module_declaration : (module_declaration, unit) Attribute.t

val do_not_enter_module_type_declaration :
  (module_type_declaration, unit) Attribute.t

val do_not_enter_let_module : (expression, unit) Attribute.t

class virtual ['res] lift :
  object
    inherit ['res] Ppxlib_traverse_builtins.lift
    inherit ['res] Ast.lift
  end

class virtual ['ctx, 'res] lift_map_with_context :
  object
    inherit ['ctx, 'res] Ppxlib_traverse_builtins.lift_map_with_context
    inherit ['ctx, 'res] Ast.lift_map_with_context
  end

class map_with_expansion_context_and_errors :
  object
    inherit
      [Expansion_context.Base.t, Location.Error.t list] Ppxlib_traverse_builtins
                                                        .std_lift_mappers_with_context

    inherit
      [Expansion_context.Base.t, Location.Error.t list] Ast
                                                        .lift_map_with_context
  end

class sexp_of :
  object
    inherit [Sexp.t] Ppxlib_traverse_builtins.std_lifters
    inherit [Sexp.t] Ast.lift
  end

val sexp_of : sexp_of
