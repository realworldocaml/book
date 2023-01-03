open! Import

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

class virtual ['res] lift :
  object
    inherit ['res] Ppxlib_traverse_builtins.lift
    inherit ['res] Ast.lift
  end
