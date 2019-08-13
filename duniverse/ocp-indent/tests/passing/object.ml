let x =
  object
    inherit foo
    method bar = _
  end

class foo =
  object
    method x = 2
    inherit bar
  end

class foo =
  object(this)
    inherit bar
  end

class virtual map = object
  method visit_expr_node :
    'env 'info_0 'info_1 .
    ('env -> 'info_0 -> 'info_1) ->
    'env -> 'info_0 expr_node -> 'info_1 expr_node =
    assert false
end
