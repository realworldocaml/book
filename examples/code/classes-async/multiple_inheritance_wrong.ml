[@@@part "1"] ;;

class square_outline w x y = object
  method draw = draw_rect x y w w
  inherit square w x y
end
