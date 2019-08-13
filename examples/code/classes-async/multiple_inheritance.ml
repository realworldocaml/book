[@@@part "1"] ;;

class square_outline w x y = object
  inherit square w x y
  method draw = draw_rect x y width width
end
