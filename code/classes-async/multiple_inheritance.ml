open Core.Std
open Async.Std
open Async_graphics

class virtual shape x y = object (self)
  method virtual private contains: int -> int -> bool

  val mutable x: int = x
  method x = x

  val mutable y: int = y
  method y = y

  method on_click ?start ?stop f =
    on_click ?start ?stop
      (fun {mouse_x;mouse_y} ->  
         if self#contains mouse_x mouse_y then
           f mouse_x mouse_y)

  method on_mousedown ?start ?stop f =
    on_mousedown ?start ?stop
      (fun {mouse_x;mouse_y} ->
         if self#contains mouse_x mouse_y then
           f mouse_x mouse_y)
end

class square w x y = object
  inherit shape x y

  val mutable width = w
  method width = width

  method draw = fill_rect x y width width

  method private contains x' y' = 
    x <= x' && x' <= x + width &&
    y <= y' && y' <= y + width 
end

(* part 1 *)
class square_outline w x y = object
  inherit square w x y
  method draw = draw_rect x y width width
end
