open Core.Std
open Async.Std
open Async_graphics
open Drawable

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

(* part 1 *)
class square w x y = object
  inherit shape x y

  val mutable width = w
  method width = width

  method draw = fill_rect x y width width

  method private contains x' y' = 
    x <= x' && x' <= x + width &&
    y <= y' && y' <= y + width 
end 

class circle r x y = object
  inherit shape x y

  val mutable radius = r
  method radius = radius

  method draw = fill_circle x y radius

  method private contains x' y' =
    let dx = abs (x' - x) in
    let dy = abs (y' - y) in
    let dist = sqrt (Float.of_int ((dx * dx) + (dy * dy))) in
    dist <= (Float.of_int radius)
end

(* part 2 *)
class growing_circle r x y = object (self)
  inherit circle r x y

  initializer
    self#on_click (fun x y -> radius <- radius * 2)
end

(* part 3 *)
class virtual draggable = object (self)
  method virtual on_mousedown: 
    ?start:unit Deferred.t -> 
    ?stop:unit Deferred.t -> 
    (int -> int -> unit) -> unit
  val virtual mutable x: int  
  val virtual mutable y: int  

  val mutable dragging = false
  method dragging = dragging

  initializer 
    self#on_mousedown 
      (fun mouse_x mouse_y ->
         let offset_x = x - mouse_x in
         let offset_y = y - mouse_y in
         let mouse_up = Ivar.create () in
         let stop = Ivar.read mouse_up in
         dragging <- true;
         on_mouseup ~stop
           (fun _ ->
              Ivar.fill mouse_up ();
              dragging <- false);
         on_mousemove ~stop
           (fun {mouse_x;mouse_y} ->
              x <- mouse_x + offset_x;
              y <- mouse_y + offset_y))
end

(* part 4 *)
class small_square = object
  inherit square 10 20 20
  inherit draggable 
end

let () = Drawable.open_display (); Drawable.shapes := [ new growing_circle 10 5 5 ]
