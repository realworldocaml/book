  
## Exercise
  Records can also be used to implement a simple form of object-oriented programming.  Suppose we are
  implementing a collection of geometric objects (blobs), where each blob has a position, a function (called a \emph{method}) to
  compute the area covered by the blob, and methods to set the position and move the blob.  The
  following record defines the methods for a generic object.
  
```ocaml
  type blob =
     { get    : unit -> float * float;
       area   : unit -> float;
       set    : float * float -> unit;
       move   : float * float -> unit
     }
```
  An actual object like a rectangle might be defined as follows.
  
```ocaml
  let new_rectangle x y w h =
     let pos = ref (x, y) in
     let rec r =
        { get  = (fun () -> !pos);
          area = (fun () -> w *. h);
          set  = (fun loc -> pos := loc);
          move = (fun (dx, dy) ->
                     let (x, y) = r.get () in
                     r.set (x +. dx, y +. dy))
        }
     in
     r
```
  The rectangle record is defined recursively so that the method `move` can be defined in
  terms of `get` and `set`.
  
  Suppose we have created a new rectangle `rect1`, manipulated it, and now we want to fix it
  in position.  We might try to do this by redefining the method `set`.
  
```ocaml
  let rect1 = new_rectangle 0.0 0.0 1.0 1.0 in
  rect1.move 1.2 3.4; $\cdots$
  let rect2 = { rect1 with set = (fun _ -> ()) }
```
1. What happens to `rect2` when `rect2.move` is called?  How can you prevent it from moving?
1. What happens to `rect2` when `rect1.set` is called?
  
