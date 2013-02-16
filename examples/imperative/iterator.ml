type 'a t =
  < has_value : bool;
    value : 'a;
    next : unit;
    set : 'a -> unit;
  >

