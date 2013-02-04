type 'a t =
  < has_value : bool;
    value : 'a;
    next : unit;
    remove : unit;
    insert_after : 'a -> unit >

