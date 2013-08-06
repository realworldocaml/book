class ['a] stack init = object
  val mutable v : 'a list = init

  method pop =
    match v with
      hd :: tl ->
      v <- tl;
      Some hd
    | [] -> None

  method push hd =
    v <- hd :: v
end

(* part 1 *)
class sstack init = object
  inherit [string] stack init

  method print =
    List.iter ~f:print_string v

  end
      string list ->
    object
      val mutable v : string Core.Std.List.t
      method pop : string option
      method print : unit
      method push : string -> unit
    end

  (* part 2 *)
  class double_stack init = object
    inherit [int] stack init as super

    method push hd =
      super#push (hd * 2)
  end;;
  class double_stack :
    int list ->
    object
      val mutable v : int list
      method pop : int option
      method push : int -> unit
    end

