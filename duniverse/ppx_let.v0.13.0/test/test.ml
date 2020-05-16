module Monad_example = struct
  module X : sig
    type 'a t

    module Let_syntax : sig
      val return : 'a -> 'a t

      module Let_syntax : sig
        val return : 'a -> 'a t
        val bind : 'a t -> f:('a -> 'b t) -> 'b t
        val map : 'a t -> f:('a -> 'b) -> 'b t
        val both : 'a t -> 'b t -> ('a * 'b) t

        module Open_on_rhs : sig
          val return : 'a -> 'a t
        end
      end
    end
  end = struct
    type 'a t = 'a

    let return x = x
    let bind x ~f = f x
    let map x ~f = f x
    let both x y = x, y

    module Let_syntax = struct
      let return = return

      module Let_syntax = struct
        let return = return
        let bind = bind
        let map = map
        let both = both

        module Open_on_rhs = struct
          let return = return
        end
      end
    end
  end

  open X.Let_syntax

  let _mf a : _ X.t =
    let%bind_open x = a in
    return (x + 1)
  ;;

  let _mf' a b c : _ X.t =
    let%bind_open x = a
    and y = b
    and u, v = c in
    return (x + y + (u * v))
  ;;

  let _mg a : _ X.t =
    let%map x : int X.t = a in
    x + 1
  ;;

  let _mg' a b c : _ X.t =
    let%map x = a
    and y = b
    and u, v = c in
    x + y + (u * v)
  ;;

  let _mh a : _ X.t =
    match%bind_open a with
    | 0 -> return true
    | _ -> return false
  ;;

  let _mi a : _ X.t =
    match%map a with
    | 0 -> true
    | _ -> false
  ;;

  let _mif a : _ X.t = if%bind_open a then return true else return false
  let _mif' a : _ X.t = if%map a then true else false
end

module Applicative_example = struct
  module X : sig
    type 'a t

    module Let_syntax : sig
      val return : 'a -> 'a t

      module Let_syntax : sig
        val return : 'a -> 'a t
        val map : 'a t -> f:('a -> 'b) -> 'b t
        val both : 'a t -> 'b t -> ('a * 'b) t

        module Open_on_rhs : sig
          val flag : int t
          val anon : int t
        end
      end
    end
  end = struct
    type 'a t = 'a

    let return x = x
    let map x ~f = f x
    let both x y = x, y

    module Let_syntax = struct
      let return = return

      module Let_syntax = struct
        let return = return
        let map = map
        let both = both

        module Open_on_rhs = struct
          let flag = 66
          let anon = 77
        end
      end
    end
  end

  open X.Let_syntax

  (* {[
       let _af a : _ X.t =
         let%bind x = a in (* "Error: Unbound value Let_syntax.bind" *)
         return (x + 1)
     ]} *)

  (* {[
       let _af' a b c : _ X.t =
         let%bind x = a and y = b and (u, v) = c in (* "Error: Unbound value Let_syntax.bind" *)
         return (x + y + (u * v))
     ]} *)

  let _ag a : _ X.t =
    let%map x = a in
    x + 1
  ;;

  let _ag' a b c : _ X.t =
    let%map x = a
    and y = b
    and u, v = c in
    x + y + (u * v)
  ;;

  (* {[
       let _ah a : _ X.t =
         match%bind a with (* "Error: Unbound value Let_syntax.bind" *)
         | 0 -> return true
         | _ -> return false
     ]} *)

  let _ai a : _ X.t =
    match%map a with
    | 0 -> true
    | _ -> false
  ;;
end

module Example_without_open = struct
  let _ag a : _ Applicative_example.X.t =
    let%map.Applicative_example.X x = a in
    x + 1
  ;;
end
