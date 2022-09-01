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

  let _mj : int X.t -> bool X.t =
    function%bind
    | 0 -> return true
    | _ -> return false
  ;;

  let _mk : int X.t -> bool X.t =
    function%map
    | 0 -> true
    | _ -> false
  ;;
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

module Example_with_mapn = struct
  module Let_syntax = struct
    let return = Monad_example.X.Let_syntax.return

    module Let_syntax = struct
      include Monad_example.X.Let_syntax.Let_syntax

      let map2 a b ~f = map (both a b) ~f:(fun (a, b) -> f a b)
      let map3 a b c ~f = map2 (both a b) c ~f:(fun (a, b) c -> f a b c)
      let map4 a b c d ~f = map2 (both a b) (both c d) ~f:(fun (a, b) (c, d) -> f a b c d)
    end
  end

  let _x =
    let open Let_syntax in
    let%mapn a = return 1
    and b = return "hi"
    and c = return 2.34
    and d = return true in
    Printf.sprintf "%d %s %f %b" a b c d
  ;;
end

module Arrow_example = struct
  module X : sig
    type 'a c
    type 'a v

    val return_v : 'a -> 'a v

    module Let_syntax : sig
      val return : 'a v -> 'a c

      module Let_syntax : sig
        val return : 'a v -> 'a c
        val sub : ?here:Lexing.position -> 'a c -> f:('a v -> 'b c) -> 'b c
        val map : 'a v -> f:('a -> 'b) -> 'b v
        val both : 'a v -> 'b v -> ('a * 'b) v
        val switch : match_:int v -> branches:int -> with_:(int -> 'b c) -> 'b c
        val arr : ?here:Lexing.position -> 'a v -> f:('a -> 'b) -> 'b c
      end
    end
  end = struct
    type 'a v = 'a
    type 'a c = 'a

    let return_v x = x
    let return x = x
    let sub ?here:_ x ~f = f x
    let map x ~f = f x
    let both a b = a, b
    let switch ~match_ ~branches:_ ~with_ = with_ match_
    let arr ?here:_ x ~f = return (f x)

    module Let_syntax = struct
      let return = return

      module Let_syntax = struct
        let return = return
        let sub = sub
        let map = map
        let both = both
        let switch = switch
        let arr = arr
      end
    end
  end

  open X.Let_syntax

  let _arrow_example_1 a : _ X.c =
    match%sub a with
    | 0 -> return (X.return_v true)
    | _ -> return (X.return_v false)
  ;;

  let _arrow_example_2 a : _ X.c =
    match%sub a with
    | 0 -> return (X.return_v true)
    | b ->
      return
        (let%map b = b in
         b = 1)
  ;;

  let _arrow_example_3 a : _ X.c =
    match%sub a with
    | 0 -> return (X.return_v true)
    | _ as b ->
      return
        (let%map b = b in
         b = 1)
  ;;

  type abc = A of int

  let _arrow_example_4 a : _ X.c =
    match%sub a with
    | A b ->
      return
        (let%map b = b in
         b = 1)
  ;;

  let _arrow_example_5 (a : _ X.v) : _ X.c =
    let%arr a = a in
    a
  ;;

  let _arrow_example_6 : 'a X.v -> 'a X.c =
    function%arr
    | a -> a
  ;;

  let _arrow_example_7 : 'a X.v -> 'a X.c =
    function%sub
    | a -> return a
  ;;

  let _arrow_example_8 : default:'a -> 'a option X.v -> 'a X.c =
    fun ~default ->
      function%arr
      | Some a -> a
      | None -> default
  ;;

  let _arrow_example_9 : default:'a X.v -> 'a option X.v -> 'a X.c =
    fun ~default ->
      function%sub
      | Some a -> return a
      | None -> return default
  ;;
end
