module T = struct
  type 'a map = 'a -> 'a
  type 'a iter = 'a -> unit
  type ('a, 'acc) fold = 'a -> 'acc -> 'acc
  type ('a, 'acc) fold_map = 'a -> 'acc -> 'a * 'acc
  type ('ctx, 'a) map_with_context = 'ctx -> 'a -> 'a
  type ('a, 'res) lift = 'a -> 'res
  type ('ctx, 'a, 'res) lift_map_with_context = 'ctx -> 'a -> 'a * 'res
end

class map =
  let any x = x in
  object
    method int : int T.map = any
    method string : string T.map = any
    method bool : bool T.map = any
    method char : char T.map = any

    method option : 'a. 'a T.map -> 'a option T.map =
      fun f x -> match x with None -> None | Some x -> Some (f x)

    method list : 'a. 'a T.map -> 'a list T.map = List.map
    method array : 'a. 'a T.map -> 'a array T.map = Array.map
  end

class iter =
  let any = ignore in
  object
    method int : int T.iter = any
    method string : string T.iter = any
    method bool : bool T.iter = any
    method char : char T.iter = any

    method option : 'a. 'a T.iter -> 'a option T.iter =
      fun f x -> match x with None -> () | Some x -> f x

    method list : 'a. 'a T.iter -> 'a list T.iter = List.iter
    method array : 'a. 'a T.iter -> 'a array T.iter = Array.iter
  end

class ['acc] fold =
  let any _ acc = acc in
  object
    method int : (int, 'acc) T.fold = any
    method string : (string, 'acc) T.fold = any
    method bool : (bool, 'acc) T.fold = any
    method char : (char, 'acc) T.fold = any

    method option : 'a. ('a, 'acc) T.fold -> ('a option, 'acc) T.fold =
      fun f x acc -> match x with None -> acc | Some x -> f x acc

    method list : 'a. ('a, 'acc) T.fold -> ('a list, 'acc) T.fold =
      let rec loop f l acc =
        match l with [] -> acc | x :: l -> loop f l (f x acc)
      in
      loop

    method array : 'a. ('a, 'acc) T.fold -> ('a array, 'acc) T.fold =
      fun f a acc ->
        let r = ref acc in
        for i = 0 to Array.length a - 1 do
          r := f (Array.unsafe_get a i) !r
        done;
        !r
  end

class ['acc] fold_map =
  let any x acc = (x, acc) in
  object
    method int : (int, 'acc) T.fold_map = any
    method string : (string, 'acc) T.fold_map = any
    method bool : (bool, 'acc) T.fold_map = any
    method char : (char, 'acc) T.fold_map = any

    method option : 'a. ('a, 'acc) T.fold_map -> ('a option, 'acc) T.fold_map =
      fun f x acc ->
        match x with
        | None -> (None, acc)
        | Some x ->
            let x, acc = f x acc in
            (Some x, acc)

    method list : 'a. ('a, 'acc) T.fold_map -> ('a list, 'acc) T.fold_map =
      let rec loop f l acc =
        match l with
        | [] -> ([], acc)
        | x :: l ->
            let x, acc = f x acc in
            let l, acc = loop f l acc in
            (x :: l, acc)
      in
      loop

    method array : 'a. ('a, 'acc) T.fold_map -> ('a array, 'acc) T.fold_map =
      fun f a acc ->
        let len = Array.length a in
        if len = 0 then (a, acc)
        else
          let x, acc = f (Array.unsafe_get a 0) acc in
          let a' = Array.make len x in
          let r = ref acc in
          for i = 1 to len - 1 do
            let x, acc = f (Array.unsafe_get a i) !r in
            Array.unsafe_set a' i x;
            r := acc
          done;
          (a', !r)
  end

class ['ctx] map_with_context =
  let any _ x = x in
  object
    method int : ('ctx, int) T.map_with_context = any
    method string : ('ctx, string) T.map_with_context = any
    method bool : ('ctx, bool) T.map_with_context = any
    method char : ('ctx, char) T.map_with_context = any

    method option
        : 'a.
          ('ctx, 'a) T.map_with_context -> ('ctx, 'a option) T.map_with_context
        =
      fun f ctx x -> match x with None -> None | Some x -> Some (f ctx x)

    method list
        : 'a.
          ('ctx, 'a) T.map_with_context -> ('ctx, 'a list) T.map_with_context =
      fun f ctx l -> List.map (f ctx) l

    method array
        : 'a.
          ('ctx, 'a) T.map_with_context -> ('ctx, 'a array) T.map_with_context =
      fun f ctx a -> Array.map (f ctx) a
  end

class virtual ['res] lift =
  object (self)
    method virtual other : 'a. ('a, 'res) T.lift
    method virtual int : (int, 'res) T.lift
    method virtual string : (string, 'res) T.lift
    method virtual bool : (bool, 'res) T.lift
    method virtual char : (char, 'res) T.lift
    method virtual array : 'a. ('a, 'res) T.lift -> ('a array, 'res) T.lift
    method virtual float : (float, 'res) T.lift
    method virtual int32 : (int32, 'res) T.lift
    method virtual int64 : (int64, 'res) T.lift
    method virtual nativeint : (nativeint, 'res) T.lift
    method virtual unit : (unit, 'res) T.lift
    method virtual record : (string * 'res) list -> 'res
    method virtual constr : string -> 'res list -> 'res
    method virtual tuple : 'res list -> 'res

    method option : 'a. ('a, 'res) T.lift -> ('a option, 'res) T.lift =
      fun f x ->
        match x with
        | None -> self#constr "None" []
        | Some x -> self#constr "Some" [ f x ]

    method list : 'a. ('a, 'res) T.lift -> ('a list, 'res) T.lift =
      fun f l ->
        match l with
        | [] -> self#constr "[]" []
        | x :: l -> self#constr "::" [ f x; self#list f l ]
  end

class virtual ['ctx, 'res] lift_map_with_context =
  object (self)
    method virtual other : 'a. 'ctx -> 'a -> 'res
    method virtual int : ('ctx, int, 'res) T.lift_map_with_context
    method virtual string : ('ctx, string, 'res) T.lift_map_with_context
    method virtual bool : ('ctx, bool, 'res) T.lift_map_with_context
    method virtual char : ('ctx, char, 'res) T.lift_map_with_context

    method virtual array
        : 'a.
          ('ctx, 'a, 'res) T.lift_map_with_context ->
          ('ctx, 'a array, 'res) T.lift_map_with_context

    method virtual float : ('ctx, float, 'res) T.lift_map_with_context
    method virtual int32 : ('ctx, int32, 'res) T.lift_map_with_context
    method virtual int64 : ('ctx, int64, 'res) T.lift_map_with_context
    method virtual nativeint : ('ctx, nativeint, 'res) T.lift_map_with_context
    method virtual unit : ('ctx, unit, 'res) T.lift_map_with_context
    method virtual record : 'ctx -> (string * 'res) list -> 'res
    method virtual constr : 'ctx -> string -> 'res list -> 'res
    method virtual tuple : 'ctx -> 'res list -> 'res

    method option
        : 'a.
          ('ctx, 'a, 'res) T.lift_map_with_context ->
          ('ctx, 'a option, 'res) T.lift_map_with_context =
      fun f ctx x ->
        match x with
        | None -> (None, self#constr ctx "None" [])
        | Some x ->
            let x, res = f ctx x in
            (Some x, self#constr ctx "Some" [ res ])

    method list
        : 'a.
          ('ctx, 'a, 'res) T.lift_map_with_context ->
          ('ctx, 'a list, 'res) T.lift_map_with_context =
      fun f ctx l ->
        match l with
        | [] -> ([], self#constr ctx "[]" [])
        | x :: l ->
            let x, res_head = f ctx x in
            let l, res_tail = self#list f ctx l in
            (x :: l, self#constr ctx "::" [ res_head; res_tail ])
  end

class type ['res] std_lifters =
  object
    method other : 'a. ('a, 'res) T.lift
    method int : (int, 'res) T.lift
    method string : (string, 'res) T.lift
    method bool : (bool, 'res) T.lift
    method char : (char, 'res) T.lift
    method array : 'a. ('a, 'res) T.lift -> ('a array, 'res) T.lift
    method record : (string * 'res) list -> 'res
    method constr : string -> 'res list -> 'res
    method tuple : 'res list -> 'res
    method float : (float, 'res) T.lift
    method int32 : (int32, 'res) T.lift
    method int64 : (int64, 'res) T.lift
    method nativeint : (nativeint, 'res) T.lift
    method unit : (unit, 'res) T.lift
    method option : 'a. ('a, 'res) T.lift -> ('a option, 'res) T.lift
    method list : 'a. ('a, 'res) T.lift -> ('a list, 'res) T.lift
  end

class type ['ctx, 'res] std_lift_mappers_with_context =
  object
    method other : 'a. 'ctx -> 'a -> 'res
    method int : ('ctx, int, 'res) T.lift_map_with_context
    method string : ('ctx, string, 'res) T.lift_map_with_context
    method bool : ('ctx, bool, 'res) T.lift_map_with_context
    method char : ('ctx, char, 'res) T.lift_map_with_context

    method array :
      'a.
      ('ctx, 'a, 'res) T.lift_map_with_context ->
      ('ctx, 'a array, 'res) T.lift_map_with_context

    method record : 'ctx -> (string * 'res) list -> 'res
    method constr : 'ctx -> string -> 'res list -> 'res
    method tuple : 'ctx -> 'res list -> 'res
    method float : ('ctx, float, 'res) T.lift_map_with_context
    method int32 : ('ctx, int32, 'res) T.lift_map_with_context
    method int64 : ('ctx, int64, 'res) T.lift_map_with_context
    method nativeint : ('ctx, nativeint, 'res) T.lift_map_with_context
    method unit : ('ctx, unit, 'res) T.lift_map_with_context

    method option :
      'a.
      ('ctx, 'a, 'res) T.lift_map_with_context ->
      ('ctx, 'a option, 'res) T.lift_map_with_context

    method list :
      'a.
      ('ctx, 'a, 'res) T.lift_map_with_context ->
      ('ctx, 'a list, 'res) T.lift_map_with_context
  end
