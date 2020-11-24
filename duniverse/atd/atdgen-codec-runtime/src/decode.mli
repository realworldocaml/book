type 'a t = Json.t -> 'a

val make : (Json.t -> 'a) -> 'a t

val decode : 'a t -> Json.t -> 'a

val unit : unit t
val bool : bool t
val int : int t
val float : float t
val char : char t
val string : string t

val optional : 'a t -> 'a option t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t
val obj_list : 'a t -> (string * 'a) list t
val obj_array : 'a t -> (string * 'a) array t

(* a field that should be present *)
val field : string -> 'a t -> 'a t

(* a field that turns into a an optional value when absent *)
val fieldDefault : string -> 'a -> 'a t -> 'a t

(* a field that returns None when is absent *)
val fieldOptional : string -> 'a t -> 'a option t

val map : ('a -> 'b) -> 'a t -> 'b t

val tuple1 : 'a t -> 'a t
val tuple2 : 'a t -> 'b t -> ('a * 'b) t
val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val enum : (string * [`Single of 'a | `Decode of 'a t]) list -> 'a t

val option_as_constr : 'a t -> 'a option t

val nullable : 'a t -> 'a option t

val adapter: (Json.t -> Json.t) -> 'a t -> 'a t
