type 'a t = 'a -> Json.t

val make : ('a -> Json.t) -> 'a t

val encode : 'a t -> 'a -> Json.t

val unit : unit t
val string : string t
val float : float t
val int : int t
val bool : bool t
val char : char t

val list : 'a t -> 'a list t
val array : 'a t -> 'a array t

val int32 : int32 t
val int64 : int64 t

type field

val field : ?default:'a -> 'a t -> name:string -> 'a -> field
val field_o : ?default:'a -> 'a t -> name:string -> 'a option -> field

val obj : field list -> Json.t

val tuple1 : 'a t -> 'a t
val tuple2 : 'a t -> 'b t -> ('a * 'b) t
val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val constr0 : string -> Json.t
val constr1 : string -> 'a t -> 'a -> Json.t

val contramap : ('b -> 'a) -> 'a t -> 'b t

val nullable : 'a t -> 'a option t

val option_as_constr : 'a t -> 'a option t

val adapter: (Json.t -> Json.t) -> 'a t -> 'a t
