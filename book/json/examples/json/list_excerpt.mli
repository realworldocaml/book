[@@@part "0"];;
val map  : 'a list -> f:('a -> 'b)   -> 'b list
val fold : 'a list -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum


[@@@part "1"]
val iter : 'a list -> f:('a -> unit) -> unit
