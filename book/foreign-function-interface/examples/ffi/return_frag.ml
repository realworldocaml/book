[@@@part "0"];;
(* correct types *)
val time: ptr time_t @-> returning time_t
val difftime: time_t @-> time_t @-> returning double

[@@@part "1"] ;;

(* incorrect types *)
val time: ptr time_t @-> time_t
val difftime: time_t @-> time_t @-> double

[@@@part "2"] ;;

val curried : int -> int -> int

[@@@part "3"] ;;

val curried : int -> (int -> int)
