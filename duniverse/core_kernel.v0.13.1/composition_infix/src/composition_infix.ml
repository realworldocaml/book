open Base

let ( >> ) f g = Fn.compose g f
let ( << ) f g = Fn.compose f g
