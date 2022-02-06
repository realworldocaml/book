open! Import

type 'a t = 'a loc = { txt : 'a; loc : Location.t }

let txt t = t.txt

let loc t = t.loc

let make ~loc txt = { loc; txt }

let map t ~f = { t with txt = f t.txt }
