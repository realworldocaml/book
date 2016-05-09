

[@@@part "1"];;
type shape = < variant : repr; area : float>
and circle = < variant : repr; area : float; radius : int >
and line = < variant : repr; area : float; length : int >
and repr =
 | Circle of circle
 | Line of line;;

let is_barbell = function
| [s1; s2; s3] ->
   (match s1#variant, s2#variant, s3#variant with
    | Circle c1, Line _, Circle c2 when c1#radius = c2#radius -> true
    | _ -> false)
| _ -> false;;
