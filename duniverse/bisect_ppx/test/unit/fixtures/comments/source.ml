(*BISECT-IGNORE-BEGIN*)
let f1 x y =
  if x = y then
    x + y
  else
    x - y
  (*BISECT-IGNORE-END*)

let g s =
  if true then
    for i = 1 to 5 do
      print_endline s
    done
  else
    assert false (*BISECT-MARK*)

let f2 b x = (*BISECT-IGNORE*)
  if b then  (*BISECT-IGNORE*)
    x * x    (*BISECT-IGNORE*)
  else       (*BISECT-IGNORE*)
    x        (*BISECT-IGNORE*)

(* Test the parsing of weird quote literals. *)
let s1 = "\\\\"
let s2 = '"'
(* let s2 = '"' *)
let s3 = '\"'
(* let s3 = '\"' *)
let s4 = "a\"a"

