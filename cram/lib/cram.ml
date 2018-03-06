type item = Lexer.item
type t = item list

let parse_lexbuf = Lexer.file
let parse_file f = Lexer.file (snd (Common.init f))

let run n ~f =
  Common.run_expect_test n ~f:(fun c l ->
      f c (parse_lexbuf l)
    )


let part n t =
  let rec record acc = function
    | [] | `Part _ :: _ -> Some (List.rev acc)
    | h::t -> record (h::acc) t
  in
  let rec aux = function
    | []                       -> None
    | `Part i :: t when i=n    -> record [] t
    | _       :: t             -> aux t
  in
  aux t

let pp_item ppf = function
  | `Output s  -> Fmt.pf ppf "  %s\n" s
  | `Part s    -> Fmt.pf ppf "### %s\n" s
  | `Comment s -> Fmt.pf ppf "%s\n" s
  | `Command s -> Fmt.pf ppf "  $ %s\n" s

let pp ppf t = List.iter (pp_item ppf) t
let to_string = Fmt.to_to_string pp
