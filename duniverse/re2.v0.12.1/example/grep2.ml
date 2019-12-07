open StdLabels

let usage = Format.sprintf "%s [options] pattern" (Array.get Sys.argv 0)

let pattern = ref ""
let rewrite = ref "\\0"
let underline = ref false
let only_matching = ref false
let str_sub = ref ""

(* this is just drudgery *)
let () =
  Arg.parse [
    "--underline", Arg.Set underline, "underline rewrites";
    "--rewrite", Arg.Set_string rewrite, "rewrite the match";
    "--only-matching", Arg.Set only_matching, "only print the match, not the line";
    "--extract-submatch", Arg.Set_string str_sub,
      "ignored unless --only-matching is also passed; whole match is submatch 0";
  ]
  (fun pattern' -> pattern := pattern')
  usage
;;

(* this is the only interesting (i.e., Re2-using) part *)
let () =
  if !underline then
    let re = Re2.create_exn "\\\\[0-9]" in
    let template = Format.sprintf "%c[4m\\0%c[24m" (Char.chr 27) (Char.chr 27) in
    match  (Re2.rewrite re ~template !rewrite) with
    | Core_kernel.Result.Ok s -> rewrite := s
    | Core_kernel.Result.Error _ -> ()
;;

let re =
  if !pattern = ""
  then raise (Failure (Format.sprintf "invalid pattern /%s/" !pattern))
  else begin
    try
      Re2.create_exn !pattern
    with
    | _ ->
      raise (Failure (Format.sprintf "invalid pattern /%s/" !pattern))
  end
;;

let sub =
  if !str_sub = "" then `Index 0
  else let id = try `Index (int_of_string !str_sub) with _ -> `Name !str_sub in
  `Index (Re2.index_of_id_exn re id)
;;

let grep str =
  try
    if not (Re2.matches re str) then None else
      let str = if not !only_matching then str else Re2.find_first_exn ~sub re str in
      Some (Re2.rewrite re ~template:(!rewrite) str)
  with
  | err -> raise err
;;

(* okay, more drudgery *)
let _ =
  let rec iter ~unfold ~fold ~init seed =
    match unfold seed with
    | None -> init
    | Some (x, seed) -> iter ~unfold ~fold ~init:(fold init x) seed
  in
  iter
  ~unfold:(fun channel ->
    match
      try grep (input_line channel) with
      | End_of_file -> None
      | err -> raise err
    with
    | None -> None
    | Some r -> Some (r, channel))
  ~fold:(fun channel str_result ->
    match str_result with
    | Core_kernel.Result.Error _ -> channel
    | Core_kernel.Result.Ok str -> output_string channel (str ^ "\n") ; channel)
  ~init:stdout
  stdin
;;
