(* This code is in the public domain *)

(* Generic JSON tree type *)

type json =
  [ `Null | `Bool of bool | `Float of float| `String of string
  | `A of json list | `O of (string * json) list ]

exception Escape of ((int * int) * (int * int)) * Jsonm.error

let json_of_src ?encoding
    (src : [`Channel of in_channel | `String of string])
  =
  let dec d = match Jsonm.decode d with
  | `Lexeme l -> l
  | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
  | `End | `Await -> assert false
  in
  let rec value v k d = match v with
  | `Os -> obj [] k d  | `As -> arr [] k d
  | `Null | `Bool _ | `String _ | `Float _ as v -> k v d
  | _ -> assert false
  and arr vs k d = match dec d with
  | `Ae -> k (`A (List.rev vs)) d
  | v -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d = match dec d with
  | `Oe -> k (`O (List.rev ms)) d
  | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
  | _ -> assert false
  in
  let d = Jsonm.decoder ?encoding src in
  try `JSON (value (dec d) (fun v _ -> v) d) with
  | Escape (r, e) -> `Error (r, e)

let json_to_dst ~minify
    (dst : [`Channel of out_channel | `Buffer of Buffer.t ])
    (json : json)
  =
  let enc e l = ignore (Jsonm.encode e (`Lexeme l)) in
  let rec value v k e = match v with
  | `A vs -> arr vs k e
  | `O ms -> obj ms k e
  | `Null | `Bool _ | `Float _ | `String _ as v -> enc e v; k e
  and arr vs k e = enc e `As; arr_vs vs k e
  and arr_vs vs k e = match vs with
  | v :: vs' -> value v (arr_vs vs' k) e
  | [] -> enc e `Ae; k e
  and obj ms k e = enc e `Os; obj_ms ms k e
  and obj_ms ms k e = match ms with
  | (n, v) :: ms -> enc e (`Name n); value v (obj_ms ms k) e
  | [] -> enc e `Oe; k e
  in
  let e = Jsonm.encoder ~minify dst in
  let finish e = ignore (Jsonm.encode e `End) in
  value json finish e

let main () =
  let exec = Filename.basename Sys.executable_name in
  let usage = Printf.sprintf
    "Usage: %s [OPTION]...\n\
    \ Recode JSON from stdin to stdout via an in-memory tree representation.\n\
     Options:" exec
  in
  let minify = ref true in
  let options = [ "-pp", Arg.Clear minify, " Pretty print output"; ] in
  let anon _ = raise (Arg.Bad "illegal argument") in
  Arg.parse (Arg.align options) anon usage;
  let minify = !minify in
  match json_of_src (`Channel stdin) with
  | `JSON j -> json_to_dst ~minify (`Channel stdout) j
  | `Error (((l1, c1), (l2, c2)), e) ->
      Format.eprintf "-:%d.%d-%d.%d: %a\n%!" l1 c1 l2 c2 Jsonm.pp_error e

let () = main ()
