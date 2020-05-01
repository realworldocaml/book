(* This is so not thread-safe it's not even funny. *)

let current = ref None

let active ~hook f =
  let prev = !current in
  current := Some hook ;
  try
    let res = f () in
    ( current := prev ; res )
  with exn -> ( current := prev ; raise exn )

let form_trace id sexp =
  let open Sexplib in
  Sexp.(List [ Atom id ; sexp ])

let is_tracing () = !current <> None

let sexp ~tag lz =
  match !current with
  | None      -> ()
  | Some hook -> hook @@ form_trace tag (Lazy.force lz)

let sexps ~tag lzs = if is_tracing () then List.iter (sexp ~tag) lzs

let sexpf ~tag ~f x = sexp ~tag @@ lazy (f x)

let sexpfs ~tag ~f xs = if is_tracing () then List.iter (sexpf ~tag ~f) xs

let cs ~tag = sexpf ~tag ~f:Cstruct_sexp.sexp_of_t

let css ~tag css = if is_tracing () then List.iter (cs ~tag) css
