(* Copyright (c) 2016 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Cpuid

let pp_list pp ppf = Format.(function
  | []    -> pp_print_string ppf "[]"
  | [x]   -> fprintf ppf "[%a]" pp x
  | x::xs -> fprintf ppf "@[%a%a@]" pp x
              (fun _ -> List.iter (fprintf ppf "@ %a" pp)) xs)

let (>>=) r k = match r with Ok x -> k x | Error e -> Error e
let (>>|) r k = r >>= fun x -> Ok (k x)

let () =
  let pr =
    (vendor () >>| Format.printf "vendor: %a\n%!" pp_vendor)
    >>= fun () ->
    (model () >>| fun (f, m, s) ->
      Format.printf "family: %d\nmodel: %d\nstepping: %d\n%!" f m s)
    >>= fun () ->
    (cores () >>| Format.printf "cores: %d\n%!")
    >>= fun () ->
    (flags () >>| Format.printf "flags: %a\n%!" (pp_list pp_flag))
  in match pr with Ok () -> ()
  | Error e -> Format.printf "Error: %a\n%!" pp_error e
