open Ppxlib

module Filename = Caml.Filename

let dirname = ref None

let set_dirname dn = dirname := dn

let () =
  Driver.add_arg "-dirname"
    (String (fun s -> dirname := Some s))
    ~doc:"<dir> Name of the current directory relative to the root of the project"

let chop_dot_slash_prefix ~fname =
  match Base.String.chop_prefix ~prefix:"./" fname with
  | Some fname -> fname
  | None -> fname

let expand_filename fname =
  match Filename.is_relative fname, !dirname with
  | true, Some dirname ->
    (* If [dirname] is given and [fname] is relative, then prepend [dirname]. *)
    Filename.concat dirname (chop_dot_slash_prefix ~fname)
  | _ -> fname

let lift_position ~loc =
  let (module Builder) = Ast_builder.make loc in
  let open Builder in
  let pos = loc.Location.loc_start in
  let id = Located.lident in
  pexp_record
    [ id "Lexing.pos_fname" , estring (expand_filename pos.Lexing.pos_fname)
    ; id "pos_lnum"         , eint    pos.Lexing.pos_lnum
    ; id "pos_cnum"         , eint    pos.Lexing.pos_cnum
    ; id "pos_bol"          , eint    pos.Lexing.pos_bol
    ] None

let lift_position_as_string ~(loc : Location.t) =
  let { Lexing. pos_fname; pos_lnum; pos_cnum; pos_bol } = loc.loc_start in
  Ast_builder.Default.estring ~loc
    (Printf.sprintf "%s:%d:%d" (expand_filename pos_fname) pos_lnum
       (pos_cnum - pos_bol))
;;
