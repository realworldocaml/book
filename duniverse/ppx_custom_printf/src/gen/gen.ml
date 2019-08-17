open StdLabels
open Format

let lsplit2 s ~on =
  match String.index s on with
  | exception Not_found -> None
  | i ->
    Some (String.sub s ~pos:0 ~len:i,
          String.sub s ~pos:(i + 1) ~len:(String.length s - i - 1))

let () =
  let oc =
    match Sys.argv with
    | [|_; "-o"; fn|] ->
      open_out_bin fn
    | _ ->
      failwith "bad command line arguments"
  in

  try
    let buf = Buffer.create 512 in
    let pp = formatter_of_buffer buf in
    pp_set_margin pp max_int; (* so we can parse line by line below *)
    Toploop.initialize_toplevel_env ();
    assert (
      Lexing.from_string "include CamlinternalFormatBasics;;"
      |> !Toploop.parse_toplevel_phrase
      |> Toploop.execute_phrase true pp
    );
    let types =
      Buffer.contents buf
      |> Str.split (Str.regexp "\n")
      |> List.fold_left ~init:(false, []) ~f:(fun (in_type_group, acc) s ->
        match lsplit2 s ~on:' ' with
        | Some ("type", s) ->
          (true, s :: acc)
        | Some ("and", s) when in_type_group ->
          (true, s :: acc)
        | _ -> (false, acc))
      |> snd
      |> List.rev
    in
    let s =
      String.concat ~sep:"\n"
        (match types with
         | [] -> []
         | x :: l ->
           ("type " ^ x) ::
           List.map l ~f:((^) "and ") @
           ["[@@deriving traverse_lift]"])
    in
    let intf = Parse.interface (Lexing.from_string s) in
    let ppf = formatter_of_out_channel oc in
    fprintf ppf "%a@." Pprintast.signature intf;
    close_out oc
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
