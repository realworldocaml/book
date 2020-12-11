let read f =
  try
    let ic = open_in_bin f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    let b = Bigstringaf.create n in
    Bigstringaf.blit_from_bytes s ~src_off:0 b ~dst_off:0 ~len:n;
    b
  with e ->
    failwith (Printf.sprintf "Cannot read content of %s.\n%s" f (Printexc.to_string e))
;;

let () =
  let twitter_big = read Sys.argv.(1) in
  match Angstrom.(parse_bigstring RFC7159.json twitter_big) with
  | Ok _ -> ()
  | Error err -> failwith err
