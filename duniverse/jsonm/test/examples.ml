(* Examples form the documentation (see also jtree.ml), this code is in public
   domain.  *)

(* Trip *)

let trip ?encoding ?minify
    (src : [`Channel of in_channel | `String of string])
    (dst : [`Channel of out_channel | `Buffer of Buffer.t])
  =
  let rec loop d e = match Jsonm.decode d with
  | `Lexeme _ as v -> ignore (Jsonm.encode e v); loop d e
  | `End -> ignore (Jsonm.encode e `End); `Ok
  | `Error err -> `Error (Jsonm.decoded_range d, err)
  | `Await -> assert false
  in
  let d = Jsonm.decoder ?encoding src in
  let e = Jsonm.encoder ?minify dst in
  loop d e

let trip_fd ?encoding ?minify
    (fdi : Unix.file_descr)
    (fdo : Unix.file_descr)
  =
  let rec encode fd s e v = match Jsonm.encode e v with `Ok -> ()
  | `Partial ->
      let rec unix_write fd s j l =
        let rec write fd s j l = try Unix.single_write fd s j l with
        | Unix.Unix_error (Unix.EINTR, _, _) -> write fd s j l
        in
        let wc = write fd s j l in
        if wc < l then unix_write fd s (j + wc) (l - wc) else ()
      in
      unix_write fd s 0 (Bytes.length s - Jsonm.Manual.dst_rem e);
      Jsonm.Manual.dst e s 0 (Bytes.length s);
      encode fd s e `Await
  in
  let rec loop fdi fdo ds es d e = match Jsonm.decode d with
  | `Lexeme _ as v -> encode fdo es e v; loop fdi fdo ds es d e
  | `End -> encode fdo es e `End; `Ok
  | `Error err -> `Error (Jsonm.decoded_range d, err)
  | `Await ->
      let rec unix_read fd s j l = try Unix.read fd s j l with
      | Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd s j l
      in
      let rc = unix_read fdi ds 0 (Bytes.length ds) in
      Jsonm.Manual.src d ds 0 rc; loop fdi fdo ds es d e
  in
  let ds = Bytes.create 65536 (* UNIX_BUFFER_SIZE in 4.0.0 *) in
  let es = Bytes.create 65536 (* UNIX_BUFFER_SIZE in 4.0.0 *) in
  let d = Jsonm.decoder ?encoding `Manual in
  let e = Jsonm.encoder ?minify `Manual in
  Jsonm.Manual.dst e es 0 (Bytes.length es);
  loop fdi fdo ds es d e

(* Member selection *)

let memsel ?encoding names
    (src : [`Channel of in_channel | `String of string])
  =
  let rec loop acc names d = match Jsonm.decode d with
  | `Lexeme (`Name n) when List.mem n names ->
      begin match Jsonm.decode d with
      | `Lexeme (`String s) -> loop (s :: acc) names d
      | _ -> loop acc names d
      end
  | `Lexeme _ | `Error _ -> loop acc names d
  | `End -> List.rev acc
  | `Await -> assert false
  in
  loop [] names (Jsonm.decoder ?encoding src)
