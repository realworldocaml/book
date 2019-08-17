(* Examples from the documentation, this code is in public domain. *)

(* Read lines *)

let lines ?encoding (src : [`Channel of in_channel | `String of string]) =
  let rec loop d buf acc = match Uutf.decode d with
  | `Uchar u ->
      begin match Uchar.to_int u with
      | 0x000A ->
          let line = Buffer.contents buf in
          Buffer.clear buf; loop d buf (line :: acc)
      | _ ->
          Uutf.Buffer.add_utf_8 buf u; loop d buf acc
      end
  | `End -> List.rev (Buffer.contents buf :: acc)
  | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep; loop d buf acc
  | `Await -> assert false
  in
  let nln = `Readline (Uchar.of_int 0x000A) in
  loop (Uutf.decoder ~nln ?encoding src) (Buffer.create 512) []

let lines_fd ?encoding (fd : Unix.file_descr) =
  let rec loop fd s d buf acc = match Uutf.decode d with
  | `Uchar u ->
      begin match Uchar.to_int u with
      | 0x000A ->
          let line = Buffer.contents buf in
          Buffer.clear buf; loop fd s d buf (line :: acc)
      | _ ->
          Uutf.Buffer.add_utf_8 buf u; loop fd s d buf acc
      end
  | `End -> List.rev (Buffer.contents buf :: acc)
  | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep; loop fd s d buf acc
  | `Await ->
      let rec unix_read fd s j l = try Unix.read fd s j l with
      | Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd s j l
      in
      let rc = unix_read fd s 0 (Bytes.length s) in
      Uutf.Manual.src d s 0 rc; loop fd s d buf acc
  in
  let s = Bytes.create 65536 (* UNIX_BUFFER_SIZE in 4.0.0 *) in
  let nln = `Readline (Uchar.of_int 0x000A) in
  loop fd s (Uutf.decoder ~nln ?encoding `Manual) (Buffer.create 512) []

(* Recode *)

let recode ?nln ?encoding out_encoding
    (src : [`Channel of in_channel | `String of string])
    (dst : [`Channel of out_channel | `Buffer of Buffer.t])
  =
  let rec loop d e = match Uutf.decode d with
  | `Uchar _ as u -> ignore (Uutf.encode e u); loop d e
  | `End -> ignore (Uutf.encode e `End)
  | `Malformed _ -> ignore (Uutf.encode e (`Uchar Uutf.u_rep)); loop d e
  | `Await -> assert false
  in
  let d = Uutf.decoder ?nln ?encoding src in
  let e = Uutf.encoder out_encoding dst in
  loop d e

let recode_fd ?nln ?encoding out_encoding
    (fdi : Unix.file_descr)
    (fdo : Unix.file_descr)
  =
  let rec encode fd s e v = match Uutf.encode e v with `Ok -> ()
  | `Partial ->
      let rec unix_write fd s j l =
        let rec write fd s j l = try Unix.single_write fd s j l with
        | Unix.Unix_error (Unix.EINTR, _, _) -> write fd s j l
        in
        let wc = write fd s j l in
        if wc < l then unix_write fd s (j + wc) (l - wc) else ()
      in
      unix_write fd s 0 (Bytes.length s - Uutf.Manual.dst_rem e);
      Uutf.Manual.dst e s 0 (Bytes.length s);
      encode fd s e `Await
  in
  let rec loop fdi fdo ds es d e = match Uutf.decode d with
  | `Uchar _ as u -> encode fdo es e u; loop fdi fdo ds es d e
  | `End -> encode fdo es e `End
  | `Malformed _ -> encode fdo es e (`Uchar Uutf.u_rep); loop fdi fdo ds es d e
  | `Await ->
      let rec unix_read fd s j l = try Unix.read fd s j l with
      | Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd s j l
      in
      let rc = unix_read fdi ds 0 (Bytes.length ds) in
      Uutf.Manual.src d ds 0 rc; loop fdi fdo ds es d e
  in
  let ds = Bytes.create 65536 (* UNIX_BUFFER_SIZE in 4.0.0 *) in
  let es = Bytes.create 65536 (* UNIX_BUFFER_SIZE in 4.0.0 *) in
  let d = Uutf.decoder ?nln ?encoding `Manual in
  let e = Uutf.encoder out_encoding `Manual in
  Uutf.Manual.dst e es 0 (Bytes.length es);
  loop fdi fdo ds es d e
