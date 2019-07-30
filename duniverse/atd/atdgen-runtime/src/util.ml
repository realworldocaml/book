
type 'a ocaml_array = 'a array

let input_file fname read =
  let ic = open_in_bin fname in
  try
    let x = read ic in
    close_in ic;
    x
  with e ->
    close_in_noerr ic;
    raise e

let output_file fname write =
  let oc = open_out_bin fname in
  try
    write oc;
    close_out oc
  with e ->
    close_out_noerr oc;
    raise e

module Biniou =
struct
  type 'a reader = Bi_inbuf.t -> 'a
  type 'a writer = Bi_outbuf.t -> 'a -> unit

  let from_channel ?len ?(shrlen = 0) read ic =
    let ib = Bi_inbuf.from_channel ?len ~shrlen ic in
    read ib

  let from_file ?len ?(shrlen = 0) read fname =
    input_file fname (fun ic -> from_channel ?len ~shrlen read ic)

  let to_channel ?len ?(shrlen = 0) write oc x =
    let ob = Bi_outbuf.create_channel_writer ?len ~shrlen oc in
    write ob x;
    Bi_outbuf.flush_channel_writer ob

  let to_file ?len ?(shrlen = 0) write fname x =
    output_file fname (fun oc -> to_channel ?len ~shrlen write oc x)
end

module Json =
struct
  type 'a reader = Yojson.Safe.lexer_state -> Lexing.lexbuf -> 'a
  type 'a writer = Bi_outbuf.t -> 'a -> unit

  let finish ls lexbuf =
    Yojson.Safe.read_space ls lexbuf;
    if not (Yojson.Safe.read_eof lexbuf) then
      Yojson.json_error "Junk after end of JSON value"

  let from_lexbuf ?(stream = false) read ls lexbuf =
    Yojson.Safe.read_space ls lexbuf;

    let x =
      if Yojson.Safe.read_eof lexbuf then
        raise Yojson.End_of_input
      else
        read ls lexbuf
    in

    if not stream then
      finish ls lexbuf;

    x

  let from_string ?buf ?fname ?lnum read s =
    let lexbuf = Lexing.from_string s in
    let ls = Yojson.Safe.init_lexer ?buf ?fname ?lnum () in
    from_lexbuf read ls lexbuf

  let from_channel ?buf ?fname ?lnum read ic =
    let lexbuf = Lexing.from_channel ic in
    let ls = Yojson.Safe.init_lexer ?buf ?fname ?lnum () in
    from_lexbuf read ls lexbuf

  let from_file ?buf ?fname:src ?lnum read fname =
    let fname0 =
      match src with
          None -> fname
        | Some s -> s
    in
    input_file fname (fun ic -> from_channel ?buf ~fname:fname0 ?lnum read ic)

  let stream_from_lexbuf ?(fin = fun () -> ()) read ls lexbuf =
    let stream = Some true in
    let f _ =
      try Some (from_lexbuf ?stream read ls lexbuf)
      with
          Yojson.End_of_input ->
            fin ();
            None
        | e ->
            (try fin () with _ -> ());
            raise e
    in
    Stream.from f

  let stream_from_string ?buf ?fin ?fname ?lnum read ic =
    let lexbuf = Lexing.from_string ic in
    let ls = Yojson.Safe.init_lexer ?buf ?fname ?lnum () in
    stream_from_lexbuf ?fin read ls lexbuf

  let stream_from_channel ?buf ?fin ?fname ?lnum read ic =
    let lexbuf = Lexing.from_channel ic in
    let ls = Yojson.Safe.init_lexer ?buf ?fname ?lnum () in
    stream_from_lexbuf ?fin read ls lexbuf

  let stream_from_file ?buf ?(fin = fun () -> ()) ?fname:src ?lnum read fname =
    let fname0 =
      match src with
          None -> fname
        | Some s -> s
    in
    let ic = open_in_bin fname in
    let fin () = close_in_noerr ic; fin () in
    stream_from_channel ?buf ~fin ~fname:fname0 ?lnum read ic

  let list_from_string ?buf ?fin ?fname ?lnum read ic =
    let stream = stream_from_string ?buf ?fin ?fname ?lnum read ic in
    let acc = ref [] in
    Stream.iter (fun x -> acc := x :: !acc) stream;
    List.rev !acc

  let list_from_channel ?buf ?fin ?fname ?lnum read ic =
    let stream = stream_from_channel ?buf ?fin ?fname ?lnum read ic in
    let acc = ref [] in
    Stream.iter (fun x -> acc := x :: !acc) stream;
    List.rev !acc

  let list_from_file ?buf ?fname:src ?lnum read fname =
    let fname0 =
      match src with
          None -> fname
        | Some s -> s
    in
    let ic = open_in_bin fname in
    let fin () = close_in_noerr ic in
    list_from_channel ?buf ~fin ~fname:fname0 ?lnum read ic

  let to_string ?(len = 1024) write x =
    let ob = Bi_outbuf.create len in
    write ob x;
    Bi_outbuf.contents ob

  let to_channel ?len write oc x = Biniou.to_channel ?len ~shrlen:0 write oc x
  let to_file ?len write fname x = Biniou.to_file ?len ~shrlen:0 write fname x

  let stream_to_string ?(len = 1024) ?(lf = "\n") write stream =
    let ob = Bi_outbuf.create len in
    Stream.iter (fun x -> write ob x; Bi_outbuf.add_string ob lf) stream;
    Bi_outbuf.contents ob

  let stream_to_channel ?len ?(lf = "\n") write oc stream =
    let ob = Bi_outbuf.create_channel_writer ?len ~shrlen:0 oc in
    Stream.iter (fun x -> write ob x; Bi_outbuf.add_string ob lf) stream;
    Bi_outbuf.flush_channel_writer ob

  let stream_to_file ?len ?lf write fname stream =
    output_file fname (fun oc -> stream_to_channel ?len ?lf write oc stream)

  let list_to_string ?len ?lf write l =
    stream_to_string ?len ?lf write (Stream.of_list l)

  let list_to_channel ?len ?lf write oc l =
    stream_to_channel ?len ?lf write oc (Stream.of_list l)

  let list_to_file ?len ?lf write fname  l =
    stream_to_file ?len ?lf write fname (Stream.of_list l)

  let preset_unknown_field_handler loc name =
    let msg =
      Printf.sprintf
        "Found unknown JSON field %s while expecting type defined at: %s"
        name loc
    in
    failwith msg

  let unknown_field_handler = ref preset_unknown_field_handler
end

module Validation =
struct
  type path_elem = [ `Field of string | `Index of int ]
  type path = path_elem list

  let string_of_path l =
    String.concat "" (
      List.rev_map (
        function
          | `Field s -> "." ^ s
          | `Index n -> "[" ^ string_of_int n ^ "]"
      ) l
    )

  type error = {
    error_path : path;
    error_msg : string option;
  }

  let error ?msg path = {
    error_path = path;
    error_msg = msg;
  }

  let string_of_error x =
    let path = string_of_path x.error_path in
    match x.error_msg with
        None ->
          "Validation error; path = <root>" ^ path
      | Some msg ->
          Printf.sprintf "Validation error: %s; path = <root>%s" msg path
end
