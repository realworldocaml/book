open Import

let with_output fn ~binary ~f =
  match fn with
  | None | Some "-" -> f stdout
  | Some fn -> Out_channel.with_file fn ~binary ~f

module Kind = struct
  type t = Intf | Impl

  let of_filename fn : t option =
    if Caml.Filename.check_suffix fn ".ml" then Some Impl
    else if Caml.Filename.check_suffix fn ".mli" then Some Intf
    else None

  let describe = function Impl -> "implementation" | Intf -> "interface"

  let equal : t -> t -> bool = Poly.equal
end

module Intf_or_impl = struct
  type t = Intf of signature | Impl of structure

  let map t (map : Ast_traverse.map) =
    match t with
    | Impl x -> Impl (map#structure x)
    | Intf x -> Intf (map#signature x)

  let map_with_context t (map : _ Ast_traverse.map_with_context) ctx =
    match t with
    | Impl x -> Impl (map#structure ctx x)
    | Intf x -> Intf (map#signature ctx x)

  let kind : _ -> Kind.t = function Intf _ -> Intf | Impl _ -> Impl
end

module Ast_io = struct
  type input_version = (module OCaml_version)

  let fall_back_input_version = (module Compiler_version : OCaml_version)
  (* This should only be used when the input version can't be determined due to
      loading or preprocessing errors *)

  type t = {
    input_name : string;
    input_version : input_version;
    ast : Intf_or_impl.t;
  }

  type read_error =
    | Not_a_binary_ast
    | Unknown_version of string * input_version
    | Source_parse_error of Location.Error.t * input_version
    | System_error of Location.Error.t * input_version

  type input_source = Stdin | File of string

  type input_kind = Possibly_source of Kind.t * string | Necessarily_binary

  let read_error_to_string (error : read_error) =
    match error with
    | Not_a_binary_ast -> "Error: Not a binary ast"
    | Unknown_version (s, _) -> "Error: Unknown version " ^ s
    | Source_parse_error (loc, _) ->
        "Source parse error:" ^ Location.Error.message loc
    | System_error (loc, _) -> "System error: " ^ Location.Error.message loc

  let parse_source_code ~(kind : Kind.t) ~input_name ~prefix_read_from_source ic
      =
    (* The input version is determined by the fact that the input will get parsed by
       the current compiler Parse module *)
    let input_version = (module Compiler_version : OCaml_version) in
    try
      (* To test if a file is an AST file, we have to read the first few bytes of the
          file. If it is not, we have to parse these bytes and the rest of the file as
          source code.

          The compiler just does [seek_on 0] in this case, however this doesn't work when
          the input is a pipe.

          What we do instead is create a lexing buffer from the input channel and pre-fill
          it with what we read to do the test. *)
      let lexbuf = Lexing.from_channel ic in
      let len = String.length prefix_read_from_source in
      Bytes.blit_string ~src:prefix_read_from_source ~src_pos:0
        ~dst:lexbuf.lex_buffer ~dst_pos:0 ~len;
      lexbuf.lex_buffer_len <- len;
      lexbuf.lex_curr_p <-
        { pos_fname = input_name; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
      Skip_hash_bang.skip_hash_bang lexbuf;
      let ast : Intf_or_impl.t =
        match kind with
        | Intf -> Intf (Parse.interface lexbuf)
        | Impl -> Impl (Parse.implementation lexbuf)
      in
      Ok { input_name; input_version; ast }
    with exn -> (
      match Location.Error.of_exn exn with
      | None -> raise exn
      | Some error -> Error (Source_parse_error (error, input_version)))

  let magic_length = String.length Astlib.Config.ast_impl_magic_number

  let read_magic ic =
    let buf = Bytes.create magic_length in
    let len = input ic buf 0 magic_length in
    let s = Bytes.sub_string buf ~pos:0 ~len in
    if len = magic_length then Ok s else Error s

  let from_channel ch ~input_kind =
    let handle_non_binary prefix_read_from_source =
      match input_kind with
      | Possibly_source (kind, input_name) ->
          parse_source_code ~kind ~input_name ~prefix_read_from_source ch
      | Necessarily_binary -> Error Not_a_binary_ast
    in
    match read_magic ch with
    | Error s -> handle_non_binary s
    | Ok s -> (
        match Find_version.from_magic s with
        | Intf (module Input_version : OCaml_version) ->
            let input_name : string = input_value ch in
            let ast = input_value ch in
            let module Input_to_ppxlib = Convert (Input_version) (Js) in
            let ast = Intf_or_impl.Intf (Input_to_ppxlib.copy_signature ast) in
            Ok
              {
                input_name;
                input_version = (module Input_version : OCaml_version);
                ast;
              }
        | Impl (module Input_version : OCaml_version) ->
            let input_name : string = input_value ch in
            let ast = input_value ch in
            let module Input_to_ppxlib = Convert (Input_version) (Js) in
            let ast = Intf_or_impl.Impl (Input_to_ppxlib.copy_structure ast) in
            Ok
              {
                input_name;
                input_version = (module Input_version : OCaml_version);
                ast;
              }
        | Unknown ->
            if
              String.equal
                (String.sub s ~pos:0 ~len:9)
                (String.sub Astlib.Config.ast_impl_magic_number ~pos:0 ~len:9)
              || String.equal
                   (String.sub s ~pos:0 ~len:9)
                   (String.sub Astlib.Config.ast_intf_magic_number ~pos:0 ~len:9)
            then Error (Unknown_version (s, fall_back_input_version))
            else handle_non_binary s)

  let read input_source ~input_kind =
    try
      match input_source with
      | Stdin -> from_channel stdin ~input_kind
      | File fn -> In_channel.with_file fn ~f:(from_channel ~input_kind)
    with exn -> (
      match Location.Error.of_exn exn with
      | None -> raise exn
      | Some error -> Error (System_error (error, fall_back_input_version)))

  let write oc { input_name; input_version = (module Input_version); ast }
      ~add_ppx_context =
    let module Ppxlib_to_input = Convert (Js) (Input_version) in
    let module Ocaml_to_input = Convert (Compiler_version) (Input_version) in
    match ast with
    | Intf sg ->
        let sg =
          if add_ppx_context then
            Selected_ast.To_ocaml.copy_signature sg
            |> Astlib.Ast_metadata.add_ppx_context_sig ~tool_name:"ppx_driver"
            |> Ocaml_to_input.copy_signature
          else Ppxlib_to_input.copy_signature sg
        in
        output_string oc Input_version.Ast.Config.ast_intf_magic_number;
        output_value oc input_name;
        output_value oc sg
    | Impl st ->
        let st =
          if add_ppx_context then
            Selected_ast.To_ocaml.copy_structure st
            |> Astlib.Ast_metadata.add_ppx_context_str ~tool_name:"ppx_driver"
            |> Ocaml_to_input.copy_structure
          else Ppxlib_to_input.copy_structure st
        in
        output_string oc Input_version.Ast.Config.ast_impl_magic_number;
        output_value oc input_name;
        output_value oc st

  module Read_bin = struct
    type ast = Intf of signature | Impl of structure

    type t = { ast : ast; input_name : string }

    let read_binary fn =
      match
        In_channel.with_file fn ~f:(from_channel ~input_kind:Necessarily_binary)
      with
      | Ok { ast; input_name; _ } ->
          let ast =
            match ast with
            | Impl structure -> Impl structure
            | Intf signature -> Intf signature
          in
          Ok { ast; input_name }
      | Error e -> Error (read_error_to_string e)

    let get_ast t = t.ast

    let get_input_name t = t.input_name
  end
end

module System = struct
  let run_preprocessor ~pp ~input ~output =
    let command =
      Printf.sprintf "%s %s > %s" pp
        (if String.equal input "-" then "" else Caml.Filename.quote input)
        (Caml.Filename.quote output)
    in
    if Caml.Sys.command command = 0 then Ok ()
    else Error (command, Ast_io.fall_back_input_version)
end
