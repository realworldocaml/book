open Atd.Import

type out_format =
  | Atd
  | Ocaml of string (* output file name [why?] *)
  | Jsonschema of string (* root type *)

(* These string identifiers aren't standard, unlike the full URLs
   such as "https://json-schema.org/draft/2020-12" that
   are too cumbersome to use on the command line. *)
let available_jsonschema_versions = [
  "draft-2019-09", Atd.Jsonschema.Draft_2019_09;
  "draft-2020-12", Atd.Jsonschema.Draft_2020_12;
]

let string_of_jsonschema_version x =
  match List.find_opt (fun (s, x0) -> x = x0) available_jsonschema_versions
  with
  | Some (s, _) -> s
  | None -> assert false

let jsonschema_version_of_string s =
  match List.assoc_opt s available_jsonschema_versions with
  | Some x -> x
  | None -> failwith (sprintf "Invalid JSON Schema version identifier: %S" s)

let html_of_doc loc s =
  let doc = Atd.Doc.parse_text loc s in
  Atd.Doc.html_of_doc doc

let format_html_comments ((section, (_, l)) as x) =
  let comment s =
    let comment = "(*html " ^ s ^ "*)" in
    Easy_format.Atom (comment, Easy_format.atom)
  in
  match section with
  | "doc" ->
      begin match List.assoc "html" l with
        | Some (_, Some s) -> comment s
        | Some _ | None ->
            begin match List.assoc "text" l with
              | Some (loc, Some s) -> comment (html_of_doc loc s)
              | Some _ | None -> Atd.Print.default_annot x
            end
      end
  | _ -> Atd.Print.default_annot x

let print_atd ~html_doc oc ast =
  let annot =
    if html_doc then Some format_html_comments
    else None
  in
  let pp = Atd.Print.format ?annot ast in
  Easy_format.Pretty.to_channel oc pp;
  output_string oc "\n"

let print_ml ~name oc ast =
  let buf = Buffer.create 1000 in
  Atd.Reflect.print_full_module_def buf name ast;
  output_string oc (Buffer.contents buf);
  output_string oc "\n"

let strip all sections x =
  let filter =
    if all then
      fun _ -> []
    else
      List.filter (fun (name, _) -> not (List.mem name sections))
  in
  Atd.Ast.map_all_annot filter x

let parse
    ~annot_schema
    ~expand ~keep_poly ~xdebug ~inherit_fields ~inherit_variants
    ~strip_all ~strip_sections files =
  let l =
    List.map (
      fun file ->
        fst (
          Atd.Util.load_file ~annot_schema ~expand ~keep_poly ~xdebug
            ~inherit_fields ~inherit_variants file
        )
    ) files
  in
  let heads, bodies = List.split l in
  let first_head =
    (* heads in other files than the first one are tolerated but ignored *)
    match heads with
        x :: _ -> x
      | [] -> (Atd.Ast.dummy_loc, [])
  in
  let m = first_head, List.flatten bodies in
  strip strip_all strip_sections m

let print
    ~xprop ~jsonschema_version
    ~src_name ~html_doc ~out_format ~out_channel:oc ast =
  let f =
    match out_format with
    | Atd -> print_atd ~html_doc
    | Ocaml name -> print_ml ~name
    | Jsonschema root_type ->
        Atd.Jsonschema.print
          ~xprop
          ~version:jsonschema_version
          ~src_name ~root_type
  in
  f oc ast

let split_on_comma =
  Re.Str.split_delim (Re.Str.regexp ",")

let () =
  let expand = ref false in
  let keep_poly = ref false in
  let xdebug = ref false in
  let inherit_fields = ref false in
  let inherit_variants = ref false in
  let strip_sections = ref [] in
  let strip_all = ref false in
  let out_format = ref Atd in
  let jsonschema_version = ref Atd.Jsonschema.default_version in
  let jsonschema_allow_additional_properties = ref true in
  let html_doc = ref false in
  let input_files = ref [] in
  let output_file = ref None in

  let options = [
    "-o", Arg.String (fun file -> output_file := Some file),
    "<path>
          write to this file instead of stdout";

    "-x", Arg.Set expand,
    "
          make type expressions monomorphic";

    "-xk", Arg.Unit (fun () -> expand := true; keep_poly := true),
    "
          keep parametrized type definitions and imply -x.
          Default is to return only monomorphic type definitions";

    "-xd", Arg.Unit (fun () -> expand := true; xdebug := true),
    "
          debug mode implying -x";

    "-i", Arg.Unit (fun () ->
                      inherit_fields := true;
                      inherit_variants := true),
    "
          expand all 'inherit' statements";

    "-if", Arg.Set inherit_fields,
    "
          expand 'inherit' statements in records";

    "-iv", Arg.Set inherit_variants,
    "
          expand 'inherit' statements in sum types";

    "-jsonschema", Arg.String (fun s -> out_format := Jsonschema s),
    "<root type name>
          translate the ATD file to JSON Schema.";

    "-jsonschema-no-additional-properties",
    Arg.Unit (fun () -> jsonschema_allow_additional_properties := false),
    "
          emit a JSON Schema that doesn't tolerate extra fields on JSON
          objects.";

    "-jsonschema-version",
    Arg.String (fun s ->
      jsonschema_version := jsonschema_version_of_string s
    ),
    sprintf "{ %s }
          specify which version of the JSON Schema standard to target.
          Default: latest supported version, which is currently '%s'."
      (available_jsonschema_versions |> List.map fst |> String.concat " | ")
      (string_of_jsonschema_version Atd.Jsonschema.default_version);

    "-ml", Arg.String (fun s -> out_format := Ocaml s),
    "<name>
          output the ocaml code of the ATD abstract syntax tree";

    "-html-doc", Arg.Set html_doc,
    "
          replace directly <doc html=\"...\"> by (*html ... *)
          or replace <doc text=\"...\"> by (*html ... *)
          where the contents are formatted as HTML
          using <p>, <code> and <pre>.
          This is suitable input for \"caml2html -ext html:cat\"
          which converts ATD files into HTML.";

    "-strip",
    Arg.String (fun s -> strip_sections := split_on_comma s @ !strip_sections),
    "NAME1[,NAME2,...]
          remove all annotations of the form <NAME1 ...>,
          <NAME2 ...>, etc.";

    "-strip-all", Arg.Set strip_all,
    "
          remove all annotations";

    "-version",
    Arg.Unit (fun () ->
                print_endline Atd.Version.version;
                exit 0),
    "
          print the version of atd and exit";
  ]
  in
  let msg = sprintf "Usage: %s FILE" Sys.argv.(0) in
  Arg.parse options (fun file -> input_files := file :: !input_files) msg;
  try
    let force_inherit, annot_schema =
      match !out_format with
      | Jsonschema _ -> true, Atd.Jsonschema.annot_schema
      | _ -> false, []
    in
    let inherit_fields = !inherit_fields || force_inherit in
    let inherit_variants = !inherit_variants || force_inherit in
    let ast =
      parse
        ~annot_schema
        ~expand: !expand
        ~keep_poly: !keep_poly
        ~xdebug: !xdebug
        ~inherit_fields
        ~inherit_variants
        ~strip_all: !strip_all
        ~strip_sections: !strip_sections
        !input_files
    in
    let out_channel =
      match !output_file with
      | None -> stdout
      | Some file -> open_out file
    in
    let src_name =
      match !input_files with
      | [] -> "<empty>"
      | [file] -> sprintf "'%s'" (Filename.basename file)
      | _ -> "multiple files"
    in
    print
      ~jsonschema_version: !jsonschema_version
      ~xprop: !jsonschema_allow_additional_properties
      ~src_name
      ~html_doc: !html_doc
      ~out_format: !out_format
      ~out_channel ast;
    close_out out_channel
  with
  | Atd.Ast.Atd_error msg
  | Failure msg ->
      flush stdout;
      eprintf "%s\n%!" msg;
      exit 1
  | e -> raise e
