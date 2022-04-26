open Atd.Import

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
    ~expand ~keep_poly ~xdebug ~inherit_fields ~inherit_variants
    ~strip_all ~strip_sections files =
  let l =
    List.map (
      fun file ->
        fst (
          Atd.Util.load_file ~expand ~keep_poly ~xdebug
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

let print ~html_doc ~out_format ~out_channel:oc ast =
  let f =
    match out_format with
        `Atd -> print_atd ~html_doc
      | `Ocaml name -> print_ml ~name
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
  let out_format = ref `Atd in
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
          expand all `inherit' statements";

    "-if", Arg.Set inherit_fields,
    "
          expand `inherit' statements in records";

    "-iv", Arg.Set inherit_variants,
    "
          expand `inherit' statements in sum types";

    "-ml", Arg.String (fun s -> out_format := `Ocaml s),
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
    let ast =
      parse
          ~expand: !expand
          ~keep_poly: !keep_poly
          ~xdebug: !xdebug
          ~inherit_fields: !inherit_fields
          ~inherit_variants: !inherit_variants
          ~strip_all: !strip_all
          ~strip_sections: !strip_sections
          !input_files
    in
    let out_channel =
      match !output_file with
      | None -> stdout
      | Some file -> open_out file
    in
    print ~html_doc: !html_doc ~out_format: !out_format ~out_channel ast;
    close_out out_channel
  with
      Atd.Ast.Atd_error s ->
        flush stdout;
        eprintf "%s\n%!" s;
        exit 1
    | e -> raise e
