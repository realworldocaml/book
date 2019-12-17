open StdLabels

let split_on_char ~sep s =
  let open String in
  let r = ref [] in
  let j = ref (length s) in
  for i = length s - 1 downto 0 do
    if unsafe_get s i = sep then begin
      r := sub s ~pos:(i + 1) ~len:(!j - i - 1) :: !r;
      j := i
    end
  done;
  sub s ~pos:0 ~len:!j :: !r

let linked_in = ["findlib.dynload"; "dynlink"; "ocaml-migrate-parsetree"; "compiler-libs.common"; "str"]

module List = struct
  include List
  let concat_map l ~f = List.concat (List.map l ~f)
end

let init_findlib preds =
  Findlib.init ();
  (* Record linked in packages *)
  List.iter (Findlib.package_deep_ancestors [] linked_in)
    ~f:(Findlib.record_package Record_core);
  Findlib.record_package_predicates
    ((if Dynlink.is_native then "native" else "byte") :: preds)

let package_property preds pkg var =
  match Findlib.package_property preds pkg var with
  | x -> Some x
  | exception Not_found -> None

let split_words = Str.split (Str.regexp "[ \t\n\r]+")

let setup_external_ppxs pkgs =
  let preds = [] in
  let pkgs =
    List.map
      (Findlib.package_deep_ancestors preds pkgs)
      ~f:(fun pkg ->
        (pkg,
         Findlib.package_directory pkg,
         package_property preds pkg "ppx",
         package_property preds pkg "ppxopt"))
  in
  let ppxopts =
    List.concat_map pkgs ~f:(fun (_pkg, dir, _ppx, ppxopt) ->
      match ppxopt with
      | None -> []
      | Some ppxopt ->
        List.concat_map (split_words ppxopt) ~f:(fun opt ->
          match split_on_char ~sep:',' opt with
          | pkg' :: (_ :: _ as opts) ->
            List.map opts ~f:(fun opt ->
              (pkg', Findlib.resolve_path opt ~base:dir ~explicit:true))
          | _ ->
            []))
  in
  let ppxs =
    List.concat_map pkgs ~f:(fun (pkg, dir, ppx, _ppxopt) ->
      match ppx with
      | None -> []
      | Some ppx ->
        let ppx = Findlib.resolve_path ~base:dir ~explicit:true ppx in
        let opts = List.filter ppxopts ~f:(fun (p, _) -> p = pkg) |> List.map ~f:snd in
        [ String.concat ~sep:" " (ppx :: opts) ])
  in

  Clflags.all_ppx := ppxs;

  let tool = "ppxfind" in
  Migrate_parsetree.Driver.register ~name:tool
    (module Migrate_parsetree.OCaml_current)
    (fun _ _ ->
       { Ast_mapper.default_mapper with
         structure = (fun _ x -> Pparse.apply_rewriters_str ~tool_name:tool x)
       ; signature = (fun _ x -> Pparse.apply_rewriters_sig ~tool_name:tool x)
       })

let main () =
  let legacy = ref false in
  let debug = ref false in
  let args =
    Arg.align
      [ "-legacy", Set legacy, " Use the legacy ppx system"
      ; "-debug", Set debug, " Enable debug messages"
      ]
  in
  let usage = "ppxfind [options] ppx1,ppx2,... [ppx-options] [file]" in
  let anon pkgs =
    let pkgs = split_on_char ~sep:',' pkgs in

    if not !legacy then begin
      init_findlib ["ppx_driver"];
      Dynlink.allow_unsafe_modules true;
      Fl_dynload.load_packages ~debug:!debug pkgs
    end else begin
      init_findlib [];
      Clflags.verbose := !debug;
      setup_external_ppxs pkgs
    end;

    (* Chain to omp *)
    Sys.argv.(!Arg.current) <- Sys.argv.(0);
    Migrate_parsetree.Driver.run_main ()
  in
  Arg.parse args anon usage;
  prerr_endline "ppxfind: no packages given";
  exit 2

let () =
  try
    main ()
  with
  | Dynlink.Error error ->
    Printf.eprintf "ppxfoo: %s\n%!" (Dynlink.error_message error);
    exit 1
