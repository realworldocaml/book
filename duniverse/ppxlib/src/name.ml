open! Import
module Format = Caml.Format

let fold_dot_suffixes name ~init:acc ~f =
  let rec collapse_after_at = function
    | [] -> []
    | part :: parts ->
        if (not (String.is_empty part)) && Char.equal part.[0] '@' then
          [ String.concat (String.drop_prefix part 1 :: parts) ~sep:"." ]
        else part :: collapse_after_at parts
  in
  let rec loop acc parts =
    match parts with
    | [] -> acc
    | part :: parts ->
        loop (f (String.concat (part :: parts) ~sep:".") acc) parts
  in
  String.split_on_char name ~sep:'.' |> collapse_after_at |> loop acc

let dot_suffixes name =
  fold_dot_suffixes name ~init:[] ~f:(fun x acc -> x :: acc)

let split_path =
  let rec loop s i =
    if i = String.length s then (s, None)
    else match s.[i] with '.' -> after_dot s (i + 1) | _ -> loop s (i + 1)
  and after_dot s i =
    if i = String.length s then (s, None)
    else
      match s.[i] with
      | 'A' .. 'Z' -> (String.prefix s (i - 1), Some (String.drop_prefix s i))
      | '.' -> after_dot s (i + 1)
      | _ -> loop s (i + 1)
  in
  fun s -> loop s 0

module Pattern = struct
  type t = { name : string; dot_suffixes : String.Set.t }

  let make name =
    { name; dot_suffixes = String.Set.of_list (dot_suffixes name) }

  let name t = t.name
  let matches t matched = String.Set.mem matched t.dot_suffixes
end

let get_outer_namespace name =
  match String.index_opt name '.' with
  | None -> None
  | Some i -> Some (String.sub name ~pos:0 ~len:i)

module Allowlisted = struct
  (* Allow list the following attributes, as well as all their dot suffixes.

     Since these attributes are interpreted by the compiler itself, we cannot check
     at the level of a ppx rewriter that they have been properly interpreted, so
     we just accept them anywhere.

     Sadly, the compiler silently ignores them if they are misplaced...
  *)
  let create_set fully_qualified_names =
    List.fold_left
      ~f:(fun acc name ->
        fold_dot_suffixes name ~init:acc ~f:(fun x acc -> String.Set.add x acc))
      ~init:String.Set.empty fully_qualified_names

  let attributes =
    create_set
      [
        "ocaml.alert";
        "ocaml.boxed";
        "ocaml.deprecated";
        "ocaml.deprecated_mutable";
        "ocaml.doc";
        "ocaml.extension_constructor";
        "ocaml.immediate";
        "ocaml.immediate64";
        "ocaml.inline";
        "ocaml.inlined";
        "ocaml.local";
        "ocaml.noalloc";
        "ocaml.ppwarning";
        "ocaml.remove_aliases";
        "ocaml.specialise";
        "ocaml.specialised";
        "ocaml.tailcall";
        "ocaml.text";
        "ocaml.unboxed";
        "ocaml.unroll";
        "ocaml.unrolled";
        "ocaml.untagged";
        "ocaml.warn_on_literal_pattern";
        "ocaml.warnerror";
        "ocaml.warning";
        "ocaml.toplevel_printer" (*Interpreted by the toplevel/utop*);
        "toplevel_printer" (*Interpreted by the toplevel/utop*);
      ]

  (* Allow list the following extensions.

     Since these extensions are interpreted by the compiler itself, we cannot check
     at the level of a ppx rewriter that they have been properly interpreted, so
     we just accept them anywhere.
  *)
  let extensions = create_set [ "ocaml.error"; "ocaml.extension_constructor" ]

  let is_allowlisted ~kind name =
    match kind with
    | `Attribute -> String.Set.mem name attributes
    | `Extension -> String.Set.mem name extensions

  let get_attribute_list () = String.Set.elements attributes
  let get_extension_list () = String.Set.elements extensions
end

module Reserved_namespaces = struct
  let tbl : (string, unit) Hashtbl.t = Hashtbl.create 16
  let reserve ns = Hashtbl.add_exn tbl ~key:ns ~data:()
  let () = reserve "merlin"
  let () = reserve "reason"
  let () = reserve "refmt"
  let () = reserve "metaocaml"
  let () = reserve "ocamlformat"

  let is_in_reserved_namespaces name =
    match get_outer_namespace name with
    | Some ns -> Hashtbl.mem tbl ns
    | None -> Hashtbl.mem tbl name

  let check_not_reserved ~kind name =
    let kind, list =
      match kind with
      | `Attribute -> ("attribute", Allowlisted.attributes)
      | `Extension -> ("extension", Allowlisted.extensions)
    in
    if String.Set.mem name list then
      Printf.ksprintf failwith
        "Cannot register %s with name '%s' as it matches an %s reserved by the \
         compiler"
        kind name kind
    else if is_in_reserved_namespaces name then
      Printf.ksprintf failwith
        "Cannot register %s with name '%s' as its namespace is marked as \
         reserved"
        kind name
end

let ignore_checks name =
  Reserved_namespaces.is_in_reserved_namespaces name
  || String.is_prefix name ~prefix:"_"

module Registrar = struct
  type element = { fully_qualified_name : string; declared_at : Caller_id.t }
  type all_for_context = { mutable all : element String.Map.t }

  type 'a t = {
    all_by_context : ('a, all_for_context) Hashtbl.t;
    skip : string list;
    kind : string;
    string_of_context : 'a -> string option;
  }

  let create ~kind ~current_file ~string_of_context =
    {
      all_by_context = Hashtbl.create 16;
      skip = [ current_file; __FILE__ ];
      kind;
      string_of_context;
    }

  let get_all_for_context t context =
    Hashtbl.find_or_add t.all_by_context context ~default:(fun () ->
        { all = String.Map.empty })

  let check_collisions_local ~caller ~all_for_context t context name =
    match String.Map.find_opt name all_for_context.all with
    | None -> ()
    | Some e ->
        let declared_at = function
          | None -> ""
          | Some (loc : Caml.Printexc.location) ->
              Printf.sprintf " declared at %s:%d" loc.filename loc.line_number
        in
        let context =
          match t.string_of_context context with
          | None -> ""
          | Some s -> " on " ^ s ^ "s"
        in
        Printf.ksprintf failwith
          "Some ppx-es tried to register conflicting transformations: %s \
           '%s'%s%s matches %s '%s'%s"
          (String.capitalize_ascii t.kind)
          name context (declared_at caller) t.kind e.fully_qualified_name
          (declared_at e.declared_at)

  let check_collisions t context name =
    let caller = Caller_id.get ~skip:t.skip in
    let all_for_context = get_all_for_context t context in
    check_collisions_local ~caller ~all_for_context t context name

  let register ~kind t context name =
    Reserved_namespaces.check_not_reserved ~kind name;
    let caller = Caller_id.get ~skip:t.skip in
    let all = get_all_for_context t context in
    check_collisions_local ~caller ~all_for_context:all t context name;
    let t = { fully_qualified_name = name; declared_at = caller } in
    all.all <-
      fold_dot_suffixes name ~init:all.all ~f:(fun name acc ->
          String.Map.add name t acc)

  let spellcheck t context ?(allowlist = []) name =
    let all =
      let all = get_all_for_context t context in
      String.Map.fold (fun key _ acc -> key :: acc) all.all []
    in
    match Spellcheck.spellcheck (all @ allowlist) name with
    | Some _ as x -> x
    | None -> (
        let other_contexts =
          Hashtbl.fold
            (fun ctx all_from_context acc ->
              if
                Poly.( <> ) context ctx
                && String.Map.mem name all_from_context.all
              then
                match t.string_of_context ctx with
                | None -> acc
                | Some s -> (s ^ "s") :: acc
              else acc)
            t.all_by_context []
        in
        let pp_text = Format.pp_print_text in
        let current_context ppf =
          match t.string_of_context context with
          | None | Some "" -> ()
          | Some s ->
              let a_or_an =
                match s.[0] with
                | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' -> "an"
                | _ -> "a"
              in
              Format.fprintf ppf
                "@ but@ is@ used@ here@ in@ the@ context@ of@ %s@ %a" a_or_an
                pp_text s
        in
        match
          List.sort ~cmp:(fun x y -> -String.compare x y) other_contexts
        with
        | [] -> None
        | [ c ] ->
            Some
              (Format.asprintf
                 "@[Hint:@ `%s'@ is@ available@ for@ %a%t.@]@\n\
                  Did you put it at the wrong level?" name pp_text c
                 current_context)
        | last :: rev_others ->
            let others = List.rev rev_others in
            Some
              (Format.asprintf
                 "@[Hint:@ `%s'@ is@ available@ for@ %a@ and@ %a%t.@]@\n\
                  Did you put it at the wrong level?" name
                 (Format.pp_print_list pp_text ~pp_sep:(fun ppf () ->
                      Format.fprintf ppf ",@ "))
                 others pp_text last current_context))

  module Error = struct
    (* TODO: hint spelling errors regarding reserved namespaces names and allowlisted
       names instead of taking an optional [allowlist] parameter. *)
    let createf t context ?allowlist fmt (name : string Loc.t) =
      Printf.ksprintf
        (fun msg ->
          match spellcheck t context name.txt ?allowlist with
          | None -> Location.Error.createf ~loc:name.loc "%s" msg
          | Some s -> Location.Error.createf ~loc:name.loc "%s.\n%s" msg s)
        fmt name.txt

    let raise_errorf t context ?allowlist fmt (name : string Loc.t) =
      Location.Error.raise @@ createf t context ?allowlist fmt name

    let error_extensionf t context ?allowlist fmt (name : string Loc.t) =
      Location.Error.to_extension @@ createf t context ?allowlist fmt name
  end

  let raise_errorf = Error.raise_errorf
end
