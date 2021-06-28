module Error = Odoc_model.Error
module Location_ = Odoc_model.Location_
module Paths = Odoc_model.Paths

let deprecated_reference_kind warnings location kind replacement =
  Parse_error.deprecated_reference_kind kind replacement location
  |> Error.warning warnings

(* http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html#sec359. *)
let match_ocamldoc_reference_kind (_warnings as w) (_location as loc) s
    : (Odoc_model.Paths_types.Reference.tag_any) option =
  let d = deprecated_reference_kind in
  match s with
  | Some "module" -> Some `TModule
  | Some "modtype" -> d w loc "modtype" "module-type"; Some `TModuleType
  | Some "class" -> Some `TClass
  | Some "classtype" -> d w loc "classtype" "class-type"; Some `TClassType
  | Some "val" -> Some `TValue
  | Some "type" -> Some `TType
  | Some "exception" -> Some `TException
  | Some "attribute" -> None
  | Some "method" -> Some `TMethod
  | Some "section" -> Some `TLabel
  | Some "const" -> d w loc "const" "constructor"; Some `TConstructor
  | Some "recfield" -> d w loc "recfield" "field"; Some `TField
  | _ -> None

let match_extra_odoc_reference_kind (_warnings as w) (_location as loc) s
    : (Odoc_model.Paths_types.Reference.tag_any) option =
  let d = deprecated_reference_kind in
  match s with
  | Some "class-type" -> Some `TClassType
  | Some "constructor" -> Some `TConstructor
  | Some "exn" -> d w loc "exn" "exception"; Some `TException
  | Some "extension" -> Some `TExtension
  | Some "field" -> Some `TField
  | Some "instance-variable" -> Some `TInstanceVariable
  | Some "label" -> d w loc "label" "section"; Some `TLabel
  | Some "module-type" -> Some `TModuleType
  | Some "page" -> Some `TPage
  | Some "value" -> d w loc "value" "val"; Some `TValue
  | _ -> None



(* Ideally, [tokenize] would call this on every reference kind annotation during
   tokenization, when generating the token list. However, that constrains the
   phantom tag type to be the same for all tokens in the list (because lists are
   homogeneous). So, the parser stores kinds as strings in the token list
   instead, and this function is called on each string at the latest possible
   time to prevent typing issues.

   A secondary reason to delay parsing, and store strings in the token list, is
   that we need the strings for user-friendly error reporting. *)
let match_reference_kind warnings location s : Odoc_model.Paths_types.Reference.tag_any =
  match s with
  | None -> `TUnknown
  | Some s as wrapped ->
    let result =
      match match_ocamldoc_reference_kind warnings location wrapped with
      | Some kind -> Some kind
      | None -> match_extra_odoc_reference_kind warnings location wrapped
    in
    match result with
    | Some kind -> kind
    | None ->
      Parse_error.unknown_reference_qualifier s location
      |> Error.raise_exception

(* The string is scanned right-to-left, because we are interested in right-most
   hyphens. The tokens are also returned in right-to-left order, because the
   traversals that consume them prefer to look at the deepest identifier
   first. *)
let tokenize location s =
  let rec scan_identifier started_at open_parenthesis_count index tokens =
    match s.[index] with
    | exception Invalid_argument _ ->
      let identifier, location = identifier_ended started_at index in
      (None, identifier, location)::tokens

    | '-' when open_parenthesis_count = 0 ->
      let identifier, location = identifier_ended started_at index in
      scan_kind identifier location index (index - 1) tokens

    | '.' when open_parenthesis_count = 0 ->
      let identifier, location = identifier_ended started_at index in
      scan_identifier index 0 (index - 1) ((None, identifier, location)::tokens)

    | ')' ->
      scan_identifier
        started_at (open_parenthesis_count + 1) (index - 1) tokens

    | '(' when open_parenthesis_count > 0 ->
      scan_identifier
        started_at (open_parenthesis_count - 1) (index - 1) tokens

    | _ ->
      scan_identifier
        started_at open_parenthesis_count (index - 1) tokens

  and identifier_ended started_at index =
    let offset = index + 1 in
    let length = started_at - offset in
    let identifier = String.trim (String.sub s offset length) in
    let location = Location_.in_string s ~offset ~length location in

    if identifier = "" then begin
      Parse_error.should_not_be_empty ~what:"Identifier in reference" location
      |> Error.raise_exception
    end;

    (identifier, location)

  and scan_kind identifier identifier_location started_at index tokens =
    match s.[index] with
    | exception Invalid_argument _ ->
      let kind, location = kind_ended identifier_location started_at index in
      (kind, identifier, location)::tokens

    | '.' ->
      let kind, location = kind_ended identifier_location started_at index in
      scan_identifier index 0 (index - 1) ((kind, identifier, location)::tokens)

    | _ ->
      scan_kind identifier identifier_location started_at (index - 1) tokens

  and kind_ended identifier_location started_at index =
    let offset = index + 1 in
    let length = started_at - offset in
    let kind = Some (String.sub s offset length) in
    let location = Location_.in_string s ~offset ~length location in
    let location = Location_.span [location; identifier_location] in
    (kind, location)

  in

  scan_identifier (String.length s) 0 (String.length s - 1) []
  |> List.rev

let expected allowed location =
  let unqualified = "or an unqualified reference" in
  let allowed =
    match allowed with
    | [one] ->
      Printf.sprintf "'%s-' %s" one unqualified
    | _ ->
      String.concat
        ", " ((List.map (Printf.sprintf "'%s-'") allowed) @ [unqualified])
  in
  Parse_error.expected allowed location

let parse warnings whole_reference_location s : (Paths.Reference.t, Error.t) Result.result =
  let open Paths.Reference in
  let open Odoc_model.Names in

  let rec signature (kind, identifier, location) tokens : Signature.t =
    let kind = match_reference_kind warnings location kind in
    match tokens with
    | [] ->
      begin match kind with
      | `TUnknown | `TModule | `TModuleType as kind -> `Root (UnitName.of_string identifier, kind)
      | _ ->
        expected ["module"; "module-type"] location |> Error.raise_exception
      end
    | next_token::tokens ->
      begin match kind with
      | `TUnknown ->
        `Dot ((parent next_token tokens :> LabelParent.t), identifier)
      | `TModule -> `Module (signature next_token tokens, ModuleName.of_string identifier)
      | `TModuleType -> `ModuleType (signature next_token tokens, ModuleTypeName.of_string identifier)
      | _ ->
        expected ["module"; "module-type"] location |> Error.raise_exception
      end

  and parent (kind, identifier, location) tokens : Parent.t =
    let kind = match_reference_kind warnings location kind in
    match tokens with
    | [] ->
      begin match kind with
      | `TUnknown | `TModule | `TModuleType | `TType | `TClass
      | `TClassType as kind -> `Root (UnitName.of_string identifier, kind)
      | _ ->
        expected
          ["module"; "module-type"; "type"; "class"; "class-type"] location
        |> Error.raise_exception
      end
    | next_token::tokens ->
      begin match kind with
      | `TUnknown ->
        `Dot ((parent next_token tokens :> LabelParent.t), identifier)
      | `TModule -> `Module (signature next_token tokens, ModuleName.of_string identifier)
      | `TModuleType -> `ModuleType (signature next_token tokens, ModuleTypeName.of_string identifier)
      | `TType -> `Type (signature next_token tokens, TypeName.of_string identifier)
      | `TClass -> `Class (signature next_token tokens, ClassName.of_string identifier)
      | `TClassType -> `ClassType (signature next_token tokens, ClassTypeName.of_string identifier)
      | _ ->
        expected
          ["module"; "module-type"; "type"; "class"; "class-type"] location
        |> Error.raise_exception
      end

  in

  let class_signature (kind, identifier, location) tokens : ClassSignature.t =
    let kind = match_reference_kind warnings location kind in
    match tokens with
    | [] ->
      begin match kind with
      | `TUnknown | `TClass | `TClassType as kind -> `Root (UnitName.of_string identifier, kind)
      | _ -> expected ["class"; "class-type"] location |> Error.raise_exception
      end
    | next_token::tokens ->
      begin match kind with
      | `TUnknown ->
        `Dot ((parent next_token tokens :> LabelParent.t), identifier)
      | `TClass -> `Class (signature next_token tokens, ClassName.of_string identifier)
      | `TClassType -> `ClassType (signature next_token tokens, ClassTypeName.of_string identifier)
      | _ -> expected ["class"; "class-type"] location |> Error.raise_exception
      end
  in

  let datatype (kind, identifier, location) tokens : DataType.t =
    let kind = match_reference_kind warnings location kind in
    match tokens with
    | [] ->
      begin match kind with
      | `TUnknown | `TType as kind -> `Root (UnitName.of_string identifier, kind)
      | _ -> expected ["type"] location |> Error.raise_exception
      end
    | next_token::tokens ->
      begin match kind with
      | `TUnknown ->
        `Dot ((parent next_token tokens :> LabelParent.t), identifier)
      | `TType -> `Type (signature next_token tokens, TypeName.of_string identifier)
      | _ -> expected ["type"] location |> Error.raise_exception
      end
  in

  let rec label_parent (kind, identifier, location) tokens : LabelParent.t =
    let kind = match_reference_kind warnings location kind in
    match tokens with
    | [] ->
      begin match kind with
      | `TUnknown | `TModule | `TModuleType | `TType | `TClass | `TClassType
      | `TPage as kind -> `Root (UnitName.of_string identifier, kind)
      | _ ->
        expected
          ["module"; "module-type"; "type"; "class"; "class-type"; "page"]
          location
        |> Error.raise_exception
      end
    | next_token::tokens ->
      begin match kind with
      | `TUnknown -> `Dot (label_parent next_token tokens, identifier)
      | `TModule -> `Module (signature next_token tokens, ModuleName.of_string identifier)
      | `TModuleType -> `ModuleType (signature next_token tokens, ModuleTypeName.of_string identifier)
      | `TType -> `Type (signature next_token tokens, TypeName.of_string identifier)
      | `TClass -> `Class (signature next_token tokens, ClassName.of_string identifier)
      | `TClassType -> `ClassType (signature next_token tokens, ClassTypeName.of_string identifier)
      | _ ->
        expected
          ["module"; "module-type"; "type"; "class"; "class-type"] location
        |> Error.raise_exception
      end
  in

  let start_from_last_component (kind, identifier, location) old_kind tokens =
    let new_kind = match_reference_kind warnings location kind in
    let kind =
      match old_kind with
      | None -> new_kind
      | Some (old_kind_string, old_kind_location) ->
        let old_kind =
          match_reference_kind
            warnings old_kind_location (Some old_kind_string)
        in
        match new_kind with
        | `TUnknown -> old_kind
        | _ ->
          if old_kind <> new_kind then begin
            let new_kind_string =
              match kind with
              | Some s -> s
              | None -> ""
            in
            Parse_error.reference_kinds_do_not_match
              old_kind_string new_kind_string whole_reference_location
            |> Error.warning warnings
          end;
          new_kind
    in

    match tokens with
    | [] -> `Root (UnitName.of_string identifier, kind)
    | next_token::tokens ->
      match kind with
      | `TUnknown -> `Dot (label_parent next_token tokens, identifier)
      | `TModule -> `Module (signature next_token tokens, ModuleName.of_string identifier)
      | `TModuleType -> `ModuleType (signature next_token tokens, ModuleTypeName.of_string identifier)
      | `TType -> `Type (signature next_token tokens, TypeName.of_string identifier)
      | `TConstructor -> `Constructor (datatype next_token tokens, ConstructorName.of_string identifier)
      | `TField -> `Field (parent next_token tokens, FieldName.of_string identifier)
      | `TExtension -> `Extension (signature next_token tokens, ExtensionName.of_string identifier)
      | `TException -> `Exception (signature next_token tokens, ExceptionName.of_string identifier)
      | `TValue -> `Value (signature next_token tokens, ValueName.of_string identifier)
      | `TClass -> `Class (signature next_token tokens, ClassName.of_string identifier)
      | `TClassType -> `ClassType (signature next_token tokens, ClassTypeName.of_string identifier)
      | `TMethod -> `Method (class_signature next_token tokens, MethodName.of_string identifier)
      | `TInstanceVariable ->
        `InstanceVariable (class_signature next_token tokens, InstanceVariableName.of_string identifier)
      | `TLabel -> `Label (label_parent next_token tokens, LabelName.of_string identifier)
      | `TPage ->
        let suggestion =
          Printf.sprintf "'page-%s' should be first." identifier in
        Parse_error.not_allowed
          ~what:"Page label"
          ~in_what:"the last component of a reference path"
          ~suggestion
          location
        |> Error.raise_exception
  in

  let old_kind, s, location =
    let rec find_old_reference_kind_separator index =
      match s.[index] with
      | ':' ->
        index
      | ')' ->
        begin match String.rindex_from s index '(' with
        | index -> find_old_reference_kind_separator (index - 1)
        | exception (Not_found as exn) -> raise exn
        end
      | _ ->
        find_old_reference_kind_separator (index - 1)
      | exception Invalid_argument _ ->
        raise Not_found
    in
    match find_old_reference_kind_separator (String.length s - 1) with
    | index ->
      let old_kind = String.trim (String.sub s 0 index) in
      let old_kind_location =
        Location_.set_end_as_offset_from_start index whole_reference_location in
      let s = String.sub s (index + 1) (String.length s - (index + 1)) in
      let location =
        Location_.nudge_start (index + 1) whole_reference_location in
      (Some (old_kind, old_kind_location), s, location)
    | exception Not_found ->
      (None, s, whole_reference_location)
  in

  Error.catch begin fun () ->
    match tokenize location s with
    | last_token::tokens -> start_from_last_component last_token old_kind tokens
    | [] ->
      Parse_error.should_not_be_empty
        ~what:"reference target" whole_reference_location
      |> Error.raise_exception
  end

let read_path_longident location s =
  let open Paths.Path in
  let rec loop : string -> int -> Module.t option =
    fun s pos ->
      try
        let idx = String.rindex_from s pos '.' in
        let name = String.sub s (idx + 1) (pos - idx) in
        if String.length name = 0 then None
        else
          match loop s (idx - 1) with
          | None -> None
          | Some parent -> Some (`Dot(parent, name))
      with Not_found ->
        let name = String.sub s 0 (pos + 1) in
        if String.length name = 0 then None
        else Some (`Root name)
  in
  match loop s (String.length s - 1) with
  | Some r -> Result.Ok r
  | None -> Result.Error (Parse_error.expected "a valid path" location)

let read_mod_longident
    warnings location lid : (Paths.Reference.Module.t, Error.t) Result.result
  =
  match parse warnings location lid with
  | Error _ as e -> e
  | Ok p ->
      match p with
      | `Root (_, (`TUnknown | `TModule))
      | `Dot (_, _)
      | `Module (_, _) as r -> Result.Ok r
      | _ ->
          Result.Error (Parse_error.expected "a reference to a module" location)
