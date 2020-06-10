open Odoc_model.Names
type sexp = Sexplib.Sexp.t =
  | Atom of string
  | List of sexp list



module Root_to_sexp =
struct
  module Root = Odoc_model.Root

  let odoc_file : Root.Odoc_file.t -> sexp = function
    | Page p ->
      List [Atom "page"; Atom p]
    | Compilation_unit {name; hidden} ->
      let hidden = if hidden then [Atom "hidden"] else [] in
      List ((Atom "compilation_unit")::(Atom name)::hidden)

  let root : Root.t -> sexp = fun {package; file; digest} ->
    List [Atom package; odoc_file file; Atom (Digest.to_hex digest)]
end



module Identifier_to_sexp =
struct
  module Identifier = Odoc_model.Paths.Identifier

  let identifier : Identifier.t -> sexp =
    let rec traverse : sexp list -> Identifier.t -> sexp =
        fun acc -> function
      | `Root (root, s) ->
        List ((List [Atom "root"; Root_to_sexp.root root; Atom (UnitName.to_string s)])::acc)
      | `Page (root, s) ->
        List ((List [Atom "root"; Root_to_sexp.root root; Atom (PageName.to_string s)])::acc)
      | `Module (parent, s) ->
        traverse ((List [Atom "module"; Atom (ModuleName.to_string s)])::acc) (parent :> Identifier.t)
      | `Argument (parent, i, s) ->
        traverse
          ((List [Atom "argument"; Atom (string_of_int i); Atom (ArgumentName.to_string s)])::acc) (parent :> Identifier.t)
      | `ModuleType (parent, s) ->
        traverse ((List [Atom "module_type"; Atom (ModuleTypeName.to_string s)])::acc) (parent :> Identifier.t)
      | `Type (parent, s) ->
        traverse ((List [Atom "type"; Atom (TypeName.to_string s)])::acc) (parent :> Identifier.t)
      | `CoreType s ->
        List ((List [Atom "core_type"; Atom (TypeName.to_string s)])::acc)
      | `Constructor (parent, s) ->
        traverse ((List [Atom "constructor"; Atom (ConstructorName.to_string s)])::acc) (parent :> Identifier.t)
      | `Field (parent, s) ->
        traverse ((List [Atom "field"; Atom (FieldName.to_string s)])::acc) (parent :> Identifier.t)
      | `Extension (parent, s) ->
        traverse ((List [Atom "extension"; Atom (ExtensionName.to_string s)])::acc) (parent :> Identifier.t)
      | `Exception (parent, s) ->
        traverse ((List [Atom "exception"; Atom (ExceptionName.to_string s)])::acc) (parent :> Identifier.t)
      | `CoreException s ->
        List ((List [Atom "core_exception"; Atom (ExceptionName.to_string s)])::acc)
      | `Value (parent, s) ->
        traverse ((List [Atom "value"; Atom (ValueName.to_string s)])::acc) (parent :> Identifier.t)
      | `Class (parent, s) ->
        traverse ((List [Atom "class"; Atom (ClassName.to_string s)])::acc) (parent :> Identifier.t)
      | `ClassType (parent, s) ->
        traverse ((List [Atom "class_type"; Atom (ClassTypeName.to_string s)])::acc) (parent :> Identifier.t)
      | `Method (parent, s) ->
        traverse ((List [Atom "method"; Atom (MethodName.to_string s)])::acc) (parent :> Identifier.t)
      | `InstanceVariable (parent, s) ->
        traverse ((List [Atom "instance_variable"; Atom (InstanceVariableName.to_string s)])::acc) (parent :> Identifier.t)
      | `Label (parent, s) ->
        traverse ((List [Atom "label"; Atom (LabelName.to_string s)])::acc) (parent :> Identifier.t)
    in
    fun path ->
      traverse [] path
end



module Path_to_sexp =
struct
  module Path = Odoc_model.Paths.Path
  module Resolved = Odoc_model.Paths.Path.Resolved

  let rec path : Path.t -> sexp = function
    | `Resolved parent ->
      List [Atom "resolved"; resolved parent]
    | `Root s ->
      List [Atom "root"; Atom s]
    | `Forward s ->
      List [Atom "forward"; Atom s]
    | `Dot (parent, s) ->
      List [Atom "dot"; Atom s; path (parent :> Path.t)]
    | `Apply (m, m') ->
      List [Atom "apply"; path (m :> Path.t); path (m' :> Path.t)]

  and resolved : Resolved.t -> sexp = function
    |`Identifier i ->
      List [Atom "identifier"; Identifier_to_sexp.identifier i]
    |`Subst (mt, m) ->
      List [Atom "subst"; resolved (mt :> Resolved.t); resolved (m :> Resolved.t)]
    |`SubstAlias (m, m') ->
      List [Atom "subst_alias"; resolved (m :> Resolved.t); resolved (m' :> Resolved.t)]
    |`Hidden m ->
      List [Atom "hidden"; resolved (m :> Resolved.t)]
    |`Module (m, s) ->
      List [Atom "module"; Atom (ModuleName.to_string s); resolved (m :> Resolved.t)]
    |`Canonical (m, p) ->
      List [Atom "canonical"; resolved (m :> Resolved.t); path (p :> Path.t)]
    |`Apply (m, p) ->
      List [Atom "apply"; resolved (m :> Resolved.t); path (p :> Path.t)]
    |`ModuleType (m, s) ->
      List [Atom "module_type"; Atom (ModuleTypeName.to_string s); resolved (m :> Resolved.t)]
    |`Type (m, s) ->
      List [Atom "type"; Atom (TypeName.to_string s); resolved (m :> Resolved.t)]
    |`Class (m, s) ->
      List [Atom "class"; Atom (ClassName.to_string s); resolved (m :> Resolved.t)]
    |`ClassType (m, s) ->
      List [Atom "class_type"; Atom (ClassTypeName.to_string s); resolved (m :> Resolved.t)]
end



module Reference_to_sexp =
struct
  module Reference = Odoc_model.Paths.Reference
  module Resolved = Odoc_model.Paths.Reference.Resolved

  let tag : Odoc_model.Paths_types.Reference.tag_any -> sexp = function
    | `TUnknown -> Atom "unknown"
    | `TModule -> Atom "module"
    | `TModuleType -> Atom "module_type"
    | `TType -> Atom "type"
    | `TConstructor -> Atom "constructor"
    | `TField -> Atom "field"
    | `TExtension -> Atom "extension"
    | `TException -> Atom "exception"
    | `TValue -> Atom "value"
    | `TClass -> Atom "class"
    | `TClassType -> Atom "class_type"
    | `TMethod -> Atom "method"
    | `TInstanceVariable -> Atom "instance_variable"
    | `TLabel -> Atom "label"
    | `TPage -> Atom "page"

  let rec reference : Reference.t -> sexp = function
    | `Resolved parent ->
      List [Atom "resolved"; resolved (parent :> Reference.Resolved.t)]
    | `Root (s, k) ->
      List [Atom "root"; Atom (UnitName.to_string s); tag k]
    | `Dot (parent, s) ->
      List [Atom "dot"; Atom (s); reference (parent :> Reference.t)]
    | `Module (parent, s) ->
      List [Atom "module"; Atom (ModuleName.to_string s); reference (parent :> Reference.t)]
    | `ModuleType (parent, s) ->
      List [Atom "module_type"; Atom (ModuleTypeName.to_string s); reference (parent :> Reference.t)]
    | `Type (parent, s) ->
      List [Atom "type"; Atom (TypeName.to_string s); reference (parent :> Reference.t)]
    | `Constructor (parent, s) ->
      List [Atom "constructor"; Atom (ConstructorName.to_string s); reference (parent :> Reference.t)]
    | `Field (parent, s) ->
      List [Atom "field"; Atom (FieldName.to_string s); reference (parent :> Reference.t)]
    | `Extension (parent, s) ->
      List [Atom "extension"; Atom (ExtensionName.to_string s); reference (parent :> Reference.t)]
    | `Exception (parent, s) ->
      List [Atom "exception"; Atom (ExceptionName.to_string s); reference (parent :> Reference.t)]
    | `Value (parent, s) ->
      List [Atom "value"; Atom (ValueName.to_string s); reference (parent :> Reference.t)]
    | `Class (parent, s) ->
      List [Atom "class"; Atom (ClassName.to_string s); reference (parent :> Reference.t)]
    | `ClassType (parent, s) ->
      List [Atom "class_type"; Atom (ClassTypeName.to_string s); reference (parent :> Reference.t)]
    | `Method (parent, s) ->
      List [Atom "method"; Atom (MethodName.to_string s); reference (parent :> Reference.t)]
    | `InstanceVariable (parent, s) ->
      List [Atom "instance_variable"; Atom (InstanceVariableName.to_string s); reference (parent :> Reference.t)]
    | `Label (parent, s) ->
      List [Atom "label"; Atom (LabelName.to_string s); reference (parent :> Reference.t)]

  and resolved : Resolved.t -> sexp = function
    | `Identifier parent ->
      List [Atom "identifier"; Identifier_to_sexp.identifier parent]
    | `SubstAlias (m, m') ->
      List [Atom "subst_alias"; Path_to_sexp.resolved (m :> Odoc_model.Paths.Path.Resolved.t); resolved (m' :> Resolved.t)]
    | `Module (parent, s) ->
      List [Atom "module"; Atom (ModuleName.to_string s); resolved (parent :> Resolved.t)]
    | `Canonical (m, m') ->
      List [Atom "canonical"; resolved (m :> Resolved.t); reference (m' :> Reference.t)]
    | `ModuleType (parent, s) ->
      List [Atom "module_type"; Atom (ModuleTypeName.to_string s); resolved (parent :> Resolved.t)]
    | `Type (parent, s) ->
      List [Atom "type"; Atom (TypeName.to_string s); resolved (parent :> Resolved.t)]
    | `Constructor (parent, s) ->
      List [Atom "constructor"; Atom (ConstructorName.to_string s); resolved (parent :> Resolved.t)]
    | `Field (parent, s) ->
      List [Atom "field"; Atom (FieldName.to_string s); resolved (parent :> Resolved.t)]
    | `Extension (parent, s) ->
      List [Atom "extension"; Atom (ExtensionName.to_string s); resolved (parent :> Resolved.t)]
    | `Exception (parent, s) ->
      List [Atom "exception"; Atom (ExceptionName.to_string s); resolved (parent :> Resolved.t)]
    | `Value (parent, s) ->
      List [Atom "value"; Atom (ValueName.to_string s); resolved (parent :> Resolved.t)]
    | `Class (parent, s) ->
      List [Atom "class"; Atom (ClassName.to_string s); resolved (parent :> Resolved.t)]
    | `ClassType (parent, s) ->
      List [Atom "class_type"; Atom (ClassTypeName.to_string s); resolved (parent :> Resolved.t)]
    | `Method (parent, s) ->
      List [Atom "method"; Atom (MethodName.to_string s); resolved (parent :> Resolved.t)]
    | `InstanceVariable (parent, s) ->
      List [Atom "instance_variable"; Atom (InstanceVariableName.to_string s); resolved (parent :> Resolved.t)]
    | `Label (parent, s) ->
      List [Atom "label"; Atom (LabelName.to_string s); resolved (parent :> Resolved.t)]
end



module Location_to_sexp =
struct
  module Location_ = Odoc_model.Location_

  let point : Location_.point -> sexp = fun {line; column} ->
    List [Atom (string_of_int line); Atom (string_of_int column)]

  let span : Location_.span -> sexp = fun {file; start; end_} ->
    List [Atom file; point start; point end_]

  let at : ('a -> sexp) -> 'a Location_.with_location -> sexp =
      fun f {location; value} ->
    List [span location; f value]
end



module Comment_to_sexp =
struct
  module Comment = Odoc_model.Comment
  let at = Location_to_sexp.at

  let style : Comment.style -> sexp = function
    | `Bold -> Atom "bold"
    | `Italic -> Atom "italic"
    | `Emphasis -> Atom "emphasis"
    | `Superscript -> Atom "superscript"
    | `Subscript -> Atom "subscript"

  let leaf_inline_element : Comment.leaf_inline_element -> sexp =
    function
    | `Space -> Atom "space"
    | `Word w -> List [Atom "word"; Atom w]
    | `Code_span c -> List [Atom "code_span"; Atom c]
    | `Raw_markup (`Html, s) -> List [Atom "raw_markup"; Atom "html"; Atom s]

  let rec non_link_inline_element : Comment.non_link_inline_element -> sexp =
    function
    | #Comment.leaf_inline_element as e ->
      leaf_inline_element e
    | `Styled (s, es) ->
      List [style s; List (List.map (at non_link_inline_element) es)]

  let rec inline_element : Comment.inline_element -> sexp = function
    | #Comment.leaf_inline_element as e ->
      leaf_inline_element e
    | `Styled (s, es) ->
      List [style s; List (List.map (at inline_element) es)]
    | `Reference (r, es) ->
      List [
        Atom "reference";
        Reference_to_sexp.reference r;
        List (List.map (at non_link_inline_element) es)
      ]
    | `Link (u, es) ->
      List [
        Atom "link";
        Atom u;
        List (List.map (at non_link_inline_element) es)
      ]

  let rec nestable_block_element
      : Comment.nestable_block_element -> sexp =
    function
    | `Paragraph es ->
      List [Atom "paragraph"; List (List.map (at inline_element) es)]
    | `Code_block c -> List [Atom "code_block"; Atom c]
    | `Verbatim t -> List [Atom "verbatim"; Atom t]
    | `Modules ps ->
      List [Atom "modules"; List (List.map Reference_to_sexp.reference (ps :> Odoc_model.Paths.Reference.t list))]
    | `List (kind, items) ->
      let kind =
        match kind with
        | `Unordered -> "unordered"
        | `Ordered -> "ordered"
      in
      let items =
        items
        |> List.map (fun item ->
          List (List.map (at nestable_block_element) item))
        |> fun items -> List items
      in
      List [Atom kind; items]

  let tag : Comment.tag -> sexp = function
    | `Author s ->
      List [Atom "@author"; Atom s]
    | `Deprecated es ->
      List ((Atom "@deprecated")::(List.map (at nestable_block_element) es))
    | `Param (s, es) ->
      List ([Atom "@param"; Atom s] @ (List.map (at nestable_block_element) es))
    | `Raise (s, es) ->
      List ([Atom "@raise"; Atom s] @ (List.map (at nestable_block_element) es))
    | `Return es ->
      List ((Atom "@return")::(List.map (at nestable_block_element) es))
    | `See (kind, s, es) ->
      let kind =
        match kind with
        | `Url -> "url"
        | `File -> "file"
        | `Document -> "document"
      in
      List
        ([Atom "@see"; Atom kind; Atom s] @
          (List.map (at nestable_block_element) es))
    | `Since s -> List [Atom "@since"; Atom s]
    | `Before (s, es) ->
      List ([Atom "@before"; Atom s] @
        (List.map (at nestable_block_element) es))
    | `Version s -> List [Atom "@version"; Atom s]
    | `Canonical (p, r) ->
      List
        [Atom "@canonical"; Path_to_sexp.path (p :> Odoc_model.Paths.Path.t); Reference_to_sexp.reference (r :> Odoc_model.Paths.Reference.t)]
    | `Inline ->
      Atom "@inline"
    | `Open ->
      Atom "@open"
    | `Closed ->
      Atom "@closed"

  let block_element : Comment.block_element -> sexp = function
    | #Comment.nestable_block_element as e -> nestable_block_element e
    | `Heading (level, label, es) ->
      let label = List [Atom "label"; Identifier_to_sexp.identifier (label :> Odoc_model.Paths.Identifier.t)] in
      let level =
        match level with
        | `Title -> "0"
        | `Section -> "1"
        | `Subsection -> "2"
        | `Subsubsection -> "3"
        | `Paragraph -> "4"
        | `Subparagraph -> "5"
      in
      List [Atom level; label; List (List.map (at non_link_inline_element) es)]
    | `Tag t -> tag t

  let comment : Comment.docs -> sexp = fun comment ->
    List (List.map (at block_element) comment)
end



module Error_to_sexp =
struct
  let error : Odoc_model.Error.t -> sexp = fun error ->
    Atom (Odoc_model.Error.to_string error)
end



let parser_output formatter {Odoc_model.Error.value; warnings} =
  let value = Comment_to_sexp.comment value in
  let warnings = List (List.map Error_to_sexp.error warnings) in
  let output =
    List [
      List [Atom "output"; value];
      List [Atom "warnings"; warnings];
    ]
  in
  Sexplib.Sexp.pp_hum formatter output;
  Format.pp_print_newline formatter ();
  Format.pp_print_flush formatter ()
