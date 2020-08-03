open Odoc_compat

module Location = Odoc_model.Location_
module Error = Odoc_model.Error
module Comment = Odoc_model.Comment

type 'a with_location = 'a Location.with_location



type ast_leaf_inline_element = [
  | `Space of string
  | `Word of string
  | `Code_span of string
  | `Raw_markup of string option * string
]



type status = {
  warnings : Error.warning_accumulator;
  sections_allowed : Ast.sections_allowed;
  parent_of_sections : Odoc_model.Paths.Identifier.LabelParent.t;
}



(* TODO This and Token.describe probably belong in Parse_error. *)
let describe_element = function
  | `Reference (`Simple, _, _) ->
    Token.describe (`Simple_reference "")
  | `Reference (`With_text, _, _) ->
    Token.describe (`Begin_reference_with_replacement_text "")
  | `Link _ ->
    Token.describe (`Begin_link_with_replacement_text "")
  | `Heading (level, _, _) ->
    Token.describe (`Begin_section_heading (level, None))



let leaf_inline_element
    : status -> ast_leaf_inline_element with_location ->
        Comment.leaf_inline_element with_location =
    fun status element ->

  match element with
  | { value = (`Word _ | `Code_span _); _ } as element ->
    element

  | { value = `Space _; _ } ->
    Location.same element `Space

  | { value = `Raw_markup (Some "html", s); _ } ->
    Location.same element (`Raw_markup (`Html, s))

  | { value = `Raw_markup (target, s); location } ->
    let error =
      match target with
      | Some invalid_target ->
        Parse_error.invalid_raw_markup_target invalid_target location
      | None ->
        Parse_error.default_raw_markup_target_not_supported location
    in
    Error.warning status.warnings error;
    Location.same element (`Code_span s)



let rec non_link_inline_element
    : status -> surrounding:_ -> Ast.inline_element with_location ->
        Comment.non_link_inline_element with_location =
    fun status ~surrounding element ->

  match element with
  | {value = #ast_leaf_inline_element; _} as element ->
    (leaf_inline_element status element
     :> Comment.non_link_inline_element with_location)

  | {value = `Styled (style, content); _} ->
    `Styled (style, non_link_inline_elements status ~surrounding content)
    |> Location.same element

  | {value = `Reference (_, _, content); _}
  | {value = `Link (_, content); _} as element ->
    Parse_error.not_allowed
      ~what:(describe_element element.value)
      ~in_what:(describe_element surrounding)
      element.location
    |> Error.warning status.warnings;

    `Styled (`Emphasis, non_link_inline_elements status ~surrounding content)
    |> Location.same element

and non_link_inline_elements status ~surrounding elements =
  List.map (non_link_inline_element status ~surrounding) elements



let rec inline_element
    : status -> Ast.inline_element with_location ->
        Comment.inline_element with_location =
    fun status element ->

  match element with
  | {value = #ast_leaf_inline_element; _} as element ->
    (leaf_inline_element status element
     :> Comment.inline_element with_location)

  | {value = `Styled (style, content); location} ->
    `Styled (style, inline_elements status content)
    |> Location.at location

  | {value = `Reference (kind, target, content) as value; location} ->
    let {Location.value = target; location = target_location} = target in
    begin match Reference.parse status.warnings target_location target with
      | Result.Ok target ->
        let content = non_link_inline_elements status ~surrounding:value content in
        Location.at location (`Reference (target, content))

      | Result.Error error ->
        Error.warning status.warnings error;
        let placeholder =
          match kind with
          | `Simple -> `Code_span target
          | `With_text -> `Styled (`Emphasis, content)
        in
        inline_element status (Location.at location placeholder)
    end

  | {value = `Link (target, content) as value; location} ->
    `Link (target, non_link_inline_elements status ~surrounding:value content)
    |> Location.at location

and inline_elements status elements =
  List.map (inline_element status) elements



let rec nestable_block_element
    : status -> Ast.nestable_block_element with_location ->
        Comment.nestable_block_element with_location =
    fun status element ->

  match element with
  | {value = `Paragraph content; location} ->
    Location.at location (`Paragraph (inline_elements status content))

  | {value = `Code_block _; _}
  | {value = `Verbatim _; _} as element ->
    element

  | {value = `Modules modules; location} ->
    let modules =
      List.fold_left (fun acc {Location.value; location} ->
          match Reference.read_mod_longident status.warnings location value with
          | Result.Ok r ->
            r :: acc
          | Result.Error error ->
            Error.warning status.warnings error;
            acc
        ) [] modules
      |> List.rev
    in
    Location.at location (`Modules modules)

  | {value = `List (kind, _syntax, items); location} ->
    `List (kind, List.map (nestable_block_elements status) items)
    |> Location.at location

and nestable_block_elements status elements =
  List.map (nestable_block_element status) elements



let tag
    : location:Location.span -> status -> Ast.tag ->
        (Comment.block_element with_location, Ast.block_element with_location) Result.result =
    fun ~location status tag ->
  let ok t = Result.Ok (Location.at location (`Tag t)) in
  match tag with
  | `Author _
  | `Since _
  | `Version _
  | `Inline
  | `Open
  | `Closed as tag ->
    ok tag

  | `Canonical {value = s; location = r_location} ->
    let path = Reference.read_path_longident r_location s in
    let module_ = Reference.read_mod_longident status.warnings r_location s in
    begin match path, module_ with
    | Result.Ok path, Result.Ok module_ ->
      ok (`Canonical (path, module_))
    | Result.Error e, _
    | Result.Ok _, Result.Error e ->
      Error.warning status.warnings e;
      let placeholder = [`Word "@canonical"; `Space " "; `Code_span s] in
      let placeholder = List.map (Location.at location) placeholder in
      Error (Location.at location (`Paragraph placeholder))
    end

  | `Deprecated content ->
    ok (`Deprecated (nestable_block_elements status content))

  | `Param (name, content) ->
    ok (`Param (name, nestable_block_elements status content))

  | `Raise (name, content) ->
    ok (`Raise (name, nestable_block_elements status content))

  | `Return content ->
    ok (`Return (nestable_block_elements status content))

  | `See (kind, target, content) ->
    ok (`See (kind, target, nestable_block_elements status content))

  | `Before (version, content) ->
    ok (`Before (version, nestable_block_elements status content))



(* When the user does not give a section heading a label (anchor), we generate
   one from the text in the heading. This is the common case. This involves
   simply scanning the AST for words, lowercasing them, and joining them with
   hyphens.

   This must be done in the parser (i.e. early, not at HTML/other output
   generation time), so that the cross-referencer can see these anchors. *)
let generate_heading_label : Comment.link_content -> string = fun content ->

  (* Code spans can contain spaces, so we need to replace them with hyphens. We
     also lowercase all the letters, for consistency with the rest of this
     procedure. *)
  let replace_spaces_with_hyphens_and_lowercase s =
    let result = Bytes.create (String.length s) in
    s |> String.iteri begin fun index c ->
      let c =
        match c with
        | ' ' | '\t' | '\r' | '\n' -> '-'
        | _ -> Char.lowercase_ascii c
      in
      Bytes.set result index c
    end;
    Bytes.unsafe_to_string result
  in

  (* Perhaps this should be done using a [Buffer.t]; we can switch to that as
     needed. *)
  let rec scan_inline_elements anchor = function
    | [] ->
      anchor
    | element::more ->
      let anchor =
        match element.Location.value with
        | `Space ->
          anchor ^ "-"
        | `Word w ->
          anchor ^ (String.lowercase_ascii w)
        | `Code_span c ->
          anchor ^ (replace_spaces_with_hyphens_and_lowercase c)
        | `Raw_markup _ ->
          (* TODO Perhaps having raw markup in a section heading should be an
             error? *)
          anchor
        | `Styled (_, content) ->
          scan_inline_elements anchor content
      in
      scan_inline_elements anchor more
  in
  scan_inline_elements "" content

let section_heading
    : status ->
      top_heading_level:int option ->
      Location.span ->
      [ `Heading of _ ] ->
        int option * (Comment.block_element with_location) =
    fun status ~top_heading_level location heading ->

  let `Heading (level, label, content) = heading in

  let content = non_link_inline_elements status ~surrounding:heading content in

  let label =
    match label with
    | Some label -> label
    | None -> generate_heading_label content
  in
  let label = `Label (status.parent_of_sections, Odoc_model.Names.LabelName.of_string label) in

  match status.sections_allowed, level with
  | `None, _any_level ->
    Error.warning status.warnings (Parse_error.headings_not_allowed location);
    let content = (content :> (Comment.inline_element with_location) list) in
    let element =
      Location.at location
        (`Paragraph [Location.at location
          (`Styled (`Bold, content))])
    in
    top_heading_level, element

  | `No_titles, 0 ->
    Error.warning status.warnings (Parse_error.titles_not_allowed location);
    let element = `Heading (`Title, label, content) in
    let element = Location.at location element in
    let top_heading_level =
      match top_heading_level with
      | None -> Some level
      | some -> some
    in
    top_heading_level, element

  | _, level ->
    let level' =
      match level with
      | 0 -> `Title
      | 1 -> `Section
      | 2 -> `Subsection
      | 3 -> `Subsubsection
      | 4 -> `Paragraph
      | 5 -> `Subparagraph
      | _ ->
        Error.warning status.warnings
          (Parse_error.bad_heading_level level location);
        (* Implicitly promote to level-5. *)
        `Subparagraph
    in
    begin match top_heading_level with
    | Some top_level when
        status.sections_allowed = `All && level <= top_level && level <= 5 ->
      Error.warning status.warnings
        (Parse_error.heading_level_should_be_lower_than_top_level
          level top_level location)
    | _ -> ()
    end;
    let element = `Heading (level', label, content) in
    let element = Location.at location element in
    let top_heading_level =
      match top_heading_level with
      | None -> Some level
      | some -> some
    in
    top_heading_level, element


let validate_first_page_heading status ast_element =
  match status.parent_of_sections with
  | `Page ({file; _}, _) ->
    begin match ast_element with
      | {Location.value = `Heading (_, _, _); _} -> ()
      | _invalid_ast_element ->
        let filename = Odoc_model.Root.Odoc_file.name file ^ ".mld" in
        Error.warning status.warnings
          (Parse_error.page_heading_required filename)
    end
  | _not_a_page -> ()


let top_level_block_elements
    : status -> (Ast.block_element with_location) list ->
        (Comment.block_element with_location) list =
    fun status ast_elements ->

  let rec traverse
      : top_heading_level:int option ->
        (Comment.block_element with_location) list ->
        (Ast.block_element with_location) list ->
          (Comment.block_element with_location) list =
      fun ~top_heading_level comment_elements_acc ast_elements ->

    match ast_elements with
    | [] ->
      List.rev comment_elements_acc

    | ast_element::ast_elements ->
      (* The first [ast_element] in pages must be a title or section heading. *)
      if status.sections_allowed = `All && top_heading_level = None then begin
        validate_first_page_heading status ast_element
      end;

      match ast_element with
      | {value = #Ast.nestable_block_element; _} as element ->
        let element = nestable_block_element status element in
        let element = (element :> Comment.block_element with_location) in
        traverse ~top_heading_level (element::comment_elements_acc) ast_elements

      | {value = `Tag the_tag; location} ->
        begin match tag ~location status the_tag with
          | Result.Ok element ->
            traverse ~top_heading_level (element::comment_elements_acc) ast_elements
          | Result.Error placeholder ->
            traverse ~top_heading_level comment_elements_acc (placeholder::ast_elements)
        end

      | {value = `Heading _ as heading; _} ->
        let top_heading_level, element =
          section_heading
            status
            ~top_heading_level
            ast_element.Location.location
            heading
        in
        traverse ~top_heading_level (element::comment_elements_acc) ast_elements
  in
  let top_heading_level =
    (* Non-page documents have a generated title. *)
    match status.parent_of_sections with
    | `Page _ -> None
    | _parent_with_generated_title -> Some 0
  in
  traverse ~top_heading_level [] ast_elements



let ast_to_comment warnings ~sections_allowed ~parent_of_sections ast =
  let status =
    {
      warnings;
      sections_allowed;
      parent_of_sections;
    }
  in
  top_level_block_elements status ast
