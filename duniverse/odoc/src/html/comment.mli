module Html = Tyxml.Html

val to_html :
  ?xref_base_uri:string ->
  ?syntax:Tree.syntax ->
  Odoc_model.Comment.docs ->
    ([> Html_types.flow5_without_header_footer ] Html.elt) list

val first_to_html :
  ?xref_base_uri:string ->
  ?syntax:Tree.syntax ->
  Odoc_model.Comment.docs ->
    ([> Html_types.flow5_without_header_footer ] Html.elt) list
(** Converts the first paragraph (i.e. everything up to the first blank line) to
    html. *)

val link_content_to_html :
  Odoc_model.Comment.link_content ->
    ([> Html_types.phrasing_without_interactive ] Html.elt) list

val has_doc : Odoc_model.Comment.docs -> bool
