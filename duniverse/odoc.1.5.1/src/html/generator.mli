val keyword : string -> [> Html_types.span ] Tyxml.Html.elt

include module type of Generator_signatures
module Make (Syntax : SYNTAX) : GENERATOR
