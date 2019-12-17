(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common
open Token_tag
open Kstream



(* Namespaces for pattern matching. *)
type ns = [ `HTML | `MathML | `SVG | `Other of string ]
type qname = ns * string

module Ns :
sig
  val to_string : ns -> string
end =
struct
  let to_string = function
    | `HTML -> html_ns
    | `MathML -> mathml_ns
    | `SVG -> svg_ns
    | `Other s -> s
end

(* Specialization of List.mem at qname list, to avoid polymorphic
   comparison. *)
let list_mem_qname ((ns, tag) : qname) l =
  let rec loop = function
    | [] -> false
    | (ns', tag')::_ when ns' = ns && tag' = tag -> true
    | _::rest -> loop rest
  in
  loop l



(* Elements. *)
type element =
  {element_name              : qname;
   location                  : location;
   is_html_integration_point : bool;
   suppress                  : bool;
   mutable buffering         : bool;
   mutable is_open           : bool;
   mutable attributes        : (name * string) list;
   mutable end_location      : location;
   mutable children          : annotated_node list;
   mutable parent            : element}

and node =
  | Element of element
  | Text of string list
  | PI of string * string
  | Comment of string

and annotated_node = location * node



(* Element helpers. *)
module Element :
sig
  val create :
    ?is_html_integration_point:bool ->
    ?suppress:bool ->
    qname -> location ->
      element
  val dummy : element

  val is_special : qname -> bool
  val is_not_hidden : Token_tag.t -> bool
end =
struct
  let rec dummy =
    {element_name              = `HTML, "dummy";
     location                  = 1, 1;
     is_html_integration_point = false;
     suppress                  = true;
     buffering                 = false;
     is_open                   = false;
     attributes                = [];
     end_location              = 1, 1;
     children                  = [];
     parent                    = dummy}

  let create
      ?(is_html_integration_point = false) ?(suppress = false) name location =
    {element_name = name;
     location;
     is_html_integration_point;
     suppress;
     buffering    = false;
     is_open      = true;
     attributes   = [];
     end_location = 1, 1;
     children     = [];
     parent       = dummy}

  let is_special name =
    list_mem_qname name
      [`HTML, "address"; `HTML, "applet"; `HTML, "area";
       `HTML, "article"; `HTML, "aside"; `HTML, "base";
       `HTML, "basefont"; `HTML, "bgsound"; `HTML, "blockquote";
       `HTML, "body"; `HTML, "br"; `HTML, "button";
       `HTML, "caption"; `HTML, "center"; `HTML, "col";
       `HTML, "colgroup"; `HTML, "dd"; `HTML, "details";
       `HTML, "dir"; `HTML, "div"; `HTML, "dl";
       `HTML, "dt"; `HTML, "embed"; `HTML, "fieldset";
       `HTML, "figcaption"; `HTML, "figure"; `HTML, "footer";
       `HTML, "form"; `HTML, "frame"; `HTML, "frameset";
       `HTML, "h1"; `HTML, "h2"; `HTML, "h3";
       `HTML, "h4"; `HTML, "h5"; `HTML, "h6";
       `HTML, "head"; `HTML, "header"; `HTML, "hgroup";
       `HTML, "hr"; `HTML, "html"; `HTML, "iframe";
       `HTML, "img"; `HTML, "input"; `HTML, "isindex";
       `HTML, "li"; `HTML, "link"; `HTML, "listing";
       `HTML, "main"; `HTML, "marquee"; `HTML, "meta";
       `HTML, "nav"; `HTML, "noembed"; `HTML, "noframes";
       `HTML, "noscript"; `HTML, "object"; `HTML, "ol";
       `HTML, "p"; `HTML, "param"; `HTML, "plaintext";
       `HTML, "pre"; `HTML, "script"; `HTML, "section";
       `HTML, "select"; `HTML, "source"; `HTML, "style";
       `HTML, "summary"; `HTML, "table"; `HTML, "tbody";
       `HTML, "td"; `HTML, "template"; `HTML, "textarea";
       `HTML, "tfoot"; `HTML, "th"; `HTML, "thead";
       `HTML, "title"; `HTML, "tr"; `HTML, "track";
       `HTML, "ul"; `HTML, "wbr"; `HTML, "xmp";
       `MathML, "mi"; `MathML, "mo"; `MathML, "mn";
       `MathML, "ms"; `MathML, "mtext"; `MathML, "annotation-xml";
       `SVG, "foreignObject"; `SVG, "desc"; `SVG, "title"]

  let is_not_hidden tag =
    tag.Token_tag.attributes |> List.exists (fun (name, value) ->
      name = "type" && value <> "hidden")
end



(* Context detection. *)
type simple_context = [ `Document | `Fragment of string ]
type context = [ `Document | `Fragment of qname ]

module Context :
sig
  type t

  val uninitialized : unit -> t
  val initialize :
    (location * Html_tokenizer.token) Kstream.t ->
    [< simple_context ] option ->
    t ->
      unit cps

  val the_context : t -> context
  val element : t -> element option
  val token : t -> string option
end =
struct
  let detect tokens throw k =
    let tokens, restore = checkpoint tokens in

    let last_name = ref None in
    let next_token k =
      next_expected tokens throw (fun token ->
        begin match token with
        | _, `Start {name} -> last_name := Some name
        | _ -> ()
        end;
        k token)
    in

    let k context = restore (); k (context, !last_name) in

    let rec scan () =
      next_token begin function
        | _, `Doctype _ -> k `Document
        | _, `Char c when not @@ is_whitespace c -> k (`Fragment "body")
        | _, `Char _ -> scan ()
        | _, `EOF -> k (`Fragment "body")
        | _, `Start {name = "html"} -> k `Document
        | _, `Start {name = "head" | "body" | "frameset"} ->
          k (`Fragment "html")
        | _, `Start {name =
            "base" | "basefont" | "bgsound" | "link" | "meta" | "noframes" |
            "style" | "template" | "title"} ->
          k (`Fragment "head")
        | _, `Start {name = "frame"} -> k (`Fragment "frameset")
        | _, `Start {name = "li"} -> k (`Fragment "ul")
        | _, `Start {name =
            "caption" | "col" | "colgroup" | "tbody" | "tfoot" | "thead"} ->
          k (`Fragment "table")
        | _, `Start {name = "tr"} -> k (`Fragment "tbody")
        | _, `Start {name = "td" | "th"} -> k (`Fragment "tr")
        | _, `Start {name = "optgroup" | "option"} -> k (`Fragment "select")
        | _, `Start {name =
            "altglyph" | "altglyphdef" | "altglyphitem" | "animate" |
            "animatecolor" | "animatemotion" | "animatetransform" | "circle" |
            "clippath" | "color-profile" | "cursor" | "defs" | "desc" |
            "ellipse" | "feblend" | "fecolormatrix" | "fecomponenttransfer" |
            "fecomposite" | "fediffuselighting" | "fedisplacementmap" |
            "fedistantlight" | "feflood" | "fefunca" | "fefuncb" | "fefuncg" |
            "fefuncr" | "fegaussianblur" | "feimage" | "femerge" |
            "femergenode" | "femorphology" | "feoffset" | "fepointlight" |
            "fespecularlighting" | "fespotlight" | "fetile" | "feturbulence" |
            "filter" | "font-face" | "font-face-format" | "font-face-name" |
            "font-face-src" | "font-face-uri" | "foreignobject" | "g" |
            "glyph" | "glyphref" | "hkern" | "image" | "line" |
            "lineargradient" | "marker" | "mask" | "metadata" |
            "missing-glyph" | "mpath" | "path" | "pattern" | "polygon" |
            "polyline" | "radialgradient" | "rect" | "set" | "stop" | "switch" |
            "symbol" | "text" | "textpath" | "tref" | "tspan" | "use"} ->
          k (`Fragment "svg")
        | _, `Start {name =
            "maction" | "maligngroup" | "malignmark" | "menclose" | "merror" |
            "mfenced" | "mfrac" | "mglyph" | "mi" | "mlabeledtr" | "mlongdiv" |
            "mmultiscripts" | "mn" | "mo" | "mover" | "mpadded" | "mphantom" |
            "mroot" | "mrow" | "ms" | "mscarries" | "mscarry" | "msgroup" |
            "msline" | "mspace" | "msqrt" | "msrow" | "mstack" | "mstyle" |
            "msub" | "msup" | "msubsup" | "mtable" | "mtd" | "mtext" | "mtr" |
            "munder" | "munderover" | "semantics" | "annotation" |
            "annotation-xml"} ->
          k (`Fragment "math")
        | _, `Start _ -> k (`Fragment "body")
        | _, (`End _ | `Comment _) -> scan ()
      end
    in

    scan ()

  type t = (context * element option * string option) ref

  let uninitialized () = ref (`Document, None, None)

  let initialize tokens requested_context state throw k =
    (fun k ->
      match requested_context with
      | Some (`Fragment element) ->
        (* HTML element names are case-insensitive, even in foreign content.
           Lowercase the element name given by the user before analysis by the
           parser, to match this convention. [String.lowercase] is acceptable
           here because the API assumes the string [element] is in UTF-8. *)
        k (`Fragment (String.lowercase element), None)
      | Some (`Document as c) -> k (c, None)
      | None -> detect tokens throw k)
    (fun (detected_context, deciding_token) ->

      let context =
        match detected_context with
        | `Document -> `Document
        | `Fragment "math" -> `Fragment (`MathML, "math")
        | `Fragment "svg" -> `Fragment (`SVG, "svg")
        | `Fragment name -> `Fragment (`HTML, name)
      in

      let context_element =
        match context with
        | `Document -> None
        | `Fragment name ->
          let is_html_integration_point =
            match name with
            | `SVG, ("foreignObject" | "desc" | "title") -> true
            | _ -> false
          in

          Some (Element.create
            ~is_html_integration_point ~suppress:true name (1, 1))
      in

      state := context, context_element, deciding_token;

      k ())

  let the_context {contents = (c, _, _)} = c
  let element {contents = (_, e, _)} = e
  let token {contents = (_, _, t)} = t
end



(* Heplers for foreign content. *)
module Foreign :
sig
  val is_mathml_text_integration_point : qname -> bool
  val is_html_integration_point :
    ns -> string -> (string * string) list -> bool

  val adjust_mathml_attributes :
    ((string * string) * string) list -> ((string * string) * string) list
  val adjust_svg_attributes :
    ((string * string) * string) list -> ((string * string) * string) list
  val adjust_svg_tag_name : string -> string
end =
struct
  let is_mathml_text_integration_point qname =
    list_mem_qname qname
      [`MathML, "mi"; `MathML, "mo"; `MathML, "mn"; `MathML, "ms";
       `MathML, "mtext"]

  let is_html_integration_point namespace tag_name attributes =
    match namespace with
    | `HTML | `Other _ -> false
    | `MathML ->
      tag_name = "annotation-xml" &&
      attributes |> List.exists (function
        | "encoding", "text/html" -> true
        | "encoding", "application/xhtml+xml" -> true
        | _ -> false)
    | `SVG ->
      list_mem_string tag_name ["foreignObject"; "desc"; "title"]

  let adjust_mathml_attributes attributes =
    attributes |> List.map (fun ((ns, name), value) ->
      let name =
        if ns = mathml_ns && name = "definitionurl" then "definitionURL"
        else name
      in
      (ns, name), value)

  let adjust_svg_attributes attributes =
    attributes |> List.map (fun ((ns, name), value) ->
      if ns <> svg_ns then (ns, name), value
      else
        let name =
          match name with
          | "attributename" -> "attributeName"
          | "attributetype" -> "attributeType"
          | "basefrequency" -> "baseFrequency"
          | "baseprofile" -> "baseProfile"
          | "calcmode" -> "calcMode"
          | "clippathunits" -> "clipPathUnits"
          | "contentscripttype" -> "contentScriptType"
          | "contentstyletype" -> "contentStyleType"
          | "diffuseconstant" -> "diffuseConstant"
          | "edgemode" -> "edgeMode"
          | "externalresourcesrequired" -> "externalResourcesRequired"
          | "filterres" -> "filterRes"
          | "filterunits" -> "filterUnits"
          | "glyphref" -> "glyphRef"
          | "gradienttransform" -> "gradientTransform"
          | "gradientunits" -> "gradientUnits"
          | "kernelmatrix" -> "kernelMatrix"
          | "kernelunitlength" -> "kernelUnitLength"
          | "keypoints" -> "keyPoints"
          | "keysplines" -> "keySplines"
          | "keytimes" -> "keyTimes"
          | "lengthadjust" -> "lengthAdjust"
          | "limitingconeangle" -> "limitingConeAngle"
          | "markerheight" -> "markerHeight"
          | "markerunits" -> "markerUnits"
          | "markerwidth" -> "markerWidth"
          | "maskcontentunits" -> "maskContentUnits"
          | "maskunits" -> "maskUnits"
          | "numoctaves" -> "numOctaves"
          | "pathlength" -> "pathLength"
          | "patterncontentunits" -> "patternContentUnits"
          | "patterntransform" -> "patternTransform"
          | "patternunits" -> "patternUnits"
          | "pointsatx" -> "pointsAtX"
          | "pointsaty" -> "pointsAtY"
          | "pointsatz" -> "pointsAtZ"
          | "preservealpha" -> "preserveAlpha"
          | "preserveaspectratio" -> "preserveAspectRatio"
          | "primitiveunits" -> "primitiveUnits"
          | "refx" -> "refX"
          | "refy" -> "refY"
          | "repeatcount" -> "repeatCount"
          | "repeatdur" -> "repeatDur"
          | "requiredextensions" -> "requiredExtensions"
          | "requiredfeatures" -> "requiredFeatures"
          | "specularconstant" -> "specularConstant"
          | "specularexponent" -> "specularExponent"
          | "spreadmethod" -> "spreadMethod"
          | "startoffset" -> "startOffset"
          | "stddeviation" -> "stdDeviation"
          | "stitchtiles" -> "stitchTiles"
          | "surfacescale" -> "surfaceScale"
          | "systemlanguage" -> "systemLanguage"
          | "tablevalues" -> "tableValues"
          | "targetx" -> "targetX"
          | "targety" -> "targetY"
          | "textlength" -> "textLength"
          | "viewbox" -> "viewBox"
          | "viewtarget" -> "viewTarget"
          | "xchannelselector" -> "xChannelSelector"
          | "ychannelselector" -> "yChannelSelector"
          | "zoomandpan" -> "zoomAndPan"
          | _ -> name
        in
        (ns, name), value)

  let adjust_svg_tag_name = function
    | "altglyph" -> "altGlyph"
    | "altglyphdef" -> "altGlyphDef"
    | "altglyphitem" -> "altGlyphItem"
    | "animatecolor" -> "animateColor"
    | "animatemotion" -> "animateMotion"
    | "animatetransform" -> "animateTransform"
    | "clippath" -> "clipPath"
    | "feblend" -> "feBlend"
    | "fecolormatrix" -> "feColorMatrix"
    | "fecomponenttransfer" -> "feComponentTransfer"
    | "fecomposite" -> "feComposite"
    | "feconvolvematrix" -> "feConvolveMatrix"
    | "fediffuselighting" -> "feDiffuseLighting"
    | "fedisplacementmap" -> "feDisplacementMap"
    | "fedistantlight" -> "feDistantLight"
    | "fedropshadow" -> "feDropShadow"
    | "feflood" -> "feFlood"
    | "fefunca" -> "feFuncA"
    | "fefuncb" -> "feFuncB"
    | "fefuncg" -> "feFuncG"
    | "fefuncr" -> "feFuncR"
    | "fegaussianblur" -> "feGaussianBlur"
    | "feimage" -> "feImage"
    | "femerge" -> "feMerge"
    | "femergenode" -> "feMergeNode"
    | "femorphology" -> "feMorphology"
    | "feoffset" -> "feOffset"
    | "fepointlight" -> "fePointLight"
    | "fespecularlighting" -> "feSpecularLighting"
    | "fespotlight" -> "feSpotLight"
    | "fetile" -> "feTile"
    | "feturbulence" -> "feTurbulence"
    | "foreignobject" -> "foreignObject"
    | "glyphref" -> "glyphRef"
    | "lineargradient" -> "linearGradient"
    | "radialgradient" -> "radialGradient"
    | "textpath" -> "textPath"
    | s -> s
end



(* Stack of open elements. *)
module Stack :
sig
  type t = element list ref

  val create : unit -> t

  val current_element : t -> element option
  val require_current_element : t -> element
  val adjusted_current_element : Context.t -> t -> element option
  val current_element_is : t -> string list -> bool
  val current_element_is_foreign : Context.t -> t -> bool

  val has : t -> string -> bool

  val in_scope : t -> string -> bool
  val in_button_scope : t -> string -> bool
  val in_list_item_scope : t -> string -> bool
  val in_table_scope : t -> string -> bool
  val in_select_scope : t -> string -> bool
  val one_in_scope : t -> string list -> bool
  val one_in_table_scope : t -> string list -> bool
  val target_in_scope : t -> element -> bool

  val remove : t -> element -> unit
  val replace : t -> old:element -> new_:element -> unit
  val insert_below : t -> anchor:element -> new_:element -> unit
end =
struct
  type t = element list ref

  let create () = ref []

  let current_element open_elements =
    match !open_elements with
    | [] -> None
    | element::_ -> Some element

  let require_current_element open_elements =
    match current_element open_elements with
    | None -> failwith "require_current_element: None"
    | Some element -> element

  let adjusted_current_element context open_elements =
    match !open_elements, Context.element context with
    | [_], Some element -> Some element
    | [], _ -> None
    | element::_, _ -> Some element

  let current_element_is open_elements names =
    match !open_elements with
    | {element_name = `HTML, name}::_ -> list_mem_string name names
    | _ -> false

  let current_element_is_foreign context open_elements =
    match adjusted_current_element context open_elements with
    | Some {element_name = ns, _} when ns <> `HTML -> true
    | _ -> false

  let has open_elements name =
    List.exists
      (fun {element_name = ns, name'} ->
        ns = `HTML && name' = name) !open_elements

  let in_scope_general scope_delimiters open_elements name' =
    let rec scan = function
      | [] -> false
      | {element_name = ns, name'' as name}::more ->
        if ns = `HTML && name'' = name' then true
        else
          if list_mem_qname name scope_delimiters then false
          else scan more
    in
    scan !open_elements

  let scope_delimiters =
    [`HTML, "applet"; `HTML, "caption"; `HTML, "html";
     `HTML, "table"; `HTML, "td"; `HTML, "th";
     `HTML, "marquee"; `HTML, "object"; `HTML, "template";
     `MathML, "mi"; `MathML, "mo"; `MathML, "mn";
     `MathML, "ms"; `MathML, "mtext"; `MathML, "annotation-xml";
     `SVG, "foreignObject"; `SVG, "desc"; `SVG, "title"]

  let in_scope = in_scope_general scope_delimiters

  let in_button_scope = in_scope_general ((`HTML, "button")::scope_delimiters)

  let in_list_item_scope =
    in_scope_general ((`HTML, "ol")::(`HTML, "ul")::scope_delimiters)

  let in_table_scope =
    in_scope_general [`HTML, "html"; `HTML, "table"; `HTML, "template"]

  let in_select_scope open_elements name =
    let rec scan = function
      | [] -> false
      | {element_name = ns, name'}::more ->
        if ns <> `HTML then false
        else
          if name' = name then true
          else
            if name' = "optgroup" || name' = "option" then scan more
            else false
    in
    scan !open_elements

  let one_in_scope open_elements names =
    let rec scan = function
      | [] -> false
      | {element_name = ns, name' as name}::more ->
        if ns = `HTML && list_mem_string name' names then true
        else
          if list_mem_qname name scope_delimiters then false
          else scan more
    in
    scan !open_elements

  let one_in_table_scope open_elements names =
    let rec scan = function
      | [] -> false
      | {element_name = ns, name' as name}::more ->
        if ns = `HTML && list_mem_string name' names then true
        else
          if list_mem_qname name
              [`HTML, "html"; `HTML, "table"; `HTML, "template"] then
            false
          else scan more
    in
    scan !open_elements

  let target_in_scope open_elements node =
    let rec scan = function
      | [] -> false
      | e::more ->
        if e == node then true
        else
          if list_mem_qname node.element_name scope_delimiters then false
          else scan more
    in
    scan !open_elements

  let remove open_elements element =
    open_elements := List.filter ((!=) element) !open_elements;
    element.is_open <- false

  let replace open_elements ~old ~new_ =
    open_elements :=
      List.map (fun e ->
        if e == old then (e.is_open <- false; new_) else e) !open_elements

  let insert_below open_elements ~anchor ~new_ =
    let rec insert prefix = function
      | [] -> List.rev prefix
      | e::more when e == anchor -> (List.rev prefix) @ (new_::e::more)
      | e::more -> insert (e::prefix) more
    in
    open_elements := insert [] !open_elements
end



(* List of active formatting elements. *)
module Active :
sig
  type entry =
    | Marker
    | Element_ of element * location * Token_tag.t

  type t = entry list ref

  val create : unit -> t

  val add_marker : t -> unit
  val clear_until_marker : t -> unit

  val has : t -> element -> bool
  val remove : t -> element -> unit
  val replace : t -> old:element -> new_:element -> unit
  val insert_after : t -> anchor:element -> new_:element -> unit

  val has_before_marker : t -> string -> element option
end =
struct
  type entry =
    | Marker
    | Element_ of element * location * Token_tag.t

  type t = entry list ref

  let create () = ref []

  let add_marker active_formatting_elements =
    active_formatting_elements := Marker::!active_formatting_elements

  let clear_until_marker active_formatting_elements =
    let rec iterate = function
      | Marker::rest -> rest
      | (Element_ _)::rest -> iterate rest
      | [] -> []
    in
    active_formatting_elements := iterate !active_formatting_elements

  let has active_formatting_elements element =
    !active_formatting_elements |> List.exists (function
      | Element_ (e, _, _) when e == element -> true
      | _ -> false)

  let remove active_formatting_elements element =
    active_formatting_elements :=
      !active_formatting_elements |> List.filter (function
        | Element_ (e, _, _) when e == element -> false
        | _ -> true)

  let replace active_formatting_elements ~old ~new_ =
    active_formatting_elements :=
      !active_formatting_elements |> List.map (function
        | Element_ (e, l, t) when e == old -> Element_ (new_, l, t)
        | e -> e)

  let insert_after active_formatting_elements ~anchor ~new_ =
    let rec insert prefix = function
      | [] -> List.rev prefix
      | (Element_ (e, l, t) as v)::more when e == anchor ->
        let new_entry = Element_ (new_, l, t) in
        (List.rev prefix) @ (v::new_entry::more)
      | v::more -> insert (v::prefix) more
    in
    active_formatting_elements := insert [] !active_formatting_elements

  let has_before_marker active_formatting_elements name =
    let rec scan = function
      | [] | Marker::_ -> None
      | Element_ (n, _, _)::_ when n.element_name = (`HTML, name) -> Some n
      | _::more -> scan more
    in
    scan !active_formatting_elements
end



type mode = unit -> unit

(* Stack of template insertion modes. *)
module Template :
sig
  type t = mode list ref

  val create : unit -> t

  val push : t -> mode -> unit
  val pop : t -> unit
end =
struct
  type t = (unit -> unit) list ref

  let create () = ref []

  let push template_insertion_modes mode =
    template_insertion_modes := mode::!template_insertion_modes

  let pop template_insertion_modes =
    match !template_insertion_modes with
    | [] -> ()
    | _::rest -> template_insertion_modes := rest
end



(* Subtree buffers. HTML specifies the "adoption agency algorithm" for
   recovering from certain kinds of errors. This algorithm is (apparently)
   incompatible with streaming parsers that do not maintain a DOM - such as
   Markup.ml. So, when the Markup.ml parser encounters a situation in which it
   may be necessary to later run the adoption agency algorithm, it buffers its
   signal output. Instead of being emitted, the signals are used to construct a
   DOM subtree. If the algorithm is run, it is run on this subtree. Whenever the
   parser can "prove" that the subtree can no longer be involved in the adoption
   agency algorithm, it serializes the subtree into the signal stream. In
   practice, this means that buffering begins when a formatting element is
   encountered, and ends when the parent of the formatting element is popped off
   the open element stack. *)
module Subtree :
sig
  type t

  val create : Stack.t -> t

  val accumulate : t -> location -> signal -> bool

  val enable : t -> unit
  val disable : t -> (location * signal) list

  val adoption_agency_algorithm :
    t -> Active.t -> location -> string -> bool * (location * Error.t) list
end =
struct
  type t =
    {open_elements    : Stack.t;
     mutable enabled  : bool;
     mutable position : element}

  let create open_elements =
    {open_elements;
     enabled  = false;
     position = Element.dummy}

  let accumulate subtree_buffer l s =
    if not subtree_buffer.enabled then true
    else begin
      begin match s with
      | `Start_element (_, attributes) ->
        let parent = subtree_buffer.position in
        let child =
          Stack.require_current_element subtree_buffer.open_elements in

        child.attributes <- attributes;
        child.parent <- parent;
        parent.children <- (l, Element child)::parent.children;

        subtree_buffer.position <- child

      | `End_element ->
        subtree_buffer.position.end_location <- l;
        subtree_buffer.position <-
          Stack.require_current_element subtree_buffer.open_elements

      | `Text ss ->
        subtree_buffer.position.children <-
          (l, Text ss)::subtree_buffer.position.children

      | `PI (t, s) ->
        subtree_buffer.position.children <-
          (l, PI (t, s))::subtree_buffer.position.children

      | `Comment s ->
        subtree_buffer.position.children <-
          (l, Comment s)::subtree_buffer.position.children

      | `Xml _ | `Doctype _ -> ()
      end;

      false
    end

  let enable subtree_buffer =
    if subtree_buffer.enabled then ()
    else
      match Stack.current_element subtree_buffer.open_elements with
      | None -> ()
      | Some element ->
        element.buffering <- true;
        subtree_buffer.position <- element;
        subtree_buffer.enabled <- true

  let disable subtree_buffer =
    let rec traverse acc = function
      | l, Element {element_name; attributes; end_location; children} ->
        let name = Ns.to_string (fst element_name), snd element_name in
        let start_signal = l, `Start_element (name, attributes) in
        let end_signal = end_location, `End_element in
        start_signal::(List.fold_left traverse (end_signal::acc) children)

      | l, Text ss ->
        begin match acc with
        | (_, `Text ss')::rest -> (l, `Text (ss @ ss'))::rest
        | _ -> (l, `Text ss)::acc
        end

      | l, PI (t, s) -> (l, `PI (t, s))::acc
      | l, Comment s -> (l, `Comment s)::acc
    in

    let result =
      List.fold_left traverse []
        (Stack.require_current_element subtree_buffer.open_elements).children
    in

    subtree_buffer.enabled <- false;

    result

  (* Part of 8.2.5.4.7. *)
  let adoption_agency_algorithm
      subtree_buffer active_formatting_elements l subject =

    let open_elements = subtree_buffer.open_elements in

    let above_removed_nodes = ref [] in

    let rec above_in_stack node = function
      | e::e'::_ when e == node -> e'
      | _::more -> above_in_stack node more
      | [] -> failwith "above_in_stack: not found"
    in

    let above_node node =
      if node.is_open then above_in_stack node !open_elements
      else
        try List.find (fun (e, _) -> e == node) !above_removed_nodes |> snd
        with Not_found -> failwith "above_node: not found"
    in

    let remove_node node =
      above_removed_nodes :=
        (node, above_in_stack node !open_elements)::!above_removed_nodes;
      Stack.remove open_elements node
    in

    let reparent node new_parent =
      let old_parent = node.parent in

      let entry, filtered_children =
        let rec remove prefix = function
          | (_, Element e as entry)::rest when e == node ->
            entry, (List.rev prefix) @ rest
          | e::rest -> remove (e::prefix) rest
          | [] -> (node.location, Element node), old_parent.children
        in
        remove [] old_parent.children
      in

      old_parent.children <- filtered_children;
      new_parent.children <- entry::new_parent.children;
      node.parent <- new_parent
    in

    let inner_loop formatting_element furthest_block =
      let rec repeat inner_loop_counter node last_node bookmark =
        let node = above_node node in

        if node == formatting_element then last_node, bookmark
        else begin
          if inner_loop_counter > 3 then
            Active.remove active_formatting_elements node;

          if not @@ Active.has active_formatting_elements node then begin
            remove_node node;
            repeat (inner_loop_counter + 1) node last_node bookmark
          end
          else begin
            let new_node =
              {node with is_open = true; children = []; parent = Element.dummy}
            in

            node.end_location <- l;

            Stack.replace open_elements ~old:node ~new_:new_node;
            Active.replace active_formatting_elements ~old:node ~new_:new_node;

            reparent last_node new_node;

            repeat (inner_loop_counter + 1) new_node new_node
              (if last_node == furthest_block then Some new_node else bookmark)
          end
        end

      in
      repeat 1 furthest_block furthest_block None
    in

    let find_formatting_element () =
      let rec scan = function
        | [] -> None
        | Active.Marker::_ -> None
        | (Active.Element_ ({element_name = `HTML, n} as e, _, _))::_
            when n = subject -> Some e
        | _::rest -> scan rest
      in
      scan !active_formatting_elements
    in

    let find_furthest_block formatting_element =
      let rec scan furthest = function
        | [] -> furthest
        | e::_ when e == formatting_element -> furthest
        | e::more when Element.is_special e.element_name -> scan (Some e) more
        | _::more -> scan furthest more
      in
      scan None !open_elements
    in

    let pop_to_formatting_element formatting_element =
      let rec pop () =
        match !open_elements with
        | [] -> ()
        | e::more ->
          open_elements := more;
          e.is_open <- false;
          e.end_location <- l;
          if e != formatting_element then pop ()
      in
      pop ();
      subtree_buffer.position <- Stack.require_current_element open_elements
    in

    let rec outer_loop outer_loop_counter errors =
      let outer_loop_counter = outer_loop_counter + 1 in

      if outer_loop_counter >= 8 then true, List.rev errors
      else begin
        match find_formatting_element () with
        | None -> false, List.rev errors
        | Some formatting_element ->
          if not formatting_element.is_open then begin
            Active.remove active_formatting_elements formatting_element;
            true, List.rev ((l, `Unmatched_end_tag subject)::errors)
          end
          else begin
            if not @@ Stack.target_in_scope open_elements
                        formatting_element then begin
              true, List.rev ((l, `Unmatched_end_tag subject)::errors)
            end
            else begin
              let errors =
                if Stack.require_current_element open_elements ==
                   formatting_element then
                  errors
                else (l, `Unmatched_end_tag subject)::errors
              in

              match find_furthest_block formatting_element with
              | None ->
                pop_to_formatting_element formatting_element;
                Active.remove active_formatting_elements formatting_element;
                true, List.rev errors

              | Some furthest_block ->
                formatting_element.end_location <- l;

                let common_ancestor =
                  above_in_stack formatting_element !open_elements in

                let last_node, bookmark =
                  inner_loop formatting_element furthest_block in

                reparent last_node common_ancestor;

                let new_node =
                  {formatting_element with
                    is_open = true; children = []; parent = Element.dummy}
                in

                new_node.children <- furthest_block.children;
                furthest_block.children <- [];
                new_node.children |> List.iter (function
                  | _, Element child -> child.parent <- new_node
                  | _ -> ());

                reparent new_node furthest_block;

                begin match bookmark with
                | None ->
                  Active.replace active_formatting_elements
                    ~old:formatting_element ~new_:new_node
                | Some node ->
                  Active.remove active_formatting_elements formatting_element;
                  Active.insert_after
                    active_formatting_elements ~anchor:node ~new_:new_node
                end;

                Stack.remove open_elements formatting_element;
                Stack.insert_below
                  open_elements ~anchor:furthest_block ~new_:new_node;

                outer_loop outer_loop_counter errors
            end
          end
      end
    in

    let current_node = Stack.require_current_element open_elements in
    if current_node.element_name = (`HTML, subject) then begin
      open_elements := List.tl !open_elements;
      current_node.is_open <- false;
      current_node.end_location <- l;
      subtree_buffer.position <- Stack.require_current_element open_elements;
      Active.remove active_formatting_elements current_node;
      true, []
    end
    else outer_loop 0 []
end



let parse requested_context report (tokens, set_tokenizer_state, set_foreign) =
  let context = Context.uninitialized () in

  let throw = ref (fun _ -> ()) in
  let ended = ref (fun _ -> ()) in
  let output = ref (fun _ -> ()) in

  let report_if = Error.report_if report in
  let unmatched_end_tag l name k =
    report l (`Unmatched_end_tag name) !throw k in

  let open_elements = Stack.create () in
  let active_formatting_elements = Active.create () in
  let subtree_buffer = Subtree.create open_elements in
  let text = Text.prepare () in
  let template_insertion_modes = Template.create () in
  let frameset_ok = ref true in
  let head_seen = ref false in
  let form_element_pointer = ref None in

  let add_character = Text.add text in

  set_foreign (fun () ->
    Stack.current_element_is_foreign context open_elements);

  let report_if_stack_has_other_than names k =
    let rec iterate = function
      | [] -> k ()
      | {element_name = ns, name; location}::more ->
        report_if (not (ns = `HTML && list_mem_string name names))
          location (fun () -> `Unmatched_start_tag name) !throw (fun () ->
        iterate more)
    in
    iterate !open_elements
  in

  let rec current_mode = ref initial_mode

  and constructor throw_ k =
    Context.initialize tokens requested_context context throw_ (fun () ->

    let initial_tokenizer_state =
      match Context.the_context context with
      | `Fragment (`HTML, ("title" | "textarea")) -> `RCDATA
      | `Fragment
          (`HTML, ("style" | "xmp" | "iframe" | "noembed" | "noframes")) ->
        `RAWTEXT
      | `Fragment (`HTML, "script") -> `Script_data
      | `Fragment (`HTML, "plaintext") -> `PLAINTEXT
      | _ -> `Data
    in

    set_tokenizer_state initial_tokenizer_state;

    begin match Context.the_context context with
    | `Document -> ()
    | `Fragment _ ->
      let notional_root =
        Element.create ~suppress:true (`HTML, "html") (1, 1) in
      open_elements := [notional_root]
    end;

    begin match Context.the_context context with
    | `Fragment (`HTML, "template") ->
      Template.push template_insertion_modes in_template_mode
    | _ -> ()
    end;

    (* The following is a deviation from conformance. The goal is to avoid
       insertion of a <head> element into a fragment beginning with a <body> or
       <frameset> element. *)
    begin match Context.token context with
    | Some ("body" | "frameset") -> head_seen := true
    | _ -> ()
    end;

    current_mode :=
      begin match Context.the_context context with
      | `Fragment _ -> reset_mode ()
      | `Document -> initial_mode
      end;

    (fun throw_ e k ->
      throw := throw_;
      ended := e;
      output := k;
      !current_mode ())
    |> make
    |> k)

  (* 8.2.3.1. *)
  and reset_mode () =
    let rec iterate last = function
      | [e] when not last && Context.the_context context <> `Document ->
        begin match Context.the_context context with
        | `Document -> assert false
        | `Fragment name -> iterate true [{e with element_name = name}]
        end
      | {element_name = _, "select"}::ancestors ->
        let rec iterate' = function
          | [] -> in_select_mode
          | {element_name = _, "template"}::_ -> in_select_mode
          | {element_name = _, "table"}::_ -> in_select_in_table_mode
          | _::ancestors -> iterate' ancestors
        in
        iterate' ancestors
      | {element_name = _, ("tr" | "th")}::_::_ -> in_cell_mode
      | {element_name = _, "tr"}::_ -> in_row_mode
      | {element_name = _, ("tbody" | "thead" | "tfoot")}::_ ->
        in_table_body_mode
      | {element_name = _, "caption"}::_ -> in_caption_mode
      | {element_name = _, "colgroup"}::_ -> in_column_group_mode
      | {element_name = _, "table"}::_ -> in_table_mode
      | {element_name = _, "template"}::_ ->
        begin match !template_insertion_modes with
        | [] -> initial_mode (* This is an internal error, actually. *)
        | mode::_ -> mode
        end
      (* The next case corresponds to item 12 of "Resetting the insertion mode
         appropriately." It is commented out as deliberate deviation from the
         specification, because that makes parsing of fragments intended for
         <head> elements more intuitive. For conformance, the pattern in the
         following case would have to end with ::_::_, not ::_. *)
      (* | [{element_name = _, "head"}] -> in_body_mode *)
      | {element_name = _, "head"}::_ -> in_head_mode
      | {element_name = _, "body"}::_ -> in_body_mode
      | {element_name = _, "frameset"}::_ -> in_frameset_mode
      | {element_name = _, "html"}::_ ->
        if !head_seen then after_head_mode else before_head_mode
      | _::rest -> iterate last rest
      | [] -> in_body_mode
    in
    iterate false !open_elements

  and emit' l s m =
    if Subtree.accumulate subtree_buffer l s then begin
      current_mode := m;
      !output (l, s)
    end
    else m ()

  and emit_list ss m =
    match ss with
    | [] -> m ()
    | (l, s)::more -> emit' l s (fun () -> emit_list more m)

  and emit_text m =
    match Text.emit text with
    | None -> m ()
    | Some (l', strings) ->
      emit' l' (`Text strings) m

  and emit l s m = emit_text (fun () -> emit' l s m)

  and push_and_emit
      ?(formatting = false) ?(acknowledge = false) ?(namespace = `HTML)
      ?(set_form_element_pointer = false) location
      ({Token_tag.name; attributes; self_closing} as tag) mode =

    report_if (self_closing && not acknowledge) location (fun () ->
      `Bad_token ("/>", "tag", "should not be self-closing"))
      !throw (fun () ->

    let namespace_string = Ns.to_string namespace in

    let tag_name =
      match namespace with
      | `SVG -> Foreign.adjust_svg_tag_name name
      | _ -> name
    in

    let is_html_integration_point =
      Foreign.is_html_integration_point namespace tag_name attributes in

    let attributes =
      List.map (fun (n, v) -> Namespace.Parsing.parse n, v) attributes in
    let attributes =
      match namespace with
      | `HTML | `Other _ -> attributes
      | `MathML -> Foreign.adjust_mathml_attributes attributes
      | `SVG -> Foreign.adjust_svg_attributes attributes
    in

    let element_entry =
      Element.create ~is_html_integration_point (namespace, name) location
    in
    open_elements := element_entry::!open_elements;

    if set_form_element_pointer then
      form_element_pointer := Some element_entry;

    if formatting then
      active_formatting_elements :=
        Active.Element_ (element_entry, location, tag)::
          !active_formatting_elements;

    emit location
      (`Start_element ((namespace_string, tag_name), attributes)) mode)

  and push_implicit location name mode =
    push_and_emit location
      {Token_tag.name = name; attributes = []; self_closing = false} mode

  and pop location mode =
    match !open_elements with
    | [] -> mode ()
    | element::more ->
      emit_text (fun () ->
      (fun k ->
        if not element.buffering then k ()
        else emit_list (Subtree.disable subtree_buffer) k)
      (fun () ->
        open_elements := more;
        element.is_open <- false;
        if element.suppress then mode ()
        else emit' location `End_element mode))

  and pop_until condition location mode =
    let rec iterate () =
      match !open_elements with
      | [] -> mode ()
      | element::_ ->
        if condition element then mode ()
        else pop location iterate
    in
    iterate ()

  and close_element ?(ns = `HTML) l name mode =
    pop_until
      (fun {element_name = ns', name'} -> ns' = ns && name' = name) l
      (fun () ->
    pop l mode)

  and pop_until_and_raise_errors names location mode =
    let rec iterate () =
      match !open_elements with
      | [] -> mode ()
      | {element_name = ns, name}::_ ->
        if ns = `HTML && list_mem_string name names then pop location mode
        else
          report location (`Unmatched_start_tag name) !throw (fun () ->
          pop location iterate)
    in
    iterate ()

  and pop_implied ?(except = "") location mode =
    pop_until (fun {element_name = _, name} ->
      name = except ||
        not @@ list_mem_string name
          ["dd"; "dt"; "li"; "option"; "optgroup"; "p"; "rb"; "rp"; "rt";
           "rtc"]) location mode

  and pop_to_table_context location mode =
    pop_until (function
      | {element_name = `HTML, ("table" | "template" | "html")} -> true
      | _ -> false) location mode

  and pop_to_table_body_context location mode =
    pop_until (function
      | {element_name =
          `HTML, ("tbody" | "thead" | "tfoot" | "template" | "html")} -> true
      | _ -> false) location mode

  and pop_to_table_row_context location mode =
    pop_until (function
      | {element_name = `HTML, ("tr" | "template" | "html")} -> true
      | _ -> false) location mode

  and close_element_with_implied name location mode =
    pop_implied ~except:name location (fun () ->
    let check_element k =
      match Stack.current_element open_elements with
      | Some {element_name = `HTML, name'} when name' = name -> k ()
      | Some {element_name = _, name; location} ->
        report location (`Unmatched_start_tag name) !throw k
      | None ->
        unmatched_end_tag location name k
    in
    check_element (fun () ->
    close_element location name mode))

  and close_cell location mode =
    pop_implied location (fun () ->
    (fun mode ->
      match Stack.current_element open_elements with
      | Some {element_name = `HTML, ("td" | "th")} -> mode ()
      | Some {element_name = _, name} ->
        unmatched_end_tag location name mode
      | None ->
        unmatched_end_tag location "" mode)
    @@ (fun () ->
    pop_until (function
      | {element_name = `HTML, ("td" | "th")} -> true
      | _ -> false) location (fun () ->
    pop location mode)))

  and close_current_p_element l mode =
    if Stack.in_button_scope open_elements "p" then
      close_element_with_implied "p" l mode
    else mode ()

  and close_preceding_tag names l mode =
    let rec scan = function
      | [] -> mode ()
      | {element_name = (ns, name) as name'}::more ->
        if ns = `HTML && list_mem_string name names then
          close_element_with_implied name l mode
        else
          if Element.is_special name' &&
            not @@ list_mem_qname name'
              [`HTML, "address"; `HTML, "div"; `HTML, "p"] then
            mode ()
          else
            scan more
    in
    scan !open_elements

  and emit_end l =
    pop_until (fun _ -> false) l (fun () ->
    emit_text (fun () ->
    !ended ()))

  and reconstruct_active_formatting_elements mode =
    let rec get_prefix prefix = function
      | [] -> prefix, []
      | Active.Marker::_ as l -> prefix, l
      | Active.Element_ ({is_open = true}, _, _)::_ as l -> prefix, l
      | Active.Element_ ({is_open = false}, l, tag)::more ->
        get_prefix ((l, tag)::prefix) more
    in
    let to_reopen, remainder = get_prefix [] !active_formatting_elements in
    active_formatting_elements := remainder;

    begin match to_reopen with
    | [] -> ()
    | _::_ -> Subtree.enable subtree_buffer
    end;

    let rec reopen = function
      | [] -> mode ()
      | (l, tag)::more ->
        push_and_emit ~formatting:true l tag (fun () -> reopen more)
    in
    reopen to_reopen

  (* 8.2.5. *)
  and dispatch tokens rules =
    next tokens !throw (fun () -> !ended ()) begin fun ((_, t) as v) ->
      let foreign =
        match Stack.adjusted_current_element context open_elements, t with
        | None, _ -> false
        | Some {element_name = `HTML, _}, _ -> false
        | Some {element_name}, `Start {name}
            when Foreign.is_mathml_text_integration_point element_name
            && name <> "mglyph" && name <> "malignmark" -> false
        | Some {element_name = `MathML, "annotation-xml"},
            `Start {name = "svg"} -> false
        | Some {is_html_integration_point = true}, `Start _ -> false
        | Some {is_html_integration_point = true}, `Char _ -> false
        | _, `EOF -> false
        | _ -> true
      in

      if not foreign then rules v
      else foreign_content !current_mode (fun () -> rules v) v
    end

  (* 8.2.5.4.1. *)
  and initial_mode () =
    dispatch tokens begin function
      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) ->
        initial_mode ()

      | l, `Comment s ->
        emit l (`Comment s) initial_mode

      | l, `Doctype d ->
        emit l (`Doctype d) before_html_mode

      | v ->
        push tokens v;
        before_html_mode ()
    end

  (* 8.2.5.4.2. *)
  and before_html_mode () =
    dispatch tokens begin function
      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          before_html_mode

      | l, `Comment s ->
        emit l (`Comment s) before_html_mode

      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) ->
        before_html_mode ()

      | l, `Start ({name = "html"} as t) ->
        push_and_emit l t before_head_mode

      | l, `End {name}
          when not @@ list_mem_string name ["head"; "body"; "html"; "br"] ->
        unmatched_end_tag l name before_html_mode

      | l, _ as v ->
        push tokens v;
        push_implicit l "html" before_head_mode
    end

  (* 8.2.5.4.3. *)
  and before_head_mode () =
    dispatch tokens begin function
      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) ->
        before_head_mode ()

      | l, `Comment s ->
        emit l (`Comment s) before_head_mode

      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          before_head_mode

      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "html" before_head_mode v

      | l, `Start ({name = "head"} as t) ->
        head_seen := true;
        push_and_emit l t in_head_mode

      | l, `End {name}
          when not @@ list_mem_string name ["head"; "body"; "html"; "br"] ->
        report l (`Unmatched_end_tag name) !throw before_head_mode

      | l, _ as v ->
        head_seen := true;
        push tokens v;
        push_implicit l "head" in_head_mode
    end

  (* 8.2.5.4.4. *)
  and in_head_mode () =
    dispatch tokens (fun v -> in_head_mode_rules in_head_mode v)

  (* 8.2.5.4.4. *)
  and in_head_mode_rules mode = function
    | l, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
      add_character l c;
      mode ()

    | l, `Comment s ->
      emit l (`Comment s) mode

    | l, `Doctype _ ->
      report l (`Bad_document "doctype should be first") !throw mode

    | _, `Start {name = "html"} as v ->
      in_body_mode_rules "head" in_head_mode v

    | l, `Start ({name =
        "base" | "basefont" | "bgsound" | "link" | "meta"} as t) ->
      push_and_emit ~acknowledge:true l t (fun () ->
      pop l mode)

    | l, `Start ({name = "title"} as t) ->
      push_and_emit l t (fun () ->
      parse_rcdata mode)

    | l, `Start ({name = "noframes" | "style"} as t) ->
      push_and_emit l t (fun () ->
      parse_rawtext mode)

    | l, `Start ({name = "noscript"} as t) ->
      push_and_emit l t in_head_noscript_mode

    | l, `Start ({name = "script"} as t) ->
      push_and_emit l t (fun () ->
      set_tokenizer_state `Script_data;
      text_mode mode)

    | l, `End {name = "head"} ->
      pop l after_head_mode

    | l, `Start ({name = "template"} as t) ->
      Active.add_marker active_formatting_elements;
      frameset_ok := false;
      Template.push template_insertion_modes in_template_mode;
      push_and_emit l t in_template_mode

    | l, `End {name = "template"} ->
      if not @@ Stack.has open_elements "template" then
        report l (`Unmatched_end_tag "template") !throw mode
      else begin
        Active.clear_until_marker active_formatting_elements;
        Template.pop template_insertion_modes;
        close_element_with_implied "template" l (fun () -> reset_mode () ())
      end

    | l, `Start {name = "head"} ->
      report l (`Misnested_tag ("head", "head")) !throw mode

    | l, `End {name} when not @@ list_mem_string name ["body"; "html"; "br"] ->
      report l (`Unmatched_end_tag name) !throw mode

    | l, _ as v ->
      push tokens v;
      pop l after_head_mode

  (* 8.2.5.4.5. *)
  and in_head_noscript_mode () =
    dispatch tokens begin function
      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          in_head_noscript_mode

      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "noscript" in_head_noscript_mode v

      | l, `End {name = "noscript"} ->
        pop l in_head_mode

      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020)
      | _, `Comment _
      | _, `Start {name =
          "basefont" | "bgsound" | "link" | "meta" | "noframes" |
          "style"} as v ->
        in_head_mode_rules in_head_noscript_mode v

      | l, `Start {name = "head" | "noscript" as name} ->
        report l (`Misnested_tag (name, "noscript")) !throw
          in_head_noscript_mode

      | l, `End {name} when name <> "br" ->
        report l (`Unmatched_end_tag name) !throw in_head_noscript_mode

      | l, _ as v ->
        report l (`Bad_content "noscript") !throw (fun () ->
        push tokens v;
        pop l in_head_mode)
    end

  (* 8.2.5.4.6. *)
  and after_head_mode () =
    dispatch tokens begin function
      | l, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
        add_character l c;
        after_head_mode ()

      | l, `Comment s ->
        emit l (`Comment s) after_head_mode

      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          after_head_mode

      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "html" after_head_mode v

      | l, `Start ({name = "body"} as t) ->
        frameset_ok := false;
        push_and_emit l t in_body_mode

      | l, `Start ({name = "frameset"} as t) ->
        push_and_emit l t in_frameset_mode

      | l, `Start {name =
          "base" | "basefont" | "bgsound" | "link" | "meta" | "noframes" |
          "script" | "style" | "template" | "title" as name} as v ->
        report l (`Misnested_tag (name, "html")) !throw (fun () ->
        in_head_mode_rules after_head_mode v)

      | _, `End {name = "template"} as v ->
        in_head_mode_rules after_head_mode v

      | l, `Start {name = "head"} ->
        report l (`Bad_document "duplicate head element") !throw
          after_head_mode

      | l, `End {name}
          when not @@ list_mem_string name ["body"; "html"; "br"] ->
        report l (`Unmatched_end_tag name) !throw after_head_mode

      (* This case is not found in the specification. It is a deliberate
         deviation from conformance, so that fragments "<head>...</head>" don't
         get an implicit <body> element generated after the <head> element. *)
      | l, `EOF
          when (Context.the_context context = `Fragment (`HTML, "html")
             || Context.the_context context = `Fragment (`HTML, "head")) ->
        emit_end l

      | l, _ as t ->
        push tokens t;
        push_implicit l "body" in_body_mode
    end

  (* 8.2.5.4.7. *)
  and in_body_mode () =
    dispatch tokens (fun v -> in_body_mode_rules "body" in_body_mode v)

  (* 8.2.5.4.7. *)
  and in_body_mode_rules context_name mode = function
    | l, `Char 0 ->
      report l (`Bad_token ("U+0000", "body", "null")) !throw mode

    | l, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
      reconstruct_active_formatting_elements (fun () ->
      add_character l c;
      mode ())

    | l, `Char c ->
      frameset_ok := false;
      reconstruct_active_formatting_elements (fun () ->
      add_character l c;
      mode ())

    | l, `Comment s ->
      emit l (`Comment s) mode

    | l, `Doctype _ ->
      report l (`Bad_document "doctype should be first") !throw mode

    | l, `Start {name = "html"} ->
      report l (`Misnested_tag ("html", context_name)) !throw mode

    | _, `Start {name =
        "base" | "basefont" | "bgsound" | "link" | "meta" | "noframes" |
        "script" | "style" | "template" | "title"}
    | _, `End {name = "template"} as v ->
      in_head_mode_rules mode v

    | l, `Start {name = "body"} ->
      report l (`Misnested_tag ("body", context_name)) !throw mode

    | l, `Start ({name = "frameset"} as t) ->
      report l (`Misnested_tag ("frameset", context_name)) !throw (fun () ->
      match !open_elements with
      | [_] -> mode ()
      | _ ->
        let rec second_is_body = function
          | [{element_name = `HTML, "body"}; _] -> true
          | [] -> false
          | _::more -> second_is_body more
        in
        if not @@ second_is_body !open_elements then mode ()
        else
          if not !frameset_ok then mode ()
          else
            (* There is a deviation here due to the nature of the parser: if a
               body element has been emitted, it can't be suppressed. *)
            pop_until
              (fun _ -> match !open_elements with [_] -> true | _ -> false)
              l (fun () ->
            push_and_emit l t in_frameset_mode))

    | l, `EOF as v ->
      report_if_stack_has_other_than
        ["dd"; "dt"; "li"; "p"; "tbody"; "td"; "tfoot"; "th"; "thead"; "tr";
         "body"; "html"] (fun () ->
      match !template_insertion_modes with
      | [] -> emit_end l
      | _ -> in_template_mode_rules mode v)

    | l, `End {name = "body"} ->
      if not @@ Stack.in_scope open_elements "body" then
        report l (`Unmatched_end_tag "body") !throw mode
      else
        report_if_stack_has_other_than
          ["dd"; "dt"; "li"; "optgroup"; "option"; "p"; "rb"; "rp"; "rt";
           "rtc"; "tbody"; "td"; "tfoot"; "th"; "thead"; "tr"; "body";
           "html"] (fun () ->
        after_body_mode ())

    | l, `End {name = "html"} as v ->
      if not @@ Stack.in_scope open_elements "body" then
        report l (`Unmatched_end_tag "html") !throw mode
      else
        report_if_stack_has_other_than
          ["dd"; "dt"; "li"; "optgroup"; "option"; "p"; "rb"; "rp"; "rt";
           "rtc"; "tbody"; "td"; "tfoot"; "th"; "thead"; "tr"; "body";
           "html"] (fun () ->
        push tokens v;
        after_body_mode ())

    | l, `Start ({name =
        "address" | "article" | "aside" | "blockquote" | "center" |
        "details" | "dialog" | "dir" | "div" | "dl" | "fieldset" |
        "figcaption" | "figure" | "footer" | "header" | "hgroup" | "main" |
        "nav" | "ol" | "p" | "section" | "summary" | "ul"} as t) ->
      close_current_p_element l (fun () ->
      push_and_emit l t mode)

    | l, `Start ({name =
        "h1" | "h2" | "h3" | "h4" | "h5" | "h6" as name} as t) ->
      close_current_p_element l (fun () ->
      (fun mode' ->
        match Stack.current_element open_elements with
        | Some {element_name = `HTML,
            ("h1" | "h2" | "h3" | "h4" | "h5" | "h6" as name')} ->
          report l (`Misnested_tag (name, name')) !throw (fun () ->
          pop l mode')
        | _ -> mode' ()) (fun () ->
      push_and_emit l t mode))

    | l, `Start ({name = "pre" | "listing"} as t) ->
      frameset_ok := false;
      close_current_p_element l (fun () ->
      push_and_emit l t (fun () ->
      next_expected tokens !throw (function
        | _, `Char 0x000A -> mode ()
        | v ->
          push tokens v;
          mode ())))

    | l, `Start ({name = "form"} as t) ->
      if !form_element_pointer <> None &&
         not @@ Stack.has open_elements "template" then
        report l (`Misnested_tag ("form", "form")) !throw mode
      else begin
        close_current_p_element l (fun () ->
        let in_template = Stack.has open_elements "template" in
        push_and_emit ~set_form_element_pointer:(not in_template) l t mode)
      end

    | l, `Start ({name = "li"} as t) ->
      frameset_ok := false;
      close_preceding_tag ["li"] l (fun () ->
      close_current_p_element l (fun () ->
      push_and_emit l t mode))

    | l, `Start ({name = "dd" | "dt"} as t) ->
      frameset_ok := false;
      close_preceding_tag ["dd"; "dt"] l (fun () ->
      close_current_p_element l (fun () ->
      push_and_emit l t mode))

    | l, `Start ({name = "plaintext"} as t) ->
      close_current_p_element l (fun () ->
      set_tokenizer_state `PLAINTEXT;
      push_and_emit l t mode)

    | l, `Start ({name = "button"} as t) ->
      (fun mode' ->
        if Stack.in_scope open_elements "button" then
          report l (`Misnested_tag ("button", "button")) !throw (fun () ->
          close_element_with_implied "button" l mode')
        else mode' ())
      (fun () ->
        frameset_ok := false;
        reconstruct_active_formatting_elements (fun () ->
        push_and_emit l t mode))

    | l, `End {name =
        "address" | "article" | "aside" | "blockquote" | "button" |
        "center" | "details" | "dialog" | "dir" | "div" | "dl" | "fieldset" |
        "figcaption" | "figure" | "footer" | "header" | "hgroup" | "listing" |
        "main" | "nav" | "ol" | "pre" | "section" | "summary" | "ul"
        as name} ->
      if not @@ Stack.in_scope open_elements name then
        report l (`Unmatched_end_tag name) !throw mode
      else
        close_element_with_implied name l mode

    | l, `End {name = "form"} ->
      if not @@ Stack.has open_elements "template" then begin
        let form_element = !form_element_pointer in
        form_element_pointer := None;
        match form_element with
        | Some element when Stack.target_in_scope open_elements element ->
          pop_implied l (fun () ->
          match Stack.current_element open_elements with
          | Some element' when element' == element ->
            pop l mode
          | _ ->
            report element.location (`Unmatched_start_tag "form") !throw
              (fun () ->
            pop_until (fun element' -> element' == element) l (fun () ->
            pop l mode)))
        | _ ->
          report l (`Unmatched_end_tag "form") !throw mode
      end
      else
        if not @@ Stack.in_scope open_elements "form" then
          report l (`Unmatched_end_tag "form") !throw mode
        else
          close_element_with_implied "form" l mode

    | l, `End {name = "p"} ->
      (fun mode' ->
        if not @@ Stack.in_button_scope open_elements "p" then
          report l (`Unmatched_end_tag "p") !throw (fun () ->
          push_implicit l "p" mode')
        else mode' ())
      (fun () -> close_element_with_implied "p" l mode)

    | l, `End {name = "li"} ->
      if not @@ Stack.in_list_item_scope open_elements "li" then
        report l (`Unmatched_end_tag "li") !throw mode
      else
        close_element_with_implied "li" l mode

    | l, `End {name = "dd" | "dt" as name} ->
      if not @@ Stack.in_scope open_elements name then
        report l (`Unmatched_end_tag name) !throw mode
      else
        close_element_with_implied name l mode

    | l, `End {name = "h1" | "h2" | "h3" | "h4" | "h5" | "h6" as name} ->
      if not @@ Stack.one_in_scope open_elements
          ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"] then
        report l (`Unmatched_end_tag name) !throw mode
      else
        pop_implied l (fun () ->
          (fun next ->
            match Stack.current_element open_elements with
            | Some {element_name = `HTML, name'}
                when list_mem_string
                  name' ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"] ->
              next ()
            | _ ->
              report l (`Unmatched_end_tag name) !throw next)
          @@ (fun () ->
            pop_until_and_raise_errors
              ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"] l mode))

    | l, `Start ({name = "a"} as t) ->
      (fun k ->
        match Active.has_before_marker active_formatting_elements "a" with
        | None -> k ()
        | Some existing ->
          report l (`Misnested_tag ("a", "a")) !throw (fun () ->
          adoption_agency_algorithm l "a" (fun () ->
          Stack.remove open_elements existing;
          Active.remove active_formatting_elements existing;
          k ())))
      (fun () ->
        Subtree.enable subtree_buffer;
        reconstruct_active_formatting_elements (fun () ->
        push_and_emit ~formatting:true l t mode))

    | l, `Start ({name =
        "b" | "big" | "code" | "em" | "font" | "i" | "s" | "small" |
        "strike" | "strong" | "tt" | "u"} as t) ->
      Subtree.enable subtree_buffer;
      reconstruct_active_formatting_elements (fun () ->
      push_and_emit ~formatting:true l t mode)

    | l, `Start ({name = "nobr"} as t) ->
      Subtree.enable subtree_buffer;
      reconstruct_active_formatting_elements (fun () ->
      (fun k ->
        if not @@ Stack.in_scope open_elements "nobr" then k ()
        else
          report l (`Misnested_tag ("nobr", "nobr")) !throw (fun () ->
          adoption_agency_algorithm l "nobr" (fun () ->
          reconstruct_active_formatting_elements k)))
      (fun () -> push_and_emit ~formatting:true l t mode))

    | l, `End {name =
        "a" | "b" | "big" | "code" | "em" | "font" | "i" | "nobr" | "s" |
        "small" | "strike" | "strong" | "tt" | "u" as name} ->
      adoption_agency_algorithm l name mode

    | l, `Start ({name = "applet" | "marquee" | "object"} as t) ->
      frameset_ok := false;
      reconstruct_active_formatting_elements (fun () ->
      Active.add_marker active_formatting_elements;
      push_and_emit l t mode)

    | l, `End {name = "applet" | "marquee" | "object" as name} ->
      if not @@ Stack.in_scope open_elements name then
        report l (`Unmatched_end_tag name) !throw mode
      else begin
        Active.clear_until_marker active_formatting_elements;
        close_element_with_implied name l mode
      end

    | l, `Start ({name = "table"} as t) ->
      frameset_ok := false;
      close_current_p_element l (fun () ->
      push_and_emit l t in_table_mode)

    | l, `End {name = "br"} ->
      report l (`Unmatched_end_tag "br") !throw (fun () ->
      in_body_mode_rules context_name mode
        (l, `Start
          {Token_tag.name = "br"; attributes = []; self_closing = false}))

    | l, `Start ({name =
        "area" | "br" | "embed" | "img" | "keygen" | "wbr"} as t) ->
      frameset_ok := false;
      reconstruct_active_formatting_elements (fun () ->
      push_and_emit ~acknowledge:true l t (fun () ->
      pop l mode))

    | l, `Start ({name = "input"} as t) ->
      if Element.is_not_hidden t then frameset_ok := false;
      reconstruct_active_formatting_elements (fun () ->
      push_and_emit ~acknowledge:true l t (fun () ->
      pop l mode))

    | l, `Start ({name = "param" | "source" | "track"} as t) ->
      push_and_emit ~acknowledge:true l t (fun () ->
      pop l mode)

    | l, `Start ({name = "hr"} as t) ->
      frameset_ok := false;
      close_current_p_element l (fun () ->
      push_and_emit ~acknowledge:true l t (fun () ->
      pop l mode))

    | l, `Start ({name = "image"} as t) ->
      report l (`Bad_token ("image", "tag", "should be 'img'")) !throw
        (fun () ->
      push tokens (l, `Start {t with name = "img"});
      mode ())

    | l, `Start ({name = "textarea"} as t) ->
      frameset_ok := false;
      push_and_emit l t (fun () ->
      set_tokenizer_state `RCDATA;
      next_expected tokens !throw (function
        | _, `Char 0x000A -> text_mode mode
        | v ->
          push tokens v;
          text_mode mode))

    | l, `Start {name = "xmp"} ->
      frameset_ok := false;
      close_current_p_element l (fun () ->
      reconstruct_active_formatting_elements (fun () ->
      parse_rawtext mode))

    | l, `Start ({name = "iframe"} as t) ->
      frameset_ok := false;
      push_and_emit l t (fun () ->
      parse_rawtext mode)

    | l, `Start ({name = "noembed"} as t) ->
      push_and_emit l t (fun () ->
      parse_rawtext mode)

    | l, `Start ({name = "select"} as t) ->
      frameset_ok := false;
      select_in_body l t in_select_mode

    | l, `Start ({name = "optgroup" | "option"} as t) ->
      (fun mode' ->
        if Stack.current_element_is open_elements ["option"] then
          pop l mode'
        else mode' ())
      (fun () ->
        reconstruct_active_formatting_elements (fun () ->
        push_and_emit l t mode))

    | l, `Start ({name = "rb" | "rp" | "rtc" as name} as t) ->
      (fun mode' ->
        if Stack.in_scope open_elements "ruby" then
          pop_implied l (fun () ->
          if Stack.current_element_is open_elements ["ruby"] then
            mode' ()
          else
            report l (`Misnested_tag (name, context_name)) !throw mode'))
      (fun () ->
        push_and_emit l t mode)

    | l, `Start ({name = "rt"} as t) ->
      (fun mode' ->
        if Stack.in_scope open_elements "ruby" then
          pop_implied ~except:"rtc" l (fun () ->
          if Stack.current_element_is open_elements ["ruby"; "rtc"] then
            mode' ()
          else
            report l (`Misnested_tag ("rt", context_name)) !throw mode'))
      (fun () ->
        push_and_emit l t mode)

    | l, `Start ({name = "math"} as t) ->
      reconstruct_active_formatting_elements (fun () ->
      push_and_emit ~acknowledge:true ~namespace:`MathML l t (fun () ->
      if t.self_closing then pop l mode
      else mode ()))

    | l, `Start ({name = "svg"} as t) ->
      reconstruct_active_formatting_elements (fun () ->
      push_and_emit ~acknowledge:true ~namespace:`SVG l t (fun () ->
      if t.self_closing then pop l mode
      else mode ()))

    | l, `Start {name =
        "caption" | "col" | "colgroup" | "frame" | "head" | "tbody" | "td" |
        "tfoot" | "th" | "thead" | "tr" as name} ->
      report l (`Misnested_tag (name, context_name)) !throw mode

    | l, `Start t ->
      reconstruct_active_formatting_elements (fun () ->
      push_and_emit l t mode)

    | l, `End {name} ->
      any_other_end_tag_in_body l name mode

  (* Part of 8.2.5.4.7. *)
  and any_other_end_tag_in_body l name mode =
    let rec close = function
      | [] -> mode ()
      | {element_name = (ns, name') as name''}::rest ->
        if ns = `HTML && name' = name then
          pop_implied ~except:name l (fun () ->
          pop l mode)
        else
          if Element.is_special name'' then
            report l (`Unmatched_end_tag name) !throw mode
          else close rest
    in
    close !open_elements

  (* Part of 8.2.5.4.7. *)
  and adoption_agency_algorithm l name mode =
    Subtree.enable subtree_buffer;
    emit_text (fun () ->
    let handled, errors =
      Subtree.adoption_agency_algorithm
        subtree_buffer active_formatting_elements l name
    in
    let rec report_all errors k =
      match errors with
      | [] -> k ()
      | (l, error)::more ->
        report l error !throw (fun () -> report_all more k)
    in
    report_all errors (fun () ->
    if not handled then any_other_end_tag_in_body l name mode
    else mode ()))

  (* Part of 8.2.5.4.7. *)
  and select_in_body l t next_mode =
    frameset_ok := false;
    reconstruct_active_formatting_elements (fun () ->
    push_and_emit l t next_mode)

  (* 8.2.5.4.8. *)
  and text_mode original_mode =
    dispatch tokens begin function
      | l, `Char c ->
        add_character l c;
        text_mode original_mode

      | l, `EOF as v ->
        report l (`Unexpected_eoi "content") !throw (fun () ->
        push tokens v;
        pop l original_mode)

      | l, `End _ ->
        pop l original_mode

      | _ ->
        text_mode original_mode
    end

  (* 8.2.5.2. *)
  and parse_rcdata original_mode =
    set_tokenizer_state `RCDATA;
    text_mode original_mode

  (* 8.2.5.2. *)
  and parse_rawtext original_mode =
    set_tokenizer_state `RAWTEXT;
    text_mode original_mode

  and anything_else_in_table mode (l, _ as v) =
    report l (`Bad_content "table") !throw (fun () ->
    in_body_mode_rules "table" mode v)

  (* 8.2.5.4.9. *)
  and in_table_mode () =
    dispatch tokens (fun v -> in_table_mode_rules in_table_mode v)

  and in_table_mode_rules mode = function
    | _, `Char _ as v
        when Stack.current_element_is open_elements
               ["table"; "tbody"; "tfoot"; "thead"; "tr"] ->
      push tokens v;
      in_table_text_mode true [] mode

    | l, `Comment s ->
      emit l (`Comment s) mode

    | l, `Doctype _ ->
      report l (`Bad_document "doctype should be first") !throw mode

    | l, `Start ({name = "caption"} as t) ->
      pop_to_table_context l (fun () ->
      Active.add_marker active_formatting_elements;
      push_and_emit l t in_caption_mode)

    | l, `Start ({name = "colgroup"} as t) ->
      pop_to_table_context l (fun () ->
      push_and_emit l t in_column_group_mode)

    | l, `Start {name = "col"} as v ->
      pop_to_table_context l (fun () ->
      push tokens v;
      push_implicit l "colgroup" in_column_group_mode)

    | l, `Start ({name = "tbody" | "tfoot" | "thead"} as t) ->
      pop_to_table_context l (fun () ->
      push_and_emit l t in_table_body_mode)

    | l, `Start {name = "td" | "th" | "tr"} as v ->
      pop_to_table_context l (fun () ->
      push tokens v;
      push_implicit l "tbody" in_table_body_mode)

    | l, `Start {name = "table"} as v ->
      report l (`Misnested_tag ("table", "table")) !throw (fun () ->
      if not @@ Stack.has open_elements "table" then mode ()
      else begin
        push tokens v;
        close_element l "table" (fun () -> reset_mode () ())
      end)

    | l, `End {name = "table"} ->
      if not @@ Stack.in_table_scope open_elements "table" then
        report l (`Unmatched_end_tag "table") !throw mode
      else
        close_element l "table" (fun () -> reset_mode () ())

    | l, `End {name =
      "body" | "caption" | "col" | "colgroup" | "html" | "tbody" | "td" |
      "tfoot" | "th" | "thead" | "tr" as name} ->
      report l (`Unmatched_end_tag name) !throw mode

    | _, `Start {name = "style" | "script" | "template"}
    | _, `End {name = "template"} as v ->
      in_head_mode_rules mode v

    | l, `Start ({name = "input"} as t) when Element.is_not_hidden t ->
      report l (`Misnested_tag ("input", "table")) !throw (fun () ->
      push_and_emit ~acknowledge:true l t (fun () ->
      pop l mode))

    | l, `Start ({name = "form"} as t) ->
      report l (`Misnested_tag ("form", "table")) !throw (fun () ->
      push_and_emit l t (fun () ->
      pop l mode))

    | _, `EOF as v ->
      in_body_mode_rules "table" mode v

    | v ->
      anything_else_in_table mode v

  (* 8.2.5.4.10. *)
  and in_table_text_mode only_space cs mode =
    dispatch tokens begin function
      | l, `Char 0 ->
        report l (`Bad_token ("U+0000", "table", "null")) !throw (fun () ->
        in_table_text_mode only_space cs mode)

      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) as v ->
        in_table_text_mode only_space (v::cs) mode

      | _, `Char _ as v ->
        in_table_text_mode false (v::cs) mode

      | v ->
        push tokens v;
        if not only_space then
          let rec reprocess = function
            | [] -> mode ()
            | v::more -> anything_else_in_table (fun () -> reprocess more) v
          in
          reprocess (List.rev cs)
        else begin
          List.rev cs |> List.iter (function
            | l, `Char c -> add_character l c
            | _ -> ());
          mode ()
        end
    end

  (* 8.2.5.4.11. *)
  and in_caption_mode () =
    dispatch tokens begin function
      | l, `End {name = "caption"} ->
        if not @@ Stack.in_table_scope open_elements "caption" then
          report l (`Unmatched_end_tag "caption") !throw in_caption_mode
        else begin
          Active.clear_until_marker active_formatting_elements;
          close_element_with_implied "caption" l in_table_mode
        end

      | l, `Start {name =
          "caption" | "col" | "colgroup" | "tbody" | "td" | "tfoot" | "th" |
          "thead" | "tr" as name} as v ->
        report l (`Misnested_tag (name, "caption")) !throw (fun () ->
        if not @@ Stack.in_table_scope open_elements "caption" then
          in_caption_mode ()
        else begin
          Active.clear_until_marker active_formatting_elements;
          push tokens v;
          close_element l "caption" in_table_mode
        end)

      | l, `End {name = "table"} as v ->
        report l (`Unmatched_end_tag "table") !throw (fun () ->
        if not @@ Stack.in_table_scope open_elements "caption" then
          in_caption_mode ()
        else begin
          Active.clear_until_marker active_formatting_elements;
          push tokens v;
          close_element l "caption" in_table_mode
        end)

      | l, `End {name =
          ("body" | "col" | "colgroup" | "html" | "tbody" | "td" | "tfoot" |
           "th" | "thead" | "tr") as name} ->
        report l (`Unmatched_end_tag name) !throw in_caption_mode

      | l, `Start ({name = "select"} as t) ->
        select_in_body l t in_select_in_table_mode

      | v ->
        in_body_mode_rules "caption" in_caption_mode v
    end

  (* 8.2.5.4.12. *)
  and in_column_group_mode () =
    dispatch tokens begin function
      | l, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
        add_character l c;
        in_column_group_mode ()

      | l, `Comment s ->
        emit l (`Comment s) in_column_group_mode

      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          in_column_group_mode

      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "colgroup" in_column_group_mode v

      | l, `Start ({name = "col"} as t) ->
        push_and_emit ~acknowledge:true l t (fun () ->
        pop l in_column_group_mode)

      | l, `End {name = "colgroup"} ->
        if not @@ Stack.current_element_is open_elements ["colgroup"] then
          report l (`Unmatched_end_tag "colgroup") !throw in_column_group_mode
        else
          pop l in_table_mode

      | l, `End {name = "col"} ->
        report l (`Unmatched_end_tag "col") !throw in_column_group_mode

      | _, `Start {name = "template"}
      | _, `End {name = "template"} as v ->
        in_head_mode_rules in_column_group_mode v

      | _, `EOF as v ->
        in_body_mode_rules "colgroup" in_column_group_mode v

      | l, _ as v ->
        if not @@ Stack.current_element_is open_elements ["colgroup"] then
          report l (`Bad_content "colgroup") !throw in_table_mode
        else begin
          push tokens v;
          pop l in_table_mode
        end
    end

  (* 8.2.5.4.13. *)
  and in_table_body_mode () =
    dispatch tokens begin function
      | l, `Start ({name = "tr"} as t) ->
        pop_to_table_body_context l (fun () ->
        push_and_emit l t in_row_mode)

      | l, `Start {name = ("th" | "td") as name} as v ->
        report l (`Misnested_tag (name, "table")) !throw (fun () ->
        pop_to_table_body_context l (fun () ->
        push tokens v;
        push_implicit l "tr" in_row_mode))

      | l, `End {name = "tbody" | "tfoot" | "thead" as name} ->
        if not @@ Stack.in_table_scope open_elements name then
          report l (`Unmatched_end_tag name) !throw in_table_body_mode
        else
          pop_to_table_body_context l (fun () ->
          pop l in_table_mode)

      | l, `Start {name =
          "caption" | "col" | "colgroup" | "tbody" | "tfoot" | "thead"
          as name} as v ->
        if not @@ Stack.one_in_table_scope open_elements
            ["tbody"; "thead"; "tfoot"] then
          report l (`Misnested_tag (name, "table")) !throw in_table_body_mode
        else begin
          push tokens v;
          pop_to_table_body_context l (fun () ->
          pop l in_table_mode)
        end

      | l, `End {name = "table" as name} as v ->
        if not @@ Stack.one_in_table_scope open_elements
            ["tbody"; "thead"; "tfoot"] then
          report l (`Unmatched_end_tag name) !throw in_table_body_mode
        else begin
          push tokens v;
          pop_to_table_body_context l (fun () ->
          pop l in_table_mode)
        end

      | l, `End {name =
          "body" | "caption" | "col" | "colgroup" | "html" | "td" | "th" |
          "tr" as name} ->
        report l (`Unmatched_end_tag name) !throw in_table_body_mode

      | v ->
        in_table_mode_rules in_table_body_mode v
    end

  (* 8.2.5.4.14. *)
  and in_row_mode () =
    dispatch tokens begin function
      | l, `Start ({name = "th" | "td"} as t) ->
        Active.add_marker active_formatting_elements;
        pop_to_table_row_context l (fun () ->
        push_and_emit l t in_cell_mode)

      | l, `End {name = "tr"} ->
        if not @@ Stack.in_table_scope open_elements "tr" then
          report l (`Unmatched_end_tag "tr") !throw in_row_mode
        else
          pop_to_table_row_context l (fun () ->
          pop l in_table_body_mode)

      | l, `Start {name =
          ("caption" | "col" | "colgroup" | "tbody" | "tfoot" | "thead" |
           "tr")}
      | l, `End {name = "table"} as v ->
        if not @@ Stack.in_table_scope open_elements "tr" then
          match snd v with
          | `Start {name} ->
            report l (`Misnested_tag (name, "tr")) !throw in_row_mode
          | `End {name} ->
            report l (`Unmatched_end_tag name) !throw in_row_mode
        else
          pop_to_table_row_context l (fun () ->
          push tokens v;
          pop l in_table_body_mode)

      | l, `End {name = "tbody" | "tfoot" | "thead" as name} as v ->
        if not @@ Stack.in_table_scope open_elements name then
          report l (`Unmatched_end_tag name) !throw in_row_mode
        else
          if not @@ Stack.in_table_scope open_elements "tr" then in_row_mode ()
          else
            pop_to_table_row_context l (fun () ->
            push tokens v;
            pop l in_table_body_mode)

      | l, `End {name =
          "body" | "caption" | "col" | "colgroup" | "html" | "td" | "th"
          as name} ->
        report l (`Unmatched_end_tag name) !throw in_row_mode

      | v ->
        in_table_mode_rules in_row_mode v
    end

  (* 8.2.5.4.15. *)
  and in_cell_mode () =
    dispatch tokens begin function
      | l, `End {name = "td" | "th" as name} ->
        if not @@ Stack.in_table_scope open_elements name then
          report l (`Unmatched_end_tag name) !throw in_cell_mode
        else
          close_element_with_implied name l (fun () ->
          Active.clear_until_marker active_formatting_elements;
          in_row_mode ())

      | l, `Start {name =
          "caption" | "col" | "colgroup" | "tbody" | "td" | "tfoot" | "th" |
          "thead" | "tr" as name} as v ->
        if not @@ Stack.one_in_table_scope open_elements ["td"; "th"] then
          report l (`Misnested_tag (name, "td/th")) !throw in_cell_mode
        else
          close_cell l (fun () ->
          Active.clear_until_marker active_formatting_elements;
          push tokens v;
          in_row_mode ())

      | l, `End {name =
          "body" | "caption" | "col" | "colgroup" | "html" as name} ->
        report l (`Unmatched_end_tag name) !throw in_cell_mode

      | l, `End {name =
          "table" | "tbody" | "tfoot" | "thead" | "tr" as name} as v ->
        if not @@ Stack.in_table_scope open_elements name then
          report l (`Unmatched_end_tag name) !throw in_cell_mode
        else
          close_cell l (fun () ->
          Active.clear_until_marker active_formatting_elements;
          push tokens v;
          in_row_mode ())

      | l, `Start ({name = "select"} as t) ->
        select_in_body l t in_select_in_table_mode

      | v ->
        in_body_mode_rules "td" in_cell_mode v
    end

  (* 8.2.5.4.16. *)
  and in_select_mode () =
    dispatch tokens (fun v -> in_select_mode_rules in_select_mode v)

  and in_select_mode_rules mode = function
    | l, `Char 0 ->
      report l (`Bad_token ("U+0000", "select", "null")) !throw mode

    | l, `Char c ->
      add_character l c;
      mode ()

    | l, `Comment s ->
      emit l (`Comment s) mode

    | l, `Doctype _ ->
      report l (`Bad_document "doctype should be first") !throw mode

    | _, `Start {name = "html"} as v ->
      in_body_mode_rules "select" mode v

    | l, `Start ({name = "option"} as t) ->
      (fun mode' ->
        if Stack.current_element_is open_elements ["option"] then pop l mode'
        else mode' ())
      (fun () -> push_and_emit l t mode)

    | l, `Start ({name = "optgroup"} as t) ->
      (fun mode' ->
        if Stack.current_element_is open_elements ["option"] then pop l mode'
        else mode' ())
      @@ (fun mode' () ->
        if Stack.current_element_is open_elements ["optgroup"] then pop l mode'
        else mode' ())
      @@ (fun () -> push_and_emit l t mode)

    | l, `End {name = "optgroup"} ->
      (fun mode' ->
        match !open_elements with
        | {element_name = `HTML, "option"}::
            {element_name = `HTML, "optgroup"}::_ ->
          pop l mode'
        | _ -> mode' ())
      (fun () ->
        if Stack.current_element_is open_elements ["optgroup"] then
          pop l mode
        else
          report l (`Unmatched_end_tag "optgroup") !throw mode)

    | l, `End {name = "option"} ->
      if Stack.current_element_is open_elements ["option"] then
        pop l mode
      else
        report l (`Unmatched_end_tag "option") !throw mode

    | l, `End {name = "select"} ->
      if not @@ Stack.in_select_scope open_elements "select" then
        report l (`Unmatched_end_tag "select") !throw mode
      else
        close_element l "select" (fun () -> reset_mode () ())

    | l, `Start {name = "select"} ->
      report l (`Misnested_tag ("select", "select")) !throw (fun () ->
      close_element l "select" (fun () -> reset_mode () ()))

    | l, `Start {name = "input" | "keygen" | "textarea" as name} as v ->
      report l (`Misnested_tag (name, "select")) !throw (fun () ->
      if not @@ Stack.in_select_scope open_elements "select" then
        mode ()
      else begin
        push tokens v;
        close_element l "select" (fun () -> reset_mode () ())
      end)

    | _, (`Start {name = "script" | "template"} |
          `End {name = "template"}) as v ->
      in_head_mode_rules mode v

    | _, `EOF as v ->
      in_body_mode_rules "select" mode v

    | l, _ ->
      report l (`Bad_content "select") !throw mode

  (* 8.2.5.4.17. *)
  and in_select_in_table_mode () =
    dispatch tokens begin function
      | l, `Start {name =
          "caption" | "table" | "tbody" | "tfoot" | "thead" | "tr" | "td" |
          "th" as name} as v ->
        report l (`Misnested_tag (name, "table")) !throw (fun () ->
        push tokens v;
        close_element l "select" (fun () -> reset_mode () ()))

      | l, `End {name =
          "caption" | "table" | "tbody" | "tfoot" | "thead" | "tr" | "td" |
          "th" as name} as v ->
        report l (`Unmatched_end_tag "name") !throw (fun () ->
        if not @@ Stack.in_table_scope open_elements name then
          in_select_in_table_mode ()
        else begin
          push tokens v;
          close_element l "select" (fun () -> reset_mode () ())
        end)

      | v ->
        in_select_mode_rules in_select_in_table_mode v
    end

  (* 8.2.5.4.18. *)
  and in_template_mode () =
    dispatch tokens (fun v -> in_table_mode_rules in_template_mode v)

  (* 8.2.5.4.18. *)
  and in_template_mode_rules mode = function
    | _, (`Char _ | `Comment _ | `Doctype _) as v ->
      in_body_mode_rules "template" mode v

    | _, `Start {name =
        "base" | "basefont" | "bgsound" | "link" | "meta" | "noframes" |
        "script" | "style" | "template" | "title"}
    | _, `End {name = "template"} as v ->
      in_head_mode_rules mode v

    | _, `Start {name =
        "caption" | "colgroup" | "tbody" | "tfoot" | "thead"} as v ->
      Template.pop template_insertion_modes;
      Template.push template_insertion_modes in_table_mode;
      push tokens v;
      in_table_mode ()

    | _, `Start {name = "col"} as v ->
      Template.pop template_insertion_modes;
      Template.push template_insertion_modes in_column_group_mode;
      push tokens v;
      in_column_group_mode ()

    | _, `Start {name = "tr"} as v ->
      Template.pop template_insertion_modes;
      Template.push template_insertion_modes in_table_body_mode;
      push tokens v;
      in_table_body_mode ()

    | _, `Start {name = "td" | "th"} as v ->
      Template.pop template_insertion_modes;
      Template.push template_insertion_modes in_row_mode;
      push tokens v;
      in_row_mode ()

    | _, `Start _ as v ->
      Template.pop template_insertion_modes;
      Template.push template_insertion_modes in_body_mode;
      push tokens v;
      in_body_mode ()

    | l, `End {name} ->
      report l (`Unmatched_end_tag name) !throw mode

    | l, `EOF as v ->
      if not @@ Stack.has open_elements "template" then emit_end l
      else begin
        report l (`Unmatched_end_tag "template") !throw (fun () ->
        Active.clear_until_marker active_formatting_elements;
        Template.pop template_insertion_modes;
        push tokens v;
        close_element l "template" (fun () -> reset_mode () ()))
      end

  (* 8.2.5.4.19. *)
  and after_body_mode () =
    dispatch tokens begin function
      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) as v ->
        in_body_mode_rules "html" after_body_mode v

      | l, `Comment s ->
        emit l (`Comment s) after_body_mode

      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          after_body_mode

      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "html" after_body_mode v

      | _, `End {name = "html"} ->
        after_after_body_mode ()

      | l, `EOF ->
        emit_end l

      | l, _ as v ->
        report l (`Bad_document "content after body") !throw (fun () ->
        push tokens v;
        in_body_mode ())
    end

  (* 8.2.5.4.20. *)
  and in_frameset_mode () =
    dispatch tokens begin function
      | l, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
        add_character l c;
        in_frameset_mode ()

      | l, `Comment s ->
        emit l (`Comment s) in_frameset_mode

      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          in_frameset_mode

      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "frameset" in_frameset_mode v

      | l, `Start ({name = "frameset"} as t) ->
        push_and_emit l t in_frameset_mode

      | l, `End {name = "frameset"} ->
        (fun mode' ->
          if Stack.current_element_is open_elements ["html"] then
            report l (`Unmatched_end_tag "frameset") !throw mode'
          else
            pop l mode')
        (fun () ->
          if Stack.current_element_is open_elements ["frameset"] then
            in_frameset_mode ()
          else after_frameset_mode ())

      | l, `Start ({name = "frame"} as t) ->
        push_and_emit ~acknowledge:true l t (fun () ->
        pop l in_frameset_mode)

      | _, `Start {name = "noframes"} as v ->
        in_head_mode_rules in_frameset_mode v

      | l, `EOF ->
        (fun mode' ->
          if not @@ Stack.current_element_is open_elements ["html"] then
            report l (`Unexpected_eoi "frameset") !throw mode'
          else mode' ())
        (fun () -> emit_end l)

      | l, _ ->
        report l (`Bad_content "frameset") !throw in_frameset_mode
    end

  (* 8.2.5.4.21. *)
  and after_frameset_mode () =
    dispatch tokens begin function
      | l, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
        add_character l c;
        after_frameset_mode ()

      | l, `Comment s ->
        emit l (`Comment s) after_frameset_mode

      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          after_frameset_mode

      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "html" after_frameset_mode v

      | l, `End {name = "html"} ->
        close_element l "html" after_after_frameset_mode

      | _, `Start {name = "noframes"} as v ->
        in_head_mode_rules after_frameset_mode v

      | l, `EOF ->
        emit_end l

      | l, _ ->
        report l (`Bad_content "html") !throw after_frameset_mode
    end

  (* 8.2.5.4.22. *)
  and after_after_body_mode () =
    dispatch tokens begin function
      | l, `Comment s ->
        emit l (`Comment s) after_after_body_mode

      | _, `Doctype _
      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020)
      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "html" after_after_body_mode v

      | l, `EOF ->
        emit_end l

      | l, _ as v ->
        push tokens v;
        report l (`Bad_content "html") !throw in_body_mode
    end

  (* 8.2.5.4.23. *)
  and after_after_frameset_mode () =
    dispatch tokens begin function
      | l, `Comment s ->
        emit l (`Comment s) after_after_frameset_mode

      | _, `Doctype _
      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020)
      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "html" after_after_frameset_mode v

      | l, `EOF ->
        emit_end l

      | _, `Start {name = "noframes"} as v ->
        in_head_mode_rules after_after_frameset_mode v

      | l, _ ->
        report l (`Bad_content "html") !throw after_after_frameset_mode
    end

  (* 8.2.5.5. *)
  and foreign_start_tag mode l tag =
    let namespace =
      match Stack.adjusted_current_element context open_elements with
      | None -> `HTML
      | Some {element_name = ns, _} -> ns
    in

    push_and_emit ~acknowledge:true ~namespace l tag (fun () ->
    if tag.self_closing then pop l mode
    else mode ())

  and is_html_font_tag tag =
    tag.Token_tag.attributes |> List.exists (function
      | ("color" | "face" | "size"), _ -> true
      | _ -> false)

  and foreign_content mode force_html v =
    match v with
    | l, `Char 0 ->
      report l (`Bad_token ("U+0000", "foreign content", "null")) !throw
        (fun () ->
      add_character l u_rep;
      mode ())

    | l, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
      add_character l c;
      mode ()

    | l, `Char c ->
      frameset_ok := false;
      add_character l c;
      mode ()

    | l, `Comment s ->
      emit l (`Comment s) mode

    | l, `Doctype _ ->
      report l (`Bad_document "doctype should be first") !throw mode

    | l, `Start ({name =
        "b" | "big" | "blockquote" | "body" | "br" | "center" | "code" |
        "dd" | "div" | "dl" | "dt" | "em" | "embed" | "font" | "h1" | "h2" |
        "h3" | "h4" | "h5" | "h6" | "head" | "hr" | "i" | "img" | "li" |
        "listing" | "main" | "meta" | "nobr" | "ol" | "p" | "pre" | "ruby" |
        "s" | "small" | "span" | "strong" | "strike" | "sub" | "sup" |
        "table" | "tt" | "u" | "ul" | "var" as name} as t) as v ->
      if name = "font" && not @@ is_html_font_tag t then
        foreign_start_tag mode l t
      else
        report l (`Misnested_tag (name, "xml tag")) !throw (fun () ->
        push tokens v;
        pop l (fun () ->
        pop_until (function
          | {element_name = `HTML, _} -> true
          | {is_html_integration_point = true} -> true
          | {element_name} ->
            Foreign.is_mathml_text_integration_point element_name)
          l mode))

    | l, `Start t ->
      foreign_start_tag mode l t

    | l, `End {name = "script"}
        when
          match Stack.current_element open_elements with
          | Some {element_name = `SVG, "script"} -> true
          | _ -> false ->
      pop l mode

    | l, `End {name} ->
      (fun mode' ->
        match Stack.current_element open_elements with
        | Some {element_name = _, name'} when String.lowercase name' = name ->
          mode' ()
        | _ ->
          report l (`Unmatched_end_tag name) !throw (fun () ->
          mode' ()))
      (fun () ->
        let rec scan = function
          | [] -> mode ()
          | {element_name = ns, name'}::_
              when String.lowercase name' = name ->
            close_element ~ns l name mode
          | {element_name = `HTML, _}::_ -> force_html ()
          | _::rest -> scan rest
        in
        scan !open_elements)

    | _, `EOF -> force_html ()

  in

  construct constructor
