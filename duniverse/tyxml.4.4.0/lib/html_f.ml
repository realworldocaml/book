(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2004 by Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
 * Copyright (C) 2007 by Vincent Balat, Gabriel Kerneis
 * Copyright (C) 2010 by Cecile Herbelin
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Suite 500, Boston, MA 02110-1301, USA.
*)

module Make_with_wrapped_functions

    (Xml : Xml_sigs.T)
    (C : Html_sigs.Wrapped_functions with module Xml = Xml)
    (Svg : Svg_sigs.T with module Xml := Xml) =

struct

  module Xml = Xml

  module W = Xml.W

  module Info = struct
    let content_type = "text/html"
    let alternative_content_types = ["application/xhtml+xml";"application/xml";"text/xml"]
    let version = "HTML5-draft"
    let standard = "http://www.w3.org/TR/html5/"
    let namespace = "http://www.w3.org/1999/xhtml"
    let doctype =
      Xml_print.compose_doctype "html" []
    let emptytags =
      [ "area"; "base"; "br"; "col"; "command"; "embed"; "hr"; "img";
        "input"; "keygen"; "link"; "meta"; "param"; "source"; "wbr" ]
  end

  type 'a wrap = 'a W.t
  type 'a list_wrap = 'a W.tlist

  type uri = Xml.uri
  let string_of_uri = Xml.string_of_uri
  let uri_of_string = Xml.uri_of_string

  type image_candidate =
    [ `Url of uri
    | `Url_width of uri * Html_types.number
    | `Url_pixel of uri * Html_types.float_number ]


  type 'a attrib = Xml.attrib

  let to_xmlattribs x = x
  let to_attrib x = x

  (* VB *)
  let float_attrib = Xml.float_attrib

  let int_attrib = Xml.int_attrib

  let string_attrib = Xml.string_attrib

  let uri_attrib a s = Xml.uri_attrib a s

  let space_sep_attrib = Xml.space_sep_attrib

  let comma_sep_attrib = Xml.comma_sep_attrib

  let user_attrib f name v = Xml.string_attrib name (W.fmap f v)

  let bool_attrib = user_attrib C.string_of_bool

  let constant_attrib a () =
    string_attrib a (W.return a)

  let linktypes_attrib name x =
    user_attrib C.string_of_linktypes name x

  let mediadesc_attrib name x =
    user_attrib C.string_of_mediadesc name x

  let srcset_attrib name x =
    user_attrib C.string_of_srcset name x

  (* Core: *)
  let a_class = space_sep_attrib "class"

  let a_id = string_attrib "id"

  let a_user_data name = string_attrib ("data-" ^ name)

  let a_title = string_attrib "title"

  (* I18N: *)
  let a_xml_lang = string_attrib "xml:lang"
  let a_lang = string_attrib "lang"

  (* Style: *)
  let a_style = string_attrib "style"

  let a_property = string_attrib "property"

  (* Events: *)
  let a_onabort = Xml.event_handler_attrib "onabort"
  let a_onafterprint = Xml.event_handler_attrib "onafterprint"
  let a_onbeforeprint = Xml.event_handler_attrib "onbeforeprint"
  let a_onbeforeunload = Xml.event_handler_attrib "onbeforeunload"
  let a_onblur = Xml.event_handler_attrib "onblur"
  let a_oncanplay = Xml.event_handler_attrib "oncanplay"
  let a_oncanplaythrough = Xml.event_handler_attrib "oncanplaythrough"
  let a_onchange = Xml.event_handler_attrib "onchange"
  let a_ondurationchange = Xml.event_handler_attrib "ondurationchange"
  let a_onemptied = Xml.event_handler_attrib "onemptied"
  let a_onended = Xml.event_handler_attrib "onended"
  let a_onerror = Xml.event_handler_attrib "onerror"
  let a_onfocus = Xml.event_handler_attrib "onfocus"
  let a_onformchange = Xml.event_handler_attrib "onformchange"
  let a_onforminput = Xml.event_handler_attrib "onforminput"
  let a_onhashchange = Xml.event_handler_attrib "onhashchange"
  let a_oninput = Xml.event_handler_attrib "oninput"
  let a_oninvalid = Xml.event_handler_attrib "oninvalid"
  let a_onoffline = Xml.event_handler_attrib "onoffline"
  let a_ononline = Xml.event_handler_attrib "ononline"
  let a_onpause = Xml.event_handler_attrib "onpause"
  let a_onplay = Xml.event_handler_attrib "onplay"
  let a_onplaying = Xml.event_handler_attrib "onplaying"
  let a_onpagehide = Xml.event_handler_attrib "onpagehide"
  let a_onpageshow = Xml.event_handler_attrib "onpageshow"
  let a_onpopstate = Xml.event_handler_attrib "onpopstate"
  let a_onprogress = Xml.event_handler_attrib "onprogress"
  let a_onratechange = Xml.event_handler_attrib "onratechange"
  let a_onreadystatechange = Xml.event_handler_attrib "onreadystatechange"
  let a_onredo = Xml.event_handler_attrib "onredo"
  let a_onresize = Xml.event_handler_attrib "onresize"
  let a_onscroll = Xml.event_handler_attrib "onscroll"
  let a_onseeked = Xml.event_handler_attrib "onseeked"
  let a_onseeking = Xml.event_handler_attrib "onseeking"
  let a_onselect = Xml.event_handler_attrib "onselect"
  let a_onshow = Xml.event_handler_attrib "onshow"
  let a_onstalled = Xml.event_handler_attrib "onstalled"
  let a_onstorage = Xml.event_handler_attrib "onstorage"
  let a_onsubmit = Xml.event_handler_attrib "onsubmit"
  let a_onsuspend = Xml.event_handler_attrib "onsuspend"
  let a_ontimeupdate = Xml.event_handler_attrib "ontimeupdate"
  let a_onundo = Xml.event_handler_attrib "onundo"
  let a_onunload = Xml.event_handler_attrib "onunload"
  let a_onvolumechange = Xml.event_handler_attrib "onvolumechange"
  let a_onwaiting = Xml.event_handler_attrib "onwaiting"
  let a_onload = Xml.event_handler_attrib "onload"
  let a_onloadeddata = Xml.event_handler_attrib "onloadeddata"
  let a_onloadedmetadata = Xml.event_handler_attrib "onloadedmetadata"
  let a_onloadstart = Xml.event_handler_attrib "onloadstart"
  let a_onmessage = Xml.event_handler_attrib "onmessage"
  let a_onmousewheel = Xml.event_handler_attrib "onmousewheel"

  (** Javascript mouse events *)
  let a_onclick = Xml.mouse_event_handler_attrib "onclick"
  let a_oncontextmenu = Xml.mouse_event_handler_attrib "oncontextmenu"
  let a_ondblclick = Xml.mouse_event_handler_attrib "ondblclick"
  let a_ondrag = Xml.mouse_event_handler_attrib "ondrag"
  let a_ondragend = Xml.mouse_event_handler_attrib "ondragend"
  let a_ondragenter = Xml.mouse_event_handler_attrib "ondragenter"
  let a_ondragleave = Xml.mouse_event_handler_attrib "ondragleave"
  let a_ondragover = Xml.mouse_event_handler_attrib "ondragover"
  let a_ondragstart = Xml.mouse_event_handler_attrib "ondragstart"
  let a_ondrop = Xml.mouse_event_handler_attrib "ondrop"
  let a_onmousedown = Xml.mouse_event_handler_attrib "onmousedown"
  let a_onmouseup = Xml.mouse_event_handler_attrib "onmouseup"
  let a_onmouseover = Xml.mouse_event_handler_attrib "onmouseover"
  let a_onmousemove = Xml.mouse_event_handler_attrib "onmousemove"
  let a_onmouseout = Xml.mouse_event_handler_attrib "onmouseout"

  (** Javascript touch events *)
  let a_ontouchstart = Xml.touch_event_handler_attrib "ontouchstart"
  let a_ontouchend = Xml.touch_event_handler_attrib "ontouchend"
  let a_ontouchmove = Xml.touch_event_handler_attrib "ontouchmove"
  let a_ontouchcancel = Xml.touch_event_handler_attrib "ontouchcancel"

  (** Javascript keyboard events *)
  let a_onkeypress = Xml.keyboard_event_handler_attrib "onkeypress"
  let a_onkeydown = Xml.keyboard_event_handler_attrib "onkeydown"
  let a_onkeyup = Xml.keyboard_event_handler_attrib "onkeyup"

  (* Other Attributes *)
  let a_version = string_attrib "version"

  let a_xmlns x =
    user_attrib C.string_of_big_variant "xmlns" x

  let a_manifest = uri_attrib "manifest"

  let a_cite = uri_attrib "cite"

  let a_xml_space x =
    user_attrib C.string_of_big_variant "xml:space" x

  let a_accesskey c =
    user_attrib C.string_of_character "accesskey" c

  let a_charset = string_attrib "charset"

  let a_accept_charset = space_sep_attrib "accept-charset"

  let a_accept = comma_sep_attrib "accept"

  let a_href = uri_attrib "href"

  let a_hreflang = string_attrib "hreflang"

  let a_download file =
    user_attrib (C.unoption_string) "download" file

  let a_rel = linktypes_attrib "rel"

  let a_tabindex = int_attrib "tabindex"

  let a_mime_type = string_attrib "type"

  let a_alt = string_attrib "alt"

  let a_height p = int_attrib "height" p

  let a_src = uri_attrib "src"

  let a_width p = int_attrib "width" p

  let a_label_for = string_attrib "for"
  let a_for = a_label_for

  let a_output_for = space_sep_attrib "for"
  let a_for_list = a_output_for

  let a_selected =
    constant_attrib "selected"

  let a_text_value = string_attrib "value"

  let a_int_value = int_attrib "value"

  let a_value = string_attrib "value"

  let a_float_value = float_attrib "value"

  let a_action = uri_attrib "action"

  let a_method x =
    user_attrib C.string_of_big_variant "method" x

  let a_formmethod = a_method

  let a_enctype = string_attrib "enctype"

  let a_checked =
    constant_attrib "checked"

  let a_disabled =
    constant_attrib "disabled"

  let a_readonly =
    constant_attrib "readonly"

  let a_maxlength = int_attrib "maxlength"

  let a_minlength = int_attrib "minlength"

  let a_name = string_attrib "name"

  let a_allowfullscreen =
    constant_attrib "allowfullscreen"

  let a_allowpaymentrequest =
    constant_attrib "allowpaymentrequest"

  let a_referrerpolicy x =
    user_attrib C.string_of_referrerpolicy "referrerpolicy" x

  let a_autocomplete x =
    user_attrib C.onoff_of_bool "autocomplete" x

  let a_async =
    constant_attrib "async"

  let a_autofocus =
    constant_attrib "autofocus"

  let a_autoplay =
    constant_attrib "autoplay"

  let a_muted =
    constant_attrib "muted"

  let a_crossorigin x =
    user_attrib C.string_of_big_variant "crossorigin" x

  let a_integrity = string_attrib "integrity"

  let a_mediagroup = string_attrib "mediagroup"

  let a_challenge = string_attrib "challenge"

  let a_contenteditable ce =
    bool_attrib "contenteditable" ce

  let a_contextmenu = string_attrib "contextmenu"

  let a_controls =
    constant_attrib "controls"

  let a_dir x =
    user_attrib C.string_of_big_variant "dir" x

  let a_draggable d =
    bool_attrib "draggable" d

  let a_form = string_attrib "form"

  let a_formaction = uri_attrib "formaction"

  let a_formenctype = string_attrib "formenctype"

  let a_formnovalidate =
    constant_attrib "formnovalidate"

  let a_formtarget = string_attrib "formtarget"

  let a_hidden =
    constant_attrib "hidden"

  let a_high = float_attrib "high"

  let a_icon = uri_attrib "icon"

  let a_ismap =
    constant_attrib "ismap"

  let a_keytype = string_attrib "keytype"

  let a_list = string_attrib "list"

  let a_loop =
    constant_attrib "loop"

  let a_low = float_attrib "low"

  let a_max = float_attrib "max"

  let a_input_max = user_attrib C.string_of_number_or_datetime "max"

  let a_min = float_attrib "min"

  let a_input_min = user_attrib C.string_of_number_or_datetime "min"

  let a_inputmode x =
    user_attrib C.string_of_big_variant "inputmode" x

  let a_novalidate =
    constant_attrib "novalidate"

  let a_open =
    constant_attrib "open"

  let a_optimum = float_attrib "optimum"

  let a_pattern = string_attrib "pattern"

  let a_placeholder = string_attrib "placeholder"

  let a_poster = uri_attrib "poster"

  let a_preload x =
    user_attrib C.string_of_big_variant "preload" x

  let a_pubdate =
    constant_attrib "pubdate"

  let a_radiogroup = string_attrib "radiogroup"

  let a_required =
    constant_attrib "required"

  let a_reversed =
    constant_attrib "reserved"

  let a_sandbox x =
    user_attrib C.string_of_sandbox "sandbox" x

  let a_spellcheck sc =
    bool_attrib "spellcheck" sc

  let a_scoped =
    constant_attrib "scoped"

  let a_seamless =
    constant_attrib "seamless"

  let a_sizes sizes =
    user_attrib C.string_of_sizes "sizes" sizes

  let a_span = int_attrib "span"

  (*let a_srcdoc*)
  let a_srclang = string_attrib "xml:lang"

  let a_srcset = srcset_attrib "srcset"

  let a_img_sizes = comma_sep_attrib "sizes"

  let a_start = int_attrib "start"

  let a_step step =
    user_attrib C.string_of_step "step" step

  let a_wrap x =
    user_attrib C.string_of_big_variant "wrap" x

  let a_size = int_attrib "size"

  let a_input_type it =
    user_attrib C.string_of_input_type "type" it

  let a_menu_type x =
    user_attrib C.string_of_big_variant "type" x

  let a_command_type x =
    user_attrib C.string_of_big_variant "type" x

  let a_button_type bt =
    user_attrib C.string_of_input_type "type" bt

  let a_multiple =
    constant_attrib "multiple"

  let a_cols = int_attrib "cols"

  let a_rows = int_attrib "rows"

  let a_summary = string_attrib "summary"

  let a_align x =
    user_attrib C.string_of_big_variant "align" x

  let a_axis = string_attrib "axis"

  let a_colspan = int_attrib "colspan"

  let a_headers = space_sep_attrib "headers"

  let a_rowspan = int_attrib "rowspan"

  let a_scope x =
    user_attrib C.string_of_big_variant "scope" x

  let a_border = int_attrib "border"

  let a_rules x =
    user_attrib C.string_of_big_variant "rules" x

  let a_char c =
    user_attrib C.string_of_character "char" c

  let a_data = uri_attrib "data"

  let a_codetype = string_attrib "codetype"

  let a_frameborder x =
    user_attrib C.string_of_big_variant "frameborder" x

  let a_marginheight = int_attrib "marginheight"

  let a_marginwidth = int_attrib "marginwidth"

  let a_scrolling x =
    user_attrib C.string_of_big_variant "scrolling" x

  let a_target = string_attrib "target"

  let a_content = string_attrib "content"

  let a_http_equiv = string_attrib "http-equiv"

  let a_media = mediadesc_attrib "media"

  (* ARIA *)

  let a_role = space_sep_attrib "role"

  let a_aria name = space_sep_attrib ("aria-" ^ name)

  type 'a elt = Xml.elt

  type ('a, 'b) nullary = ?a: (('a attrib) list) -> unit -> 'b elt

  type ('a, 'b, 'c) unary = ?a: (('a attrib) list) -> 'b elt wrap -> 'c elt

  type ('a, 'b, 'c) star =
    ?a: (('a attrib) list) -> ('b elt) list_wrap -> 'c elt

  let terminal tag ?a () = Xml.leaf ?a tag

  let unary tag ?a elt =
    Xml.node ?a tag (W.singleton elt)

  let star tag ?a elts = Xml.node ?a tag elts

  let plus tag ?a elt elts =
    Xml.node ?a tag (W.cons elt elts)

  let option_cons opt elts =
    match opt with
    | None -> elts
    | Some x -> W.cons x elts

  let body = star "body"

  let head = plus "head"

  let title = unary "title"

  let html ?a head body =
    let content = W.cons head (W.singleton body) in
    Xml.node ?a "html" content

  let footer = star "footer"

  let header = star "header"

  let section = star "section"

  let nav = star "nav"

  let txt s = Xml.pcdata s
  let pcdata = txt

  let entity = Xml.entity

  let space () = entity "nbsp"

  let cdata = Xml.cdata

  let cdata_script = Xml.cdata_script

  let cdata_style = Xml.cdata_style

  let h1 = star "h1"

  let h2 = star "h2"

  let h3 = star "h3"

  let h4 = star "h4"

  let h5 = star "h5"

  let h6 = star "h6"

  let hgroup = star "hgroup"

  let address = star "address"

  let blockquote = star "blockquote"

  let div = star "div"

  let p = star "p"

  let pre = star "pre"

  let abbr = star "abbr"

  let br = terminal "br"

  let cite = star "cite"

  let code = star "code"

  let dfn = star "dfn"

  let em = star "em"

  let kbd = star "kbd"

  let q = star "q"

  let samp = star "samp"

  let span = star "span"

  let strong = star "strong"

  let time = star "time"

  let var = star "var"

  let a = star "a"

  let dl = star "dl"

  let ol = star "ol"

  let ul = star "ul"

  let dd = star "dd"

  let dt = star "dt"

  let li = star "li"

  let hr = terminal "hr"

  let b = star "b"

  let i = star "i"

  let u = star "u"

  let small = star "small"

  let sub = star "sub"

  let sup = star "sup"

  let mark = star "mark"

  let rp = star "rp"

  let rt = star "rt"

  let ruby = star "ruby"

  let wbr = terminal "wbr"

  (* VB *)
  type shape = [ | `Rect | `Circle | `Poly | `Default ]

  let bdo ~dir ?(a = []) elts = Xml.node ~a: ((a_dir dir) :: a) "bdo" elts

  let a_datetime = string_attrib "datetime"

  let a_shape x =
    user_attrib C.string_of_big_variant "shape" x

  let a_coords coords =
    user_attrib C.string_of_numbers "coords" coords

  let a_usemap = string_attrib "usemap"

  let a_defer =
    constant_attrib "defer"

  let a_label = string_attrib "label"

  let area ~alt ?(a = []) () = Xml.leaf ~a: ((a_alt alt) :: a) "area"

  let map = star "map"

  let del = star "del"

  let ins = star "ins"

  let script = unary "script"

  let noscript = star "noscript"

  let template = star "template"

  let article = star "article"

  let aside = star "aside"

  let main = star "main"

  let video_audio name ?src ?srcs ?(a = []) elts =
    let a =
      match src with
      | None -> a
      | Some uri -> (a_src uri) :: a
    in
    match srcs with
    | None -> Xml.node name ~a elts
    | Some srcs -> Xml.node name ~a (W.append srcs elts)

  let audio = video_audio "audio"

  let video = video_audio "video"

  let canvas = star "canvas"

  let command ~label ?(a = []) () =
    Xml.leaf ~a: ((a_label label) :: a) "command"

  let menu ?children ?a () =
    let children = match children with
      | None -> W.nil ()
      | Some (`Lis l)
      | Some (`Flows l) -> l in
    Xml.node ?a "menu" children

  let embed = terminal "embed"

  let source = terminal "source"

  let meter = star "meter"

  let output_elt = star "output"

  let form = star "form"

  let svg ?(a = []) children =
    Svg.toelt (Svg.svg ~a children)

  let input = terminal "input"

  let keygen = terminal "keygen"

  let label = star "label"

  let option = unary "option"

  let select = star "select"

  let textarea = unary "textarea"

  let button = star "button"

  let datalist ?children ?a () =
    let children = match children with
      | None -> W.nil ()
      | Some (`Options x | `Phras x) -> x in
    Xml.node ?a "datalist" children

  let progress = star "progress"

  let legend = star "legend"

  let details summary ?a children =
    plus "details" ?a summary children

  let summary = star "summary"

  let fieldset ?legend ?a elts =
    Xml.node ?a "fieldset" (option_cons legend elts)

  let optgroup ~label ?(a = []) elts =
    Xml.node ~a: ((a_label label) :: a) "optgroup" elts

  let figcaption = star "figcaption"
  let figure ?figcaption ?a elts =
    let content = match figcaption with
      | None -> elts
      | Some (`Top c) -> W.cons c elts
      | Some (`Bottom c) -> W.append elts (W.singleton c)
    in
    Xml.node ?a "figure" content

  let caption = star "caption"

  let tablex ?caption ?columns ?thead ?tfoot ?a elts =
    let content = option_cons thead (option_cons tfoot elts) in
    let content = match columns with
      | None -> content
      | Some columns -> W.append columns content in
    let content = option_cons caption content in
    Xml.node ?a "table" content

  let table = tablex

  let td = star "td"

  let th = star "th"

  let tr = star "tr"

  let colgroup = star "colgroup"

  let col = terminal "col"

  let thead = star "thead"

  let tbody = star "tbody"

  let tfoot = star "tfoot"

  let iframe = star "iframe"

  let object_ ?params ?(a = []) elts =
    let elts = match params with
      | None -> elts
      | Some e -> W.append e elts in
    Xml.node ~a "object" elts

  let param = terminal "param"

  let img ~src ~alt ?(a = []) () =
    let a = (a_src src) :: (a_alt alt) :: a in
    Xml.leaf ~a "img"

  let meta = terminal "meta"

  let style ?(a = []) elts = Xml.node ~a "style" elts

  let link ~rel ~href ?(a = []) () =
    Xml.leaf ~a: ((a_rel rel) :: (a_href href) :: a) "link"

  let base = terminal "base"

  (******************************************************************)
  (* Conversion from and to Xml module *)
  let tot x = x

  let totl x = x

  let toelt x = x

  let toeltl x = x

  type doc  = [ `Html ] elt
  let doc_toelt x = x

  module I = Xml_stream.Import(Xml)
  let of_seq s = totl @@ I.of_seq s

  module Unsafe = struct

    let data s = Xml.encodedpcdata s

    let leaf tag ?a () = Xml.leaf ?a tag

    let node tag ?a elts = Xml.node ?a tag elts

    let coerce_elt x = x

    let float_attrib = Xml.float_attrib

    let int_attrib = Xml.int_attrib

    let string_attrib = Xml.string_attrib

    let uri_attrib a s = Xml.uri_attrib a s

    let space_sep_attrib = Xml.space_sep_attrib

    let comma_sep_attrib = Xml.comma_sep_attrib

  end

end

module Wrapped_functions
    (Xml : Xml_sigs.T with type ('a,'b) W.ft = 'a -> 'b) =
struct

  module Xml = Xml

  let string_of_sandbox_token = function
    | `Allow_forms -> "allow-forms"
    | `Allow_pointer_lock -> "allow-pointer-lock"
    | `Allow_popups -> "allow-popups"
    | `Allow_top_navigation -> "allow-top-navigation"
    | `Allow_same_origin -> "allow-same-origin"
    | `Allow_script -> "allow-script"

  let string_of_linktype = function
    | `Alternate -> "alternate"
    | `Archives -> "archives"
    | `Author -> "author"
    | `Bookmark -> "bookmark"
    | `Canonical -> "canonical"
    | `External -> "external"
    | `First -> "first"
    | `Help -> "help"
    | `Icon -> "icon"
    | `Index -> "index"
    | `Last -> "last"
    | `License -> "license"
    | `Next -> "next"
    | `Nofollow -> "nofollow"
    | `Noreferrer -> "noreferrer"
    | `Noopener -> "noopener"
    | `Pingback -> "pingback"
    | `Prefetch -> "prefetch"
    | `Prev -> "prev"
    | `Search -> "search"
    | `Stylesheet -> "stylesheet"
    | `Sidebar -> "sidebar"
    | `Tag -> "tag"
    | `Up -> "up"
    | `Other s -> s

  let string_of_mediadesc_token =
    function
    | `All -> "all"
    | `Aural -> "aural"
    | `Braille -> "braille"
    | `Embossed -> "embossed"
    | `Handheld -> "handheld"
    | `Print -> "print"
    | `Projection -> "projection"
    | `Screen -> "screen"
    | `Speech -> "speech"
    | `Tty -> "tty"
    | `Tv -> "tv"
    | `Raw_mediadesc s -> s

  let string_of_referrerpolicy = function
    | `Empty -> ""
    | `No_referrer -> "no-referrer"
    | `No_referrer_when_downgrade -> "no-referrer-when-downgrade"
    | `Origin -> "origin"
    | `Origin_when_cross_origin -> "origin-when-cross-origin"
    | `Same_origin -> "same-origin"
    | `Strict_origin -> "strict-origin"
    | `Strict_origin_when_cross_origin -> "strict-origin-when-cross-origin"
    | `Unsafe_url -> "unsafe-url"

  let string_of_big_variant = function
    | `Anonymous -> "anonymous"
    | `Async -> "async"
    | `Autofocus -> "autofocus"
    | `Autoplay -> "autoplay"
    | `Checked -> "checked"
    | `Defer -> "defer"
    | `Disabled -> "disabled"
    | `Muted -> "muted"
    | `ReadOnly -> "readonly"
    | `Rect -> "rect"
    | `Selected -> "selected"
    | `Use_credentials -> "use-credentials"
    | `W3_org_1999_xhtml -> "http://www.w3.org/1999/xhtml"
    | `All -> "all"
    | `Preserve -> "preserve"
    | `Default -> "default"
    | `Controls -> "controls"
    | `Ltr -> "ltr"
    | `Rtl -> "rtl"
    | `Get -> "GET"
    | `Post -> "POST"
    | `Formnovalidate -> "formnovalidate"
    | `Hidden -> "hidden"
    | `Ismap -> "ismap"
    | `Loop -> "loop"
    | `Novalidate -> "novalidate"
    | `Open -> "open"
    | `None -> "none"
    | `Metadata -> "metadata"
    | `Audio -> "audio"
    | `Pubdate -> "pubdate"
    | `Required -> "required"
    | `Reversed -> "reserved"
    | `Scoped -> "scoped"
    | `Seamless -> "seamless"
    | `Any -> "any"
    | `Soft -> "soft"
    | `Hard -> "hard"
    | `Context -> "context"
    | `Toolbar -> "toolbar"
    | `Command -> "command"
    | `Checkbox -> "checkbox"
    | `Radio -> "radio"
    | `Multiple -> "multiple"
    | `Left -> "left"
    | `Right -> "right"
    | `Justify -> "justify"
    | `Char -> "char"
    | `Row -> "row"
    | `Col -> "col"
    | `Rowgroup -> "rowgroup"
    | `Colgroup -> "colgroup"
    | `Groups -> "groups"
    | `Rows -> "rows"
    | `Cols -> "cols"
    | `Zero -> "0"
    | `One -> "1"
    | `Yes -> "yes"
    | `No -> "no"
    | `Auto -> "auto"
    | `Circle -> "circle"
    | `Poly -> "poly"
    | `Alternate -> "alternate"
    | `Archives -> "archives"
    | `Author -> "author"
    | `Bookmark -> "bookmark"
    | `External -> "external"
    | `First -> "first"
    | `Help -> "help"
    | `Icon -> "icon"
    | `Index -> "index"
    | `Last -> "last"
    | `License -> "license"
    | `Next -> "next"
    | `Nofollow -> "nofollow"
    | `Noreferrer -> "noreferrer"
    | `Pingback -> "pingback"
    | `Prefetch -> "prefetch"
    | `Prev -> "prev"
    | `Search -> "search"
    | `Stylesheet -> "stylesheet"
    | `Sidebar -> "sidebar"
    | `Tag -> "tag"
    | `Up -> "up"
    | `Verbatim -> "verbatim"
    | `Latin -> "latin"
    | `Latin_name -> "latin-name"
    | `Latin_prose -> "latin-prose"
    | `Full_width_latin -> "full-width-latin"
    | `Kana -> "kana"
    | `Katakana -> "katakana"
    | `Numeric -> "numeric"
    | `Tel -> "tel"
    | `Email -> "email"
    | `Url -> "url"
    | `Other s -> s

  let string_of_input_type = function
    | `Button -> "button"
    | `Checkbox -> "checkbox"
    | `Color -> "color"
    | `Date -> "date"
    | `Datetime -> "datetime"
    | `Datetime_local -> "datetime-local"
    | `Email -> "email"
    | `File -> "file"
    | `Hidden -> "hidden"
    | `Image -> "image"
    | `Month -> "month"
    | `Number -> "number"
    | `Password -> "password"
    | `Radio -> "radio"
    | `Range -> "range"
    | `Readonly -> "readonly"
    | `Reset -> "reset"
    | `Search -> "search"
    | `Submit -> "submit"
    | `Tel -> "tel"
    | `Text -> "text"
    | `Time -> "time"
    | `Url -> "url"
    | `Week -> "week"

  let string_of_number_or_datetime = function
    | `Number n -> string_of_int n
    | `Datetime t -> t

  let string_of_character = String.make 1

  let string_of_number = string_of_int

  let string_of_bool = string_of_bool

  let onoff_of_bool = function
    | false -> "off"
    | true -> "on"

  let unoption_string = function
    | Some x -> x
    | None -> ""

  let string_of_step = function
    | Some x -> Xml_print.string_of_number x
    | None -> "any"

  let string_of_sizes = function
    | Some l ->
      String.concat " "
        (List.map (fun (x, y) -> Printf.sprintf "%dx%d" x y) l)
    | None ->
      "any"

  let string_of_sandbox l =
    String.concat " " (List.map string_of_sandbox_token l)

  let string_of_numbers l =
    String.concat "," (List.map string_of_number l)

  let string_of_mediadesc l =
    String.concat ", " (List.map string_of_mediadesc_token l)

  let string_of_linktypes l =
    String.concat " " (List.map string_of_linktype l)

  type image_candidate =
    [ `Url of Xml.uri
    | `Url_width of Xml.uri * Html_types.number
    | `Url_pixel of Xml.uri * Html_types.float_number ]

  let string_of_srcset (l : [< image_candidate] list) =
    let f = function
    | `Url url -> Xml.string_of_uri url
    | `Url_width (url, v) ->
      Printf.sprintf "%s %sw" (Xml.string_of_uri url) (string_of_number v)
    | `Url_pixel (url, v) ->
      Printf.sprintf "%s %sx" (Xml.string_of_uri url) (Xml_print.string_of_number v)
    in
    String.concat ", " (List.map f l)

end

module Make
    (Xml : Xml_sigs.T with type ('a, 'b) W.ft = 'a -> 'b)
    (Svg : Svg_sigs.T with module Xml := Xml) =
  Make_with_wrapped_functions(Xml)(Wrapped_functions(Xml))(Svg)
