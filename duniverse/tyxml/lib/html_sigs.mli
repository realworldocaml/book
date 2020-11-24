(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2011 Pierre Chambart, Grégoire Henry
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

(** HTML signatures for the functorial interface. *)

(** Signature of typesafe constructors for HTML documents. *)
module type T = sig

  open Html_types

  (** HTML elements.

      Element constructors are in section {{!elements}elements}. Most elements constructors
      are either {{!nullary}nullary}, {{!unary}unary} or {{!star}star},
      depending on the number of children they accept.
      Children are usually given as a list of elements.
      {{!txt}txt} is used for text.

      [div [a [txt "Foo"]]]
      is equivalent to
      [<div><a>foo</a></div>]

      The type variable ['a] is used to track the element's type. This
      allows the OCaml typechecker to check HTML validity.

      For example, [div []] is of type [[> `Div] elt].
      The {{!span}span} function only accepts children of type
      {!Html_types.span_content}.
      Since [`Div] is not part of it. [span [div []]] will not typecheck.

      Note that the concrete implementation of this type can vary.
      See {!Xml} for details.
  *)
  type +'a elt

  (** A complete HTML document. *)
  type doc = html elt

  (** HTML attributes

      Attribute constructors are in section {{!attributes}attributes} and their name starts
      with [a_]. Attributes are given to elements with the [~a] optional argument.

      [a ~a:[a_href "ocsigen.org"] [txt "link!"]]
      is equivalent to
      [<a href="ocsigen.org">link!</a>]

      Similarly to {{!elt}elt}, attributes use the OCaml type system to enforce
      HTML validity.

      For example {!a_href} returns a value of type [[> `Href] attrib].
      The {{!div}div} function only accepts attributes of type
      {!Html_types.div_attrib}.
      Since [`Href] is not part of it,
      [div ~a:[a_href "ocsigen.org"] []] will not typecheck.

      In some cases, attributes have to be disambiguated.
      The [max] attribute has two version,
      {!a_max} and {!a_input_max}, depending on the
      element.
      Such disambiguated attribute will contain the name of the associated element.
  *)
  type +'a attrib

  (** Underlying XML data-structure

      The type variables in {!elt} and {!attrib} are know as {i phantom types}.
      The implementation, defined here, is actually monomorphic.

      In particular, tyxml doesn't impose any overhead over the underlying
      representation. The {!tot} and {!toelt} functions allows to convert
      between the typed and the untyped representation without any cost.

      Note that some implementation may not be iterable or printable, such as the
      Dom representation exposed by js_of_ocaml.
  *)
  module Xml : Xml_sigs.T

  (** [wrap] is a container for elements and values.

      In most cases, ['a wrap = 'a]. For [R] modules (in eliom or js_of_ocaml),
      It will be {!React.S.t}.
  *)
  type 'a wrap = 'a Xml.W.t

  (** [list_wrap] is a container for list of elements.

      In most cases, ['a list_wrap = 'a list]. For [R] modules (in eliom or js_of_ocaml),
      It will be {!ReactiveData.RList.t}.
  *)
  type 'a list_wrap = 'a Xml.W.tlist

  (** A nullary element is an element that doesn't have any children. *)
  type ('a, 'b) nullary = ?a:('a attrib list) -> unit -> 'b elt

  (** A unary element is an element that have exactly one children. *)
  type ('a, 'b, 'c) unary = ?a:('a attrib list) -> 'b elt wrap -> 'c elt

  (** A star element is an element that has any number of children, including zero. *)
  type ('a, 'b, 'c) star =
    ?a:('a attrib list) -> 'b elt list_wrap -> 'c elt

  (** Associated SVG module, for the {!svg} combinator. *)
  module Svg : Svg_sigs.T with module Xml := Xml

  (** Various information about HTML, such as the doctype, ... *)
  module Info : Xml_sigs.Info

  (** {3 Uri} *)

  type uri = Xml.uri
  val string_of_uri : (uri, string) Xml.W.ft
  val uri_of_string : (string, uri) Xml.W.ft

  (** {2:attributes Attributes} *)

  val a_class : nmtokens wrap -> [> | `Class] attrib
  (** This attribute assigns a class name or set of class names to an
      element. Any number of elements may be assigned the same class
      name or names.  *)

  val a_user_data : nmtoken -> text wrap -> [> | `User_data] attrib
  (** May be used to specify custom attributes.
      The example given by the W3C is as follows :
      {v
<ol>
  <li data-length="2m11s">Beyond The Sea</li>
</ol> v}
      It should be used for preprocessing ends only. *)

  val a_id : text wrap -> [> | `Id] attrib
  (** This attribute assigns a name to an element. This name must be
      unique in a document. The text should be without any space. *)

  val a_title : text wrap -> [> | `Title] attrib
  (** This attribute offers advisory information about the element for
      which it is set.

      Values of the title attribute may be rendered by user agents in a
      variety of ways. For instance, visual browsers frequently display
      the title as a {i tool tip} (a short message that appears when the
      pointing device pauses over an object). Audio user agents may
      speak the title information in a similar context.

      The title attribute has an additional role when used with the [link]
      element to designate an external style sheet. Please consult the
      section on links and style sheets for details.  *)

  (** {3 I18N} *)

  val a_xml_lang : languagecode wrap -> [> | `XML_lang] attrib

  val a_lang : languagecode wrap -> [> | `Lang] attrib

  (** {3 Events}

      {4 Javascript events} *)

  val a_onabort : Xml.event_handler -> [> | `OnAbort] attrib
  val a_onafterprint : Xml.event_handler -> [> | `OnAfterPrint] attrib
  val a_onbeforeprint : Xml.event_handler -> [> | `OnBeforePrint] attrib
  val a_onbeforeunload : Xml.event_handler -> [> | `OnBeforeUnload] attrib
  val a_onblur : Xml.event_handler -> [> | `OnBlur] attrib
  val a_oncanplay : Xml.event_handler -> [> | `OnCanPlay] attrib
  val a_oncanplaythrough : Xml.event_handler -> [> | `OnCanPlayThrough] attrib
  val a_onchange : Xml.event_handler -> [> | `OnChange] attrib
  val a_ondurationchange : Xml.event_handler -> [> | `OnDurationChange] attrib
  val a_onemptied : Xml.event_handler -> [> | `OnEmptied] attrib
  val a_onended : Xml.event_handler -> [> | `OnEnded] attrib
  val a_onerror : Xml.event_handler -> [> | `OnError] attrib
  val a_onfocus : Xml.event_handler -> [> | `OnFocus] attrib
  val a_onformchange : Xml.event_handler -> [> | `OnFormChange] attrib
  val a_onforminput : Xml.event_handler -> [> | `OnFormInput] attrib
  val a_onhashchange : Xml.event_handler -> [> | `OnHashChange] attrib
  val a_oninput : Xml.event_handler -> [> | `OnInput] attrib
  val a_oninvalid : Xml.event_handler -> [> | `OnInvalid] attrib
  val a_onmousewheel : Xml.event_handler -> [> | `OnMouseWheel] attrib
  val a_onoffline : Xml.event_handler -> [> | `OnOffLine] attrib
  val a_ononline : Xml.event_handler -> [> | `OnOnLine] attrib
  val a_onpause : Xml.event_handler -> [> | `OnPause] attrib
  val a_onplay : Xml.event_handler -> [> | `OnPlay] attrib
  val a_onplaying : Xml.event_handler -> [> | `OnPlaying] attrib
  val a_onpagehide : Xml.event_handler -> [> | `OnPageHide] attrib
  val a_onpageshow : Xml.event_handler -> [> | `OnPageShow] attrib
  val a_onpopstate : Xml.event_handler -> [> | `OnPopState] attrib
  val a_onprogress : Xml.event_handler -> [> | `OnProgress] attrib
  val a_onratechange : Xml.event_handler -> [> | `OnRateChange] attrib
  val a_onreadystatechange : Xml.event_handler -> [> | `OnReadyStateChange] attrib
  val a_onredo : Xml.event_handler -> [> | `OnRedo] attrib
  val a_onresize : Xml.event_handler -> [> | `OnResize] attrib
  val a_onscroll : Xml.event_handler -> [> | `OnScroll] attrib
  val a_onseeked : Xml.event_handler -> [> | `OnSeeked] attrib
  val a_onseeking : Xml.event_handler -> [> | `OnSeeking] attrib
  val a_onselect : Xml.event_handler -> [> | `OnSelect] attrib
  val a_onshow : Xml.event_handler -> [> | `OnShow] attrib
  val a_onstalled : Xml.event_handler -> [> | `OnStalled] attrib
  val a_onstorage : Xml.event_handler -> [> | `OnStorage] attrib
  val a_onsubmit : Xml.event_handler -> [> | `OnSubmit] attrib
  val a_onsuspend : Xml.event_handler -> [> | `OnSuspend] attrib
  val a_ontimeupdate : Xml.event_handler -> [> | `OnTimeUpdate] attrib
  val a_onundo : Xml.event_handler -> [> | `OnUndo] attrib
  val a_onunload : Xml.event_handler -> [> | `OnUnload] attrib
  val a_onvolumechange : Xml.event_handler -> [> | `OnVolumeChange] attrib
  val a_onwaiting : Xml.event_handler -> [> | `OnWaiting] attrib
  val a_onload : Xml.event_handler -> [> | `OnLoad] attrib
  val a_onloadeddata : Xml.event_handler -> [> | `OnLoadedData] attrib
  val a_onloadedmetadata : Xml.event_handler -> [> | `OnLoadedMetaData] attrib
  val a_onloadstart : Xml.event_handler -> [> | `OnLoadStart] attrib
  val a_onmessage : Xml.event_handler -> [> | `OnMessage] attrib

  (** {4 Mouse events} *)

  val a_onclick : Xml.mouse_event_handler -> [> | `OnClick] attrib
  val a_oncontextmenu : Xml.mouse_event_handler -> [> | `OnContextMenu] attrib
  val a_ondblclick : Xml.mouse_event_handler -> [> | `OnDblClick] attrib
  val a_ondrag : Xml.mouse_event_handler -> [> | `OnDrag] attrib
  val a_ondragend : Xml.mouse_event_handler -> [> | `OnDragEnd] attrib
  val a_ondragenter : Xml.mouse_event_handler -> [> | `OnDragEnter] attrib
  val a_ondragleave : Xml.mouse_event_handler -> [> | `OnDragLeave] attrib
  val a_ondragover : Xml.mouse_event_handler -> [> | `OnDragOver] attrib
  val a_ondragstart : Xml.mouse_event_handler -> [> | `OnDragStart] attrib
  val a_ondrop : Xml.mouse_event_handler -> [> | `OnDrop] attrib
  val a_onmousedown : Xml.mouse_event_handler -> [> | `OnMouseDown] attrib
  val a_onmouseup : Xml.mouse_event_handler -> [> | `OnMouseUp] attrib
  val a_onmouseover : Xml.mouse_event_handler -> [> | `OnMouseOver] attrib
  val a_onmousemove : Xml.mouse_event_handler -> [> | `OnMouseMove] attrib
  val a_onmouseout : Xml.mouse_event_handler -> [> | `OnMouseOut] attrib

  (** {4 Touch events} *)
  val a_ontouchstart : Xml.touch_event_handler -> [> | `OnTouchStart] attrib
  val a_ontouchend : Xml.touch_event_handler -> [> | `OnTouchEnd] attrib
  val a_ontouchmove : Xml.touch_event_handler -> [> | `OnTouchMove] attrib
  val a_ontouchcancel : Xml.touch_event_handler -> [> | `OnTouchCancel] attrib

  (** {4 Keyboard events} *)

  val a_onkeypress : Xml.keyboard_event_handler -> [> | `OnKeyPress] attrib
  val a_onkeydown : Xml.keyboard_event_handler -> [> | `OnKeyDown] attrib
  val a_onkeyup : Xml.keyboard_event_handler -> [> | `OnKeyUp] attrib

  (** {3 Other attributes} *)

  val a_allowfullscreen : unit -> [> | `Allowfullscreen] attrib

  val a_allowpaymentrequest : unit -> [> | `Allowpaymentrequest] attrib

  val a_autocomplete : (bool[@onoff]) wrap -> [> | `Autocomplete] attrib

  val a_async : unit -> [> | `Async] attrib

  val a_autofocus : unit -> [> | `Autofocus] attrib

  val a_autoplay : unit -> [> | `Autoplay] attrib

  val a_muted : unit -> [> | `Muted] attrib

  val a_crossorigin :
    [< | `Anonymous | `Use_credentials ] wrap -> [> | `Crossorigin ] attrib

  val a_integrity :
    string wrap -> [> | `Integrity ] attrib

  val a_mediagroup : string wrap -> [> | `Mediagroup ] attrib

  val a_challenge : text wrap -> [> | `Challenge] attrib

  val a_contenteditable : bool wrap -> [> | `Contenteditable] attrib

  val a_contextmenu : idref wrap -> [> | `Contextmenu] attrib

  val a_controls : unit -> [> | `Controls] attrib

  val a_dir : [< | `Rtl | `Ltr] wrap -> [> | `Dir] attrib

  val a_draggable : bool wrap -> [> | `Draggable] attrib

  val a_form : idref wrap -> [> | `Form] attrib

  val a_formaction : Xml.uri wrap -> [> | `Formaction] attrib

  val a_formenctype : contenttype wrap -> [> | `Formenctype] attrib

  val a_formnovalidate : unit -> [> | `Formnovalidate] attrib

  val a_formtarget : text wrap -> [> | `Formtarget] attrib

  val a_hidden : unit -> [> | `Hidden] attrib

  val a_high : float_number wrap -> [> | `High] attrib

  val a_icon : Xml.uri wrap -> [> | `Icon] attrib

  val a_ismap : unit -> [> | `Ismap] attrib

  val a_keytype : text wrap -> [> | `Keytype] attrib

  val a_list : idref wrap -> [> | `List] attrib

  val a_loop : unit -> [> | `Loop] attrib

  val a_low : float_number wrap -> [> | `High] attrib

  val a_max : float_number wrap -> [> | `Max] attrib

  val a_input_max : number_or_datetime wrap -> [> | `Input_Max] attrib
  [@@reflect.attribute "max" ["input"]]

  val a_min : float_number wrap -> [> | `Min] attrib

  val a_input_min : number_or_datetime wrap -> [> | `Input_Min] attrib
  [@@reflect.attribute "min" ["input"]]

  val a_inputmode :
    [< `Verbatim | `Latin | `Latin_name | `Latin_prose | `Full_width_latin
    | `Kana | `Katakana | `Numeric | `Tel | `Email | `Url ] wrap ->
    [> `Inputmode] attrib
  (** @see <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input#Attributes> Input HTML documentation. *)

  val a_novalidate : unit -> [> | `Novalidate] attrib

  val a_open : unit -> [> | `Open] attrib

  val a_optimum : float_number wrap -> [> | `Optimum] attrib

  val a_pattern : text wrap -> [> | `Pattern] attrib

  val a_placeholder : text wrap -> [> | `Placeholder] attrib

  val a_poster : Xml.uri wrap -> [> | `Poster] attrib

  val a_preload : [< | `None | `Metadata | `Audio] wrap -> [> | `Preload] attrib

  val a_pubdate : unit -> [> | `Pubdate] attrib

  val a_radiogroup : text wrap -> [> | `Radiogroup] attrib

  val a_referrerpolicy : referrerpolicy wrap -> [> `Referrerpolicy] attrib
  (** @see <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe#Attributes>
  *)

  val a_required : unit -> [> | `Required] attrib

  val a_reversed : unit -> [> | `Reversed] attrib

  val a_sandbox : [< | sandbox_token ] list wrap -> [> | `Sandbox] attrib

  val a_spellcheck : bool wrap -> [> | `Spellcheck] attrib

  val a_scoped : unit -> [> | `Scoped] attrib

  val a_seamless : unit -> [> | `Seamless] attrib

  val a_sizes : (number * number) list option wrap -> [> | `Sizes] attrib

  val a_span : number wrap -> [> | `Span] attrib

  (** @deprecated Use {!a_xml_lang} instead. *)
  val a_srclang : nmtoken wrap -> [> | `XML_lang] attrib
  [@@ocaml.deprecated "Use a_xml_lang instead."]

  type image_candidate =
    [ `Url of uri
    | `Url_width of uri * number
    | `Url_pixel of uri * float_number ]

  val a_srcset : image_candidate list wrap -> [> | `Srcset] attrib

  val a_img_sizes : text list wrap -> [> | `Img_sizes] attrib
  [@@reflect.attribute "sizes" ["img"]]

  val a_start : number wrap -> [> | `Start] attrib

  val a_step : float_number option wrap -> [> | `Step] attrib

  val a_wrap : [< | `Soft | `Hard] wrap -> [> | `Wrap] attrib

  val a_version : cdata wrap -> [> | `Version] attrib

  val a_xmlns : [< | `W3_org_1999_xhtml] wrap -> [> | `XMLns] attrib

  val a_manifest : Xml.uri wrap -> [> | `Manifest] attrib

  val a_cite : Xml.uri wrap -> [> | `Cite] attrib

  val a_xml_space : [< | `Default | `Preserve] wrap -> [> | `XML_space] attrib

  val a_accesskey : character wrap -> [> | `Accesskey] attrib
  (** This attribute assigns an access key to an element. An access key
      is a single character from the document character
      set. NB: authors should consider the input method of the
      expected reader when specifying an accesskey. *)

  val a_charset : charset wrap -> [> | `Charset] attrib
  (** This attribute specifies the character encoding of the resource
      designated by the link. Please consult the section on character
      encodings for more details. *)

  val a_accept_charset : charsets wrap -> [> | `Accept_charset] attrib

  val a_accept : contenttypes wrap -> [> | `Accept] attrib

  val a_href : Xml.uri wrap -> [> | `Href] attrib
  (** This attribute specifies the location of a Web resource, thus
      defining a link between the current element (the source anchor)
      and the destination anchor defined by this attribute. *)

  val a_hreflang : languagecode wrap -> [> | `Hreflang] attrib
  (** This attribute specifies the base language of the resource
      designated by href and may only be used when href is specified. *)

  val a_download : string option wrap -> [> | `Download] attrib

  val a_rel : linktypes wrap -> [> | `Rel] attrib
  (** This attribute describes the relationship from the current
      document to the anchor specified by the href attribute. The
      value of this attribute is a space-separated list of link
      types. *)

  (** This attribute is used to describe a reverse link from the
      anchor specified by the href attribute to the current
      document. The value of this attribute is a space-separated
      list of link types. *)

  val a_tabindex : number wrap -> [> | `Tabindex] attrib
  (** This attribute specifies the position of the current
      element in the tabbing order for the current document. This
      value must be a number between 0 and 32767. User agents
      should ignore leading zeros. *)

  val a_mime_type : contenttype wrap -> [> | `Mime_type] attrib
  [@@reflect.attribute "type" ["object"; "embed"; "area"; "link"; "source"]]
  (** This attribute gives an advisory hint as to the content type
      of the content available at the link target address. It
      allows user agents to opt to use a fallback mechanism rather
      than fetch the content if they are advised that they will
      get content in a content type they do not support.Authors
      who use this attribute take responsibility to manage the
      risk that it may become inconsistent with the content
      available at the link target address. *)

  val a_datetime : cdata wrap -> [> | `Datetime] attrib

  val a_action : Xml.uri wrap -> [> | `Action] attrib
  (** This attribute specifies a form processing agent. User agent
      behavior for a value other than an HTTP URI is undefined. *)

  val a_checked : unit -> [> | `Checked] attrib
  (** When the [type] attribute has the value ["radio"] or
      ["checkbox"], this boolean attribute specifies that the
      button is on. User agents must ignore this attribute for
      other control types. *)

  val a_cols : number wrap -> [> | `Cols] attrib
  (** This attribute specifies the visible width in average
      character widths. Users should be able to enter longer lines
      than this, so user agents should provide some means to
      scroll through the contents of the control when the contents
      extend beyond the visible area. User agents may wrap visible
      text lines to keep long lines visible without the need for
      scrolling. *)

  val a_enctype : contenttype wrap -> [> | `Enctype] attrib

  val a_label_for : idref wrap -> [> | `Label_for] attrib
  [@@reflect.attribute "for" ["label"]]

  val a_for : idref wrap -> [> | `Label_for] attrib
  [@@ocaml.deprecated "Use a_label_for"]
  (** @deprecated Use a_label_for *)

  val a_output_for : idrefs wrap -> [> | `Output_for] attrib
  [@@reflect.attribute "for" ["output"]]

  val a_for_list : idrefs wrap -> [> | `Output_for] attrib
  [@@ocaml.deprecated "Use a_output_for"]
  (** @deprecated Use a_output_for *)

  val a_maxlength : number wrap -> [> | `Maxlength] attrib

  val a_minlength : number wrap -> [> | `Minlength] attrib

  val a_method :
    [< | `Get | `Post] wrap -> [> | `Method] attrib

  val a_formmethod :
    [< | `Get | `Post] wrap -> [> | `Method] attrib
  [@@ocaml.deprecated "Use a_method"]
  (** @deprecated Use a_method *)

  val a_multiple : unit -> [> | `Multiple] attrib

  val a_name : text wrap -> [> | `Name] attrib
  (** This attribute assigns the control name. *)

  val a_rows : number wrap -> [> | `Rows] attrib
  (** This attribute specifies the number of visible text
      lines. Users should be able to enter more lines than this,
      so user agents should provide some means to scroll through
      the contents of the control when the contents extend beyond
      the visible area. *)

  val a_selected : unit -> [> | `Selected] attrib
  (** When set, this boolean attribute specifies that
      this option is pre-selected. *)

  val a_size : number wrap -> [> | `Size] attrib

  val a_src : Xml.uri wrap -> [> | `Src] attrib

  val a_input_type : [<
    | `Url
    | `Tel
    | `Text
    | `Time
    | `Search
    | `Password
    | `Checkbox
    | `Range
    | `Radio
    | `Submit
    | `Reset
    | `Number
    | `Hidden
    | `Month
    | `Week
    | `File
    | `Email
    | `Image
    | `Datetime_local
    | `Datetime
    | `Date
    | `Color
    | `Button] wrap -> [> | `Input_Type] attrib
  [@@reflect.attribute "type" ["input"]]

  val a_text_value : text wrap -> [> | `Text_Value] attrib
  [@@reflect.attribute "value" ["param"; "button"; "option"]]
  (** This attribute specifies the initial value of the
      control. If this attribute is not set, the initial value is
      set to the contents of the [option] element. *)

  val a_int_value : number wrap -> [> | `Int_Value] attrib
  [@@reflect.attribute "value" ["li"]]

  val a_value : cdata wrap -> [> | `Value] attrib

  val a_float_value : float_number wrap -> [> | `Float_Value] attrib
  [@@reflect.attribute "value" ["progress"; "meter"]]

  val a_disabled : unit -> [> | `Disabled] attrib

  val a_readonly : unit -> [> | `ReadOnly] attrib
  val a_button_type :
    [< | `Button | `Submit | `Reset] wrap -> [> | `Button_Type] attrib
  [@@reflect.attribute "type" ["button"]]

  val a_command_type :
    [< | `Command | `Checkbox | `Radio] wrap -> [> | `Command_Type] attrib
  [@@reflect.attribute "type" ["command"]]

  val a_menu_type : [< | `Context | `Toolbar] wrap -> [> | `Menu_Type] attrib
  [@@reflect.attribute "type" ["menu"]]

  val a_label : text wrap -> [> | `Label] attrib

  val a_align :
    [< | `Left | `Right | `Justify | `Char] wrap -> [> | `Align] attrib
  [@@ocaml.deprecated "Use CSS text-align"]
  (** @deprecated Use CSS text-align *)

  val a_axis : cdata wrap -> [> | `Axis] attrib
  [@@ocaml.deprecated "Not supported in HTML5"]
  (** @deprecated Not supported in HTML5 *)

  val a_colspan : number wrap -> [> | `Colspan] attrib

  val a_headers : idrefs wrap -> [> | `Headers] attrib

  val a_rowspan : number wrap -> [> | `Rowspan] attrib

  val a_scope :
    [< | `Row | `Col | `Rowgroup | `Colgroup] wrap -> [> | `Scope] attrib
  [@@ocaml.deprecated "Not supported in HTML5"]
  (** @deprecated Not supported in HTML5 *)

  val a_summary : text wrap -> [> | `Summary] attrib
  [@@ocaml.deprecated "Move content elsewhere or to a <caption> child"]
  (** @deprecated Move content elsewhere or to a <caption> child *)

  val a_border : pixels wrap -> [> | `Border] attrib
  [@@ocaml.deprecated "Use CSS border and/or border-width"]
  (** @deprecated Use CSS border and/or border-width *)

  val a_rules :
    [< | `None | `Groups | `Rows | `Cols | `All] wrap -> [> | `Rules] attrib
  [@@ocaml.deprecated "Use CSS border"]
  (** @deprecated Use CSS border *)

  val a_char : character wrap -> [> | `Char] attrib
  [@@ocaml.deprecated "The char attribute is not supported in HTML5"]
  (** @deprecated The char attribute is not supported in HTML5 *)

  val a_alt : text wrap -> [> | `Alt] attrib

  val a_height : number wrap -> [> | `Height] attrib

  val a_width : number wrap -> [> | `Width] attrib

  type shape = [ | `Rect | `Circle | `Poly | `Default ]

  val a_shape : shape wrap -> [> | `Shape] attrib

  val a_coords : numbers wrap -> [> | `Coords] attrib

  val a_usemap : idref wrap -> [> | `Usemap] attrib

  val a_data : Xml.uri wrap -> [> | `Data] attrib

  val a_codetype : contenttype wrap -> [> | `Codetype] attrib
  [@@ocaml.deprecated "Not supported in HTML5"]
  (** @deprecated Not supported in HTML5 *)

  val a_frameborder : [< | `Zero | `One] wrap -> [> | `Frameborder] attrib
  [@@ocaml.deprecated "Use CSS border"]
  (** @deprecated Use CSS border *)

  val a_marginheight : pixels wrap -> [> | `Marginheight] attrib
  [@@ocaml.deprecated "Use CSS margin"]
  (** @deprecated Use CSS *)

  val a_marginwidth : pixels wrap -> [> | `Marginwidth] attrib
  [@@ocaml.deprecated "Use CSS margin"]
  (** @deprecated Use CSS *)

  val a_scrolling : [< | `Yes | `No | `Auto] wrap -> [> | `Scrolling] attrib

  val a_target : frametarget wrap -> [> | `Target] attrib

  val a_content : text wrap -> [> | `Content] attrib

  val a_http_equiv : text wrap -> [> | `Http_equiv] attrib

  val a_defer : unit -> [> | `Defer] attrib

  val a_media : mediadesc wrap -> [> | `Media] attrib

  val a_style : string wrap -> [> | `Style_Attr] attrib

  val a_property : string wrap -> [> | `Property] attrib

  (** {3 ARIA support} *)

  (** {{: https://www.w3.org/TR/wai-aria-1.1/} WAI-ARIA} is a specification
      written by the W3C, defining a set of additional HTML attributes that can
      be applied to elements to provide additional semantics and improve
      accessibility wherever it is lacking.

      See for example a {{:
      https://developer.mozilla.org/en-US/docs/Learn/Accessibility/WAI-ARIA_basics}
      WAI-ARIA tutorial}.
  *)

  val a_role : string list wrap -> [> | `Role] attrib
  (** @see <https://www.w3.org/TR/role-attribute> Role attribute specification
      @see <https://www.w3.org/TR/wai-aria-1.1/#role_definitions> List of WAI-ARIA roles
  *)

  val a_aria : string -> string list wrap -> [> | `Aria] attrib
  (** Basic support for WAI-ARIA attributes: [a_aria "foo"] corresponds to an
      "aria-foo" attribute.

      @see <https://www.w3.org/TR/wai-aria-1.1/#state_prop_def> List of WAI-ARIA attributes
  *)

  (** {2:elements Elements} *)

  val txt : string wrap -> [> | txt] elt

  val html :
    ?a: ((html_attrib attrib) list) ->
    [< | head] elt wrap -> [< | body] elt wrap -> [> | html] elt
  [@@reflect.filter_whitespace]
  [@@reflect.element "html"]

  val head :
    ?a: ((head_attrib attrib) list) ->
    [< | title] elt wrap -> (head_content_fun elt) list_wrap -> [> | head] elt
  [@@reflect.filter_whitespace]
  [@@reflect.element "head"]

  val base : ([< | base_attrib], [> | base]) nullary

  val title : (title_attrib, [< | title_content_fun], [> | title]) unary

  val body : ([< | body_attrib], [< | body_content_fun], [> | body]) star


  val svg : ?a : [< svg_attrib ] Svg.attrib list -> [< svg_content ] Svg.elt list_wrap -> [> svg ] elt

  (** {3 Section} *)

  val footer :
    ([< | footer_attrib], [< | footer_content_fun], [> | footer]) star

  val header :
    ([< | header_attrib], [< | header_content_fun], [> | header]) star

  val section :
    ([< | section_attrib], [< | section_content_fun], [> | section]) star

  val nav : ([< | nav_attrib], [< | nav_content_fun], [> | nav]) star

  val h1 : ([< | h1_attrib], [< | h1_content_fun], [> | h1]) star

  val h2 : ([< | h2_attrib], [< | h2_content_fun], [> | h2]) star

  val h3 : ([< | h3_attrib], [< | h3_content_fun], [> | h3]) star

  val h4 : ([< | h4_attrib], [< | h4_content_fun], [> | h4]) star

  val h5 : ([< | h5_attrib], [< | h5_content_fun], [> | h5]) star

  val h6 : ([< | h6_attrib], [< | h6_content_fun], [> | h6]) star

  val hgroup :
    ([< | hgroup_attrib], [< | hgroup_content_fun], [> | hgroup]) star

  val address :
    ([< | address_attrib], [< | address_content_fun], [> | address]) star

  val article :
    ([< | article_attrib], [< | article_content_fun], [> | article]) star

  val aside :
    ([< | aside_attrib], [< | aside_content_fun], [> | aside]) star

  val main :
    ([< | main_attrib], [< | main_content_fun], [> | main]) star

  (** {3 Grouping content} *)

  val p : ([< | p_attrib], [< | p_content_fun], [> | p]) star

  val pre : ([< | pre_attrib], [< | pre_content_fun], [> | pre]) star

  val blockquote :
    ([< | blockquote_attrib], [< | blockquote_content_fun], [> | blockquote])
      star

  val div : ([< | div_attrib], [< | div_content_fun], [> | div]) star

  val dl : ([< | dl_attrib], [< | dl_content_fun], [> | dl]) star

  val ol : ([< | ol_attrib], [< | ol_content_fun], [> | ol]) star
  [@@reflect.filter_whitespace]

  val ul : ([< | ul_attrib], [< | ul_content_fun], [> | ul]) star
  [@@reflect.filter_whitespace]

  val dd : ([< | dd_attrib], [< | dd_content_fun], [> | dd]) star

  val dt : ([< | dt_attrib], [< | dt_content_fun], [> | dt]) star

  val li : ([< | li_attrib], [< | li_content_fun], [> | li]) star

  val figcaption :
    ([< | figcaption_attrib], [< | figcaption_content_fun], [> | figcaption]) star

  val figure :
    ?figcaption: ([`Top of [< | figcaption ] elt wrap | `Bottom of [< | figcaption ] elt wrap ]) ->
    ([< | figure_attrib], [< | figure_content_fun], [> | figure]) star
  [@@reflect.element "figure"]

  val hr : ([< | hr_attrib], [> | hr]) nullary

  (** {3 Semantic} *)

  val b : ([< | b_attrib], [< | b_content_fun], [> | b]) star

  val i : ([< | i_attrib], [< | i_content_fun], [> | i]) star

  val u : ([< | u_attrib], [< | u_content_fun], [> | u]) star

  val small :
    ([< | small_attrib], [< | small_content_fun], [> | small]) star

  val sub : ([< | sub_attrib], [< | sub_content_fun], [> | sub]) star

  val sup : ([< | sup_attrib], [< | sup_content_fun], [> | sup]) star

  val mark : ([< | mark_attrib], [< | mark_content_fun], [> | mark]) star

  val wbr : ([< | wbr_attrib], [> | wbr]) nullary

  val bdo :
    dir: [< | `Ltr | `Rtl] wrap ->
    ([< | bdo_attrib], [< | bdo_content_fun], [> | bdo]) star

  val abbr : ([< | abbr_attrib], [< | abbr_content_fun], [> | abbr]) star

  val br : ([< | br_attrib], [> | br]) nullary

  val cite : ([< | cite_attrib], [< | cite_content_fun], [> | cite]) star

  val code : ([< | code_attrib], [< | code_content_fun], [> | code]) star

  val dfn : ([< | dfn_attrib], [< | dfn_content_fun], [> | dfn]) star

  val em : ([< | em_attrib], [< | em_content_fun], [> | em]) star

  val kbd : ([< | kbd_attrib], [< | kbd_content_fun], [> | kbd]) star

  val q : ([< | q_attrib], [< | q_content_fun], [> | q]) star

  val samp : ([< | samp_attrib], [< | samp_content_fun], [> | samp]) star

  val span : ([< | span_attrib], [< | span_content_fun], [> | span]) star

  val strong :
    ([< | strong_attrib], [< | strong_content_fun], [> | strong]) star

  val time : ([< | time_attrib], [< | time_content_fun], [> | time]) star

  val var : ([< | var_attrib], [< | var_content_fun], [> | var]) star

  (** {3 Hypertext} *)

  val a : ([< | a_attrib], 'a, [> | 'a a]) star

  (** {3 Edit} *)

  val del : ([< | del_attrib], 'a, [> | 'a del]) star
  val ins : ([< | ins_attrib], 'a, [> | 'a ins]) star

  (** {3 Embedded} *)

  val img :
    src: Xml.uri wrap ->
    alt: text wrap ->
    ([< img_attrib], [> img]) nullary

  val iframe :
    ([< | iframe_attrib], [< | iframe_content_fun], [> | iframe]) star

  val object_ :
    ?params: (([< | param] elt) list_wrap ) ->
    ([< | object__attrib], 'a, [> | `Object of 'a]) star
  [@@reflect.element "object_" "object"]

  val param : ([< | param_attrib], [> | param]) nullary

  val embed : ([< | embed_attrib], [> | embed]) nullary

  val audio :
    ?src:Xml.uri wrap ->
    ?srcs:(([< | source] elt) list_wrap) ->
    ([< | audio_attrib], 'a, [> 'a audio ]) star
  [@@reflect.element "audio_video"]

  val video :
    ?src:Xml.uri wrap ->
    ?srcs: (([< | source] elt) list_wrap) ->
    ([< | video_attrib], 'a, [> 'a video]) star
  [@@reflect.element "audio_video"]

  val canvas : ([< | canvas_attrib], 'a, [> | 'a canvas]) star

  val source : ([< | source_attrib], [> | source]) nullary

  val area :
    alt: text wrap ->
    ([<
      | common
      | `Alt
      | `Coords
      | `Shape
      | `Target
      | `Rel
      | `Media
      | `Hreflang
      | `Mime_type
    ], [> | area]) nullary

  val map : ([< | map_attrib], 'a, [> | 'a map]) star

  (** {3 Tables Data} *)

  val caption :
    ([< | caption_attrib], [< | caption_content_fun], [> | caption]) star

  val table :
    ?caption: [< | caption] elt wrap ->
    ?columns: [< | colgroup] elt list_wrap ->
    ?thead: [< | thead] elt wrap ->
    ?tfoot: [< | tfoot] elt wrap ->
    ([< | table_attrib], [< | table_content_fun], [> | table]) star
  [@@reflect.filter_whitespace]
  [@@reflect.element "table"]

  val tablex :
    ?caption: [< | caption] elt wrap ->
    ?columns: [< | colgroup] elt list_wrap ->
    ?thead: [< | thead] elt wrap ->
    ?tfoot: [< | tfoot] elt wrap ->
    ([< | tablex_attrib], [< | tablex_content_fun], [> | tablex]) star
  [@@reflect.filter_whitespace]
  [@@reflect.element "table" "table"]

  val colgroup :
    ([< | colgroup_attrib], [< | colgroup_content_fun], [> | colgroup]) star
  [@@reflect.filter_whitespace]

  val col : ([< | col_attrib], [> | col]) nullary

  val thead :
    ([< | thead_attrib], [< | thead_content_fun], [> | thead]) star
  [@@reflect.filter_whitespace]

  val tbody :
    ([< | tbody_attrib], [< | tbody_content_fun], [> | tbody]) star
  [@@reflect.filter_whitespace]

  val tfoot :
    ([< | tfoot_attrib], [< | tfoot_content_fun], [> | tfoot]) star
  [@@reflect.filter_whitespace]

  val td : ([< | td_attrib], [< | td_content_fun], [> | td]) star

  val th : ([< | th_attrib], [< | th_content_fun], [> | th]) star

  val tr : ([< | tr_attrib], [< | tr_content_fun], [> | tr]) star
  [@@reflect.filter_whitespace]

  (** {3 Forms} *)

  val form : ([< | form_attrib], [< | form_content_fun], [> | form]) star

  val fieldset :
    ?legend: [< | legend ] elt wrap ->
    ([< | fieldset_attrib], [< | fieldset_content_fun], [> | fieldset]) star
  [@@reflect.element "fieldset"]

  val legend :
    ([< | legend_attrib], [< | legend_content_fun], [> | legend]) star

  (** Label authorizes only one control inside them
      that should be labelled with a [for] attribute
      (although it is not necessary). Such constraints are not currently
      enforced by the type-system *)
  val label :
    ([< | label_attrib], [< | label_content_fun], [> | label]) star

  val input : ([< | input_attrib], [> | input]) nullary

  val button :
    ([< | button_attrib], [< | button_content_fun], [> | button]) star

  val select :
    ([< | select_attrib], [< | select_content_fun], [> | select]) star
  [@@reflect.filter_whitespace]

  val datalist :
    ?children:(
      [<
        | `Options of ([< | selectoption] elt) list_wrap
        | `Phras of ([< | phrasing] elt) list_wrap
      ]) ->
    ([< | datalist_attrib], [> | datalist]) nullary
  [@@reflect.filter_whitespace]
  [@@reflect.element "datalist"]

  val optgroup :
    label: text wrap  ->
    ([< | optgroup_attrib], [< | optgroup_content_fun], [> | optgroup]) star

  val option :
    ([< | option_attrib], [< | option_content_fun], [> | selectoption]) unary

  val textarea :
    ([< | textarea_attrib], [< | textarea_content_fun], [> | textarea]) unary

  val keygen : ([< | keygen_attrib], [> | keygen]) nullary

  val progress :
    ([< | progress_attrib], [< | progress_content_fun], [> | progress]) star

  val meter :
    ([< | meter_attrib], [< | meter_content_fun], [> | meter]) star

  val output_elt :
    ([< | output_elt_attrib], [< | output_elt_content_fun], [> | output_elt]) star
  [@@reflect.element "star" "output"]

  (** {3 Data} *)

  (** [entity "foo"] is the HTML entity [&foo;]. Both numerical and named form are allowed.

      @see <http://www.w3schools.com/html/html_entities.asp> A tutorial on HTML entities.
      @see <https://www.w3.org/TR/html5/syntax.html#named-character-references> The list of HTML entities.
  *)
  val entity : string -> [> | txt] elt

  val space : unit -> [> | txt] elt

  val cdata : string -> [> | txt] elt
  val cdata_script : string -> [> | txt] elt
  val cdata_style : string -> [> | txt] elt


  (** {3 Interactive} *)

  val details :
    [< | summary] elt wrap ->
    ([< | details_attrib], [< | details_content_fun], [> | details]) star
  [@@reflect.element "details"]

  val summary :
    ([< | summary_attrib], [< | summary_content_fun], [> | summary]) star

  val command :
    label: text wrap ->
    ([< | command_attrib], [> | command]) nullary

  val menu :
    ?children:(
      [<
        | `Lis of ([< | `Li of [< | common]] elt) list_wrap
        | `Flows of ([< | flow5] elt) list_wrap
      ]) ->
    ([< | menu_attrib], [> | menu]) nullary
  [@@reflect.element "menu"]

  (** {3 Scripting} *)

  val script :
    ([< | script_attrib], [< | script_content_fun], [> | script]) unary
  [@@reflect.element "script"]

  val noscript :
    ([< | noscript_attrib], [< | noscript_content_fun], [> | noscript]) star

  val template :
    ([< | template_attrib], [< | template_content_fun], [> | template]) star
  (** @see <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/template>
      Template element documentation on MDN *)

  val meta : ([< | meta_attrib], [> | meta]) nullary

  (** {3 Style Sheets} *)

  val style :
    ([< | style_attrib], [< | style_content_fun], [> | style]) star

  val link :
    rel: linktypes wrap ->
    href: Xml.uri wrap ->
    ([< | link_attrib], [> | link]) nullary

  (** {3 Ruby} *)

  val rt : ([< | rt_attrib], [< | rt_content_fun], [> | rt]) star

  val rp : ([< | rp_attrib], [< | rp_content_fun], [> | rp]) star

  val ruby : ([< | ruby_attrib], [< | ruby_content_fun], [> | ruby]) star

  (** {3 Deprecated} *)

  val pcdata : string wrap -> [> | pcdata] elt
  [@@ocaml.deprecated "Use txt instead"]
  (** @deprecated Use txt instead *)

  (** {2 Conversion with untyped representation}

      WARNING: These functions do not ensure HTML or SVG validity! You should
      always explicitly given an appropriate type to the output.
  *)

  (** [import signal] converts the given XML signal into Tyxml elements.
      It can be used with HTML and SVG parsing libraries, such as Markup.
      @raise Xml_stream.Malformed_stream if the stream is malformed.
  *)
  val of_seq : Xml_stream.signal Seq.t -> 'a elt list_wrap

  val tot : Xml.elt -> 'a elt
  val totl : Xml.elt list_wrap -> 'a elt list_wrap
  val toelt : 'a elt -> Xml.elt
  val toeltl : 'a elt list_wrap -> Xml.elt list_wrap

  val doc_toelt : doc -> Xml.elt
  val to_xmlattribs : 'a attrib list -> Xml.attrib list
  val to_attrib : Xml.attrib -> 'a attrib

  (** Unsafe features.

      Using this module can break
      HTML validity and may introduce security problems like
      code injection.
      Use it with care.
  *)
  module Unsafe : sig

    (** Insert raw text without any encoding *)
    val data : string wrap -> 'a elt

    (** Insert an XML node that is not implemented in this module.
        If it is a standard HTML node which is missing,
        please report to the Ocsigen team.
    *)
    val node : string -> ?a:'a attrib list -> 'b elt list_wrap -> 'c elt

    (** Insert an XML node without children
        that is not implemented in this module.
        If it is a standard HTML node which is missing,
        please report to the Ocsigen team.
    *)
    val leaf : string -> ?a:'a attrib list -> unit -> 'b elt

    (** Remove phantom type annotation on an element,
        to make it usable everywhere.
    *)
    val coerce_elt : 'a elt -> 'b elt

    (** Insert an attribute that is not implemented in this module.
        If it is a standard HTML attribute which is missing,
        please report to the Ocsigen team.
    *)
    val string_attrib : string -> string wrap -> 'a attrib

    (** Same, for float attribute *)
    val float_attrib : string -> float wrap -> 'a attrib

    (** Same, for int attribute *)
    val int_attrib : string -> int wrap -> 'a attrib

    (** Same, for URI attribute *)
    val uri_attrib : string -> uri wrap -> 'a attrib

    (** Same, for a space separated list of values *)
    val space_sep_attrib : string -> string list wrap -> 'a attrib

    (** Same, for a comma separated list of values *)
    val comma_sep_attrib : string -> string list wrap -> 'a attrib

  end

end

(** Equivalent to {!T}, but without wrapping. *)
module type NoWrap = T with module Xml.W = Xml_wrap.NoWrap


(** {2 Signature functors}
    {% See <<a_manual chapter="functors"|the manual of the functorial interface>>. %} *)

(** Signature functor for {!Html_f.Make}. *)
module Make
    (Xml : Xml_sigs.T)
    (Svg : Svg_sigs.T with module Xml := Xml) :
sig

  (** See {!module-type:Html_sigs.T}. *)
  module type T = T
    with type 'a Xml.W.t = 'a Xml.W.t
     and type 'a Xml.W.tlist = 'a Xml.W.tlist
     and type ('a,'b) Xml.W.ft = ('a,'b) Xml.W.ft
     and type Xml.uri = Xml.uri
     and type Xml.event_handler = Xml.event_handler
     and type Xml.mouse_event_handler = Xml.mouse_event_handler
     and type Xml.keyboard_event_handler = Xml.keyboard_event_handler
     and type Xml.touch_event_handler = Xml.touch_event_handler
     and type Xml.attrib = Xml.attrib
     and type Xml.elt = Xml.elt
     and module Svg := Svg
end

(** Wrapped functions, to be used with {!Html_f.Make_with_wrapped_functions}. *)
module type Wrapped_functions = sig

  module Xml : Xml_sigs.T

  val string_of_big_variant :
    ([< Html_types.big_variant], string) Xml.W.ft

  val string_of_bool : (bool, string) Xml.W.ft

  val onoff_of_bool : (bool, string) Xml.W.ft

  val string_of_character : (Html_types.character, string) Xml.W.ft

  val string_of_input_type :
    ([< Html_types.input_type], string) Xml.W.ft

  val string_of_number_or_datetime :
    ([< Html_types.number_or_datetime], string) Xml.W.ft

  val string_of_linktypes :
    ([< Html_types.linktype] list, string) Xml.W.ft

  val string_of_mediadesc :
    ([< Html_types.mediadesc_token] list, string) Xml.W.ft

  val string_of_referrerpolicy :
    ([< Html_types.referrerpolicy], string) Xml.W.ft

  val string_of_numbers : (Html_types.numbers, string) Xml.W.ft

  val string_of_sandbox :
    ([< Html_types.sandbox_token] list, string) Xml.W.ft

  val string_of_sizes :
    ((Html_types.number * Html_types.number) list option, string) Xml.W.ft

  type image_candidate =
    [ `Url of Xml.uri
    | `Url_width of Xml.uri * Html_types.number
    | `Url_pixel of Xml.uri * Html_types.float_number ]

  val string_of_srcset :
    ([< image_candidate] list, string) Xml.W.ft

  val string_of_step : (float option, string) Xml.W.ft

  val unoption_string : (string option, string) Xml.W.ft

end
