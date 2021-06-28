(* TyXML
 * http://www.ocsigen.org/tyxml
 * Copyright (C) 2010 by Simon Castellan
 * Copyright (C) 2010 by Cecile Herbelin
 * Copyright (C) 2010 by Vincent Balat
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

(* _fun prefix are the types that must be used
   in Html_sigs. They are more restrictive as
   some param are already taken as separate argument,
   to ensure better compatibility.
   SC *)


(** HTML types with variants, goes with {!Html_sigs.T}.

    @see <http://dev.w3.org/html5/spec/Overview.html> information concerning HTML at W3C.
*)


(** {1 Attribute types.} *)

type cdata = string
(** Character data *)

type id = string
(** A document-unique identifier *)

type idref = string
(** A reference to a document-unique identifier *)

type idrefs = idref list
(** A space-separated list of references to document-unique identifiers *)

type name = string
(** A name with the same character constraints as ID above *)

type nmtoken = string
(** A name composed of only name tokens as defined in XML 1.0
        @see <http://www.w3.org/TR/2000/REC-xml-20001006> XML 1.0 *)

type nmtokens = nmtoken list
(** One or more white space separated NMTOKEN values *)

(** {2 Data Types} *)

type character = char
(** A single character from ISO 10646. *)

type charset = string
(** A character encoding, as per RFC2045 (MIME).
        @see <http://tools.ietf.org/html/rfc2045> RFC2045 *)

type charsets = charset list
(** A space-separated list of character encodings, as per RFC2045 (MIME).
        @see <http://tools.ietf.org/html/rfc2045> RFC2045 *)

type contenttype = string
(** A media type, as per RFC2045 (MIME).
        @see <http://tools.ietf.org/html/rfc2045> RFC2045 *)

type contenttypes = contenttype list
(** A comma-separated list of media types, as per RFC2045 (MIME).
        @see <http://tools.ietf.org/html/rfc2045> RFC2045 *)

type number = int

(* space-separated *)
type numbers = number list

type coords = string list
(** Comma- separated list of coordinates to use in defining areas. *)

type datetime = string
(** Date and time information. *)

type number_or_datetime = [ | `Number of number | `Datetime of datetime ]
(** Either a number or date and time information. *)

type fpi = string
(** A character string representing an SGML Formal Public Identifier. *)

type frametarget = string
(** Frame name used as destination for results of certain actions. *)

type languagecode = string
(** A language code, as per RFC5646/BCP47.
    @see <http://tools.ietf.org/html/rfc5646> RFC5646 *)

type linktype =
  [
    | `Alternate
    | `Archives
    | `Author
    | `Bookmark
    | `Canonical
    | `External
    | `First
    | `Help
    | `Icon
    | `Index
    | `Last
    | `License
    | `Next
    | `Nofollow
    | `Noreferrer
    | `Noopener
    | `Pingback
    | `Prefetch
    | `Prev
    | `Search
    | `Stylesheet
    | `Sidebar
    | `Tag
    | `Up
    | `Other of string ] [@@reflect.total_variant]

type linktypes = linktype list
(** Authors may use the following recognized link types, listed here with
        their conventional interpretations. A LinkTypes value refers to a
        space-separated list of link types. White space characters are not
        permitted within link types.  These link types are case-insensitive, i.e.,
        ["Alternate"] has the same meaning as ["alternate"].

        User agents, search engines, etc. may interpret these link types in a
        variety of ways. For example, user agents may provide access to linked
        documents through a navigation bar.

        {ul
        {- [`Alternate]:
        Gives alternate representations of the current document.}
        {- [`Archives]:
        Provides a link to a collection of records, documents, or other materials of historical interest.}
        {- [`Author]:
        Gives a link to the current document's author.}
        {- [`Bookmark]:
        Gives the permalink for the nearest ancestor section.}
        {- [`Canonical]:
        Gives the preferred location for accessing the current document.}
        {- [`External]:
        Indicates that the referenced document is not part of the same site as the current document.}
        {- [`First]:
        Indicates that the current document is a part of a series, and that the first document in the series is the referenced document.}
        {- [`Help]:
        Provides a link to context-sensitive help.}
        {- [`Icon]:
        Imports an icon to represent the current document.}
        {- [`Index]:
        Gives a link to the document that provides a table of contents or index listing the current document.}
        {- [`Last]:
        Indicates that the current document is a part of a series, and that the last document in the series is the referenced document.}
        {- [`Licence]:
        Indicates that the main content of the current document is covered by the copyright license described by the referenced document.}
        {- [`Next]:
        Indicates that the current document is a part of a series, and that the next document in the series is the referenced document.}
        {- [`Nofollow]:
        Indicates that the current document's original author or publisher does not endorse the referenced document.}
        {- [`Noreferrer]:
        Requires that the user agent not send an HTTP Referer (sic) header if the user follows the hyperlink.}
        {- [`Noopener]:
        Instructs the browser to open the link without granting the new browsing context access to the document that opened it.}
        {- [`Pingback]:
        Gives the address of the pingback server that handles pingbacks to the current document.}
        {- [`Prefetch]:
        Specifies that the target resource should be preemptively cached.}
        {- [`Prev]:
        Indicates that the current document is a part of a series, and that the previous document in the series is the referenced document.}
        {- [`Search]:
        Gives a link to a resource that can be used to search through the current document and its related pages.}
        {- [`Stylesheet]:
        Imports a stylesheet.}
        {- [`Sidebar]:
        Specifies that the referenced document, if retrieved, is intended to be shown in the browser's sidebar (if it has one).}
        {- [`Tag]:
        Gives a tag (identified by the given address) that applies to the current document.}
        {- [`Up]:
        Provides a link to a document giving the context for the current document.}
        } *)

type mediadesc_token =
  [ `All
  | `Aural
  | `Braille
  | `Embossed
  | `Handheld
  | `Print
  | `Projection
  | `Screen
  | `Speech
  | `Tty
  | `Tv
  | `Raw_mediadesc of string ] [@@reflect.total_variant]

type mediadesc = mediadesc_token list

(** The MediaDesc attribute is a comma-separated list of media descriptors.
        The following is a list of recognized media descriptors:
        {ul
        {- [`Screen]:
        For non-paged computer screens.}
        {- [`TTY]:
        For media using a fixed-pitch character grid (like teletypes, terminals, or devices with limited display capabilities).}
        {- [`TV]:
        For TV-type devices (low resolution, limited scrollability).}
        {- [`Projection]:
        For projectors.}
        {- [`Handheld]:
        For handheld devices (small screen, limited bandwidth).}
        {- [`Print]:
        For paged and for documents viewed on screen in print preview mode.}
        {- [`Braille]:
        For braille tactile feedback devices.}
        {- [`Aural]:
        For speech synthesizers.}
        {- [`All]:
        For speech synthesizers.}
        {- [`Raw_mediadesc]:
        For more complex (untyped) media descriptors.}}

*)

type float_number = float

type pixels = int
(** The value is an integer that represents the number of pixels of
        the canvas (screen, paper). Thus, the value ["50"] means fifty
        pixels. For normative information about the definition of a pixel,
        please consult CSS2.
        @see <http://www.w3.org/TR/1998/REC-CSS2-19980512> CSS2 *)

type script_ = string
(** Script data can be the content of the ["script"] element and the
        value of intrinsic event attributes. User agents must not evaluate
        script data as HTML markup but instead must pass it on as data to a
        script engine.

        The case-sensitivity of script data depends on the scripting
        language.

        Please note that script data that is element content may not
        contain character references, but script data that is the value of
        an attribute may contain them. *)

type text = string
(** Arbitrary textual data, likely meant to be human-readable. *)

(** {2 Core} *)

type i18n = [ | `XML_lang | `Lang ]

type core =
  [
    | `Accesskey
    | `Class
    | `Contenteditable
    | `Contextmenu
    | `Dir
    | `Draggable
    | `Hidden
    | `Id
    | i18n
    | `Spellcheck
    | `Style_Attr
    | `Tabindex
    | `Title
    | `User_data
    | `XMLns
  ]

(** {2 Events} *)

(** Javascript events *)
type events =
  [
    | `OnAbort
    | `OnBlur
    | `OnCanPlay
    | `OnCanPlayThrough
    | `OnChange
    | `OnClick
    | `OnContextMenu
    | `OnDblClick
    | `OnDrag
    | `OnDragEnd
    | `OnDragEnter
    | `OnDragLeave
    | `OnDragOver
    | `OnDragStart
    | `OnDrop
    | `OnDurationChange
    | `OnEmptied
    | `OnEnded
    | `OnError
    | `OnFocus
    | `OnFormChange
    | `OnFormInput
    | `OnInput
    | `OnInvalid
    | `OnMouseDown
    | `OnMouseUp
    | `OnMouseOver
    | `OnMouseMove
    | `OnMouseOut
    | `OnMouseWheel
    | `OnPause
    | `OnPlay
    | `OnPlaying
    | `OnProgress
    | `OnRateChange
    | `OnReadyStateChange
    | `OnScroll
    | `OnSeeked
    | `OnSeeking
    | `OnSelect
    | `OnShow
    | `OnStalled
    | `OnSubmit
    | `OnSuspend
    | `OnTimeUpdate
    | `OnTouchStart
    | `OnTouchEnd
    | `OnTouchMove
    | `OnTouchCancel
    | `OnVolumeChange
    | `OnWaiting
    | `OnKeyPress
    | `OnKeyDown
    | `OnKeyUp
    | `OnLoad
    | `OnLoadedData
    | `OnLoadedMetaData
    | `OnLoadStart
  ]

(** {2 ARIA} *)

type aria =
  [
    | `Role
    | `Aria
  ]

(** Common attributes *)
type common = [ | core | i18n | events | aria ]

(** {1 Categories of HTML elements}

    These category are mainly subdivised in
    - interactive,
    - phrasing,
    - flow5,
    these categories may overlap *)

type heading = [ | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hgroup ]

type sectioning = [ | `Section | `Nav | `Aside | `Article ]

type resetable = [ | `Textarea | `Select | `Output | `Keygen | `Input ]

type submitable = [ | `Textarea | `Select | `Keygen | `Input | `Button ]

type labelable = [ | resetable | `Progress | `Meter | `Button ]

type labelable_without_interactive = [ `Progress | `Meter]

type formatblock =
  [
    | heading
    | sectioning
    | `Pre
    | `P
    | `Header
    | `Footer
    | `Div
    | `Blockquote
    | `Address
  ]

type sectionningroot =
  [ | `Td | `Figure | `Fieldset | `Details | `Body | `Blockquote
  ]

type listed = [ | resetable | submitable | `Fieldset ]

type formassociated = [ | listed | `Progress | `Meter | `Label ]

type subressource_integrity = [ | `Crossorigin | `Integrity ]
(** @see <https://developer.mozilla.org/en-US/docs/Web/Security/Subresource_Integrity> *)

(** Transparent elements.
    Such elements have a part of they children in their data
    and behaves like them. We could do something like [a: 'a elt list -> 'a elt]
    but the information about the node name would be forgotten and would allow
    things like that : [p [a [a []]]].
    This system allow to build non-conforming terms such as [a [a []]] but when passed
    to a standard element (such as [p]), it will yield an error.
    Exception to that : if you embdedd the element in another transparent (of an
    another kind) : [p [noscript (a [a []])]] will be correctly typed.
*)
type (+'interactive, +'noscript, +'regular, +'media) transparent =
  [
    | `A of 'interactive
    | `Noscript of 'noscript
    | `Canvas of 'regular
    | `Map of 'regular
    | `Ins of 'regular
    | `Del of 'regular
    | `Object of 'regular
    | `Object_interactive of 'regular
    | `Audio_interactive of 'media
    | `Video_interactive of 'media
    | `Audio of 'media
    | `Video of 'media
  ]
(* _interactive variants are not used for now *)
type (+'noscript, +'regular, +'media) transparent_without_interactive =
  [
    | `Noscript of 'noscript
    | `Ins of 'regular
    | `Del of 'regular
    | `Object of 'regular
    | `Canvas of 'regular
    | `Map of 'regular
    | `Audio of 'media
    | `Video of 'media
  ]

type (+'interactive, +'regular, +'media) transparent_without_noscript =
  [
    | `A of 'interactive
    | `Ins of 'regular
    | `Del of 'regular
    | `Canvas of 'regular
    | `Map of 'regular
    | `Object of 'regular
    | `Object_interactive of 'regular
    | `Video of 'media
    | `Audio of 'media
    | `Video_interactive of 'media
    | `Audio_interactive of 'media
  ]

type (+'interactive, +'noscript, +'regular) transparent_without_media =
  [
    | `A of 'interactive
    | `Noscript of 'noscript
    | `Ins of 'regular
    | `Del of 'regular
    | `Map of 'regular
    | `Canvas of 'regular
    | `Object of 'regular
    | `Object_interactive of 'regular
  ]

(** Metadata without title *)
type metadata_without_title =
  [
    | `Template
    | `Style
    | `Script
    | `Noscript of [ | `Meta | `Link | `Style ]
    | `Meta
    | `Link
    | `Command
    | `Base
  ]

(** Metadata contents. Used specially in <head> *)
type metadata = [ | metadata_without_title | `Title ]

(** Interactive contents : contents that require user-interaction
        (Forms, link, etc.) *)

(** Core element types are element types without transparent. *)
type core_interactive =
  [
    | `Textarea
    | `Select
    | `Menu
    | `Label
    | `Keygen
    | `Input
    | `Img_interactive
    | `Iframe
    | `Embed
    | `Details
    | `Button
  ]

type interactive =
  [
    core_interactive | (interactive, interactive, interactive) transparent_without_interactive
  ]

(** Phrasing contents is inline contents : bold text, span, and so on. *)
type core_phrasing =
  [
    | labelable
    | submitable
    | `Wbr
    | `Var
    | `U
    | `Svg
    | `Time
    | `Template
    | `Sup
    | `Sub
    | `Strong
    | `Span
    | `Small
    | `Script
    | `Samp
    | `Ruby
    | `Q
    | `Mark
    | `Label
    | `Kbd
    | `Iframe
    | `I
    | `Embed
    | `Em
    | `Dfn
    | `Datalist
    | `Command
    | `Code
    | `Cite
    | `Br
    | `Bdo
    | `B
    | `Abbr
    | `Img | `Img_interactive
    | `PCDATA
  ]

type core_phrasing_without_noscript =
  [
    | labelable
    | submitable
    | `Wbr
    | `Var
    | `U
    | `Time
    | `Template
    | `Sup
    | `Sub
    | `Svg
    | `Strong
    | `Span
    | `Small
    | `Script
    | `Samp
    | `Ruby
    | `Q
    | `Mark
    | `Label
    | `Kbd
    | `Iframe
    | `I
    | `Embed
    | `Em
    | `Dfn
    | `Datalist
    | `Command
    | `Code
    | `Cite
    | `Br
    | `Bdo
    | `Img | `Img_interactive
    | `B
    | `Abbr
    | `PCDATA
  ]
type core_phrasing_without_interactive =
  [
    | labelable_without_interactive
    | `Wbr
    | `Var
    | `U
    | `Img
    | `Time
    | `Template
    | `Sup
    | `Sub
    | `Strong
    | `Span
    | `Small
    | `Script
    | `Svg
    | `Samp
    | `Ruby
    | `Q
    | `Mark
    | `Kbd
    | `Img
    | `I
    | `Em
    | `Dfn
    | `Datalist
    | `Command
    | `Code
    | `Cite
    | `Br
    | `Bdo
    | `B
    | `Abbr
    | `PCDATA
  ]

type core_phrasing_without_media =
  [
    | labelable
    | submitable
    | `Wbr
    | `Var
    | `U
    | `Time
    | `Template
    | `Svg
    | `Sup
    | `Sub
    | `Strong
    | `Span
    | `Small
    | `Script
    | `Samp
    | `Ruby
    | `Q
    | `Mark
    | `Label
    | `Kbd
    | `Img | `Img_interactive
    | `Iframe
    | `I
    | `Embed
    | `Em
    | `Dfn
    | `Datalist
    | `Command
    | `Code
    | `Cite
    | `Br
    | `Bdo
    | `B
    | `Abbr
    | `PCDATA
  ]

type phrasing_without_noscript =
  (phrasing_without_interactive,
   phrasing,
   phrasing_without_media) transparent_without_noscript

and phrasing_without_media =
  [
    | core_phrasing_without_media
    | (phrasing_without_interactive, phrasing_without_noscript, phrasing)
        transparent_without_media
  ]


and phrasing_without_interactive =
  [
    | core_phrasing_without_interactive
    | (phrasing_without_noscript, phrasing, phrasing_without_media)
        transparent_without_interactive
  ]

and phrasing =
  [
    | (phrasing_without_interactive, phrasing_without_noscript, phrasing,
       phrasing_without_media) transparent
    | core_phrasing
  ]

type (+'a, +'b) between_phrasing_and_phrasing_without_interactive =
  ( [<  core_phrasing
    | ([< phrasing_without_interactive] as 'b,
                                           phrasing_without_noscript,
                                           phrasing,
                                           phrasing_without_media) transparent
        > `Abbr `B `Bdo `Br `Canvas `Cite `Code `Command
        `Datalist `Del `Dfn `Em `I `Img `Ins `Kbd `Map `Mark `Meter
        `Noscript `Object `PCDATA `Progress `Q `Ruby `Samp `Script
        `Small `Span `Strong `Sub `Sup `Svg `Template `Time `U `Var `Wbr ] as 'a)

(** Phrasing without the interactive markups *)
type phrasing_without_dfn =
  [
    | labelable
    | submitable
    | `Wbr
    | `Var
    | `U
    | `Time
    | `Template
    | `Sup
    | `Sub
    | `Strong
    | `Span
    | `Small
    | `Script
    | `Samp
    | `Ruby
    | `Q
    | `Mark
    | `Label
    | `Img | `Img_interactive
    | `Kbd
    | `I
    | `Em
    | `Datalist
    | `Command
    | `Code
    | `Cite
    | `Br
    | `Bdo
    | `B
    | `Abbr
    | `PCDATA
    | (phrasing_without_interactive, phrasing_without_noscript,
       phrasing_without_dfn, phrasing_without_media) transparent
  ]

type phrasing_without_label =
  [
    | labelable
    | submitable
    | `Wbr
    | `Var
    | `U
    | `Time
    | `Template
    | `Sup
    | `Sub
    | `Strong
    | `Span
    | `Img | `Img_interactive
    | `Small
    | `Script
    | `Samp
    | `Ruby
    | `Q
    | `Mark
    | `Kbd
    | `I
    | `Em
    | `Dfn
    | `Datalist
    | `Command
    | `Code
    | `Cite
    | `Br
    | `Bdo
    | `B
    | `Abbr
    | `PCDATA
    | (phrasing_without_interactive, phrasing_without_noscript,
       phrasing_without_label, phrasing_without_media) transparent
  ]

type phrasing_without_progress =
  [
    | resetable
    | submitable
    | `Wbr
    | `Var
    | `U
    | `Time
    | `Template
    | `Sup
    | `Sub
    | `Strong
    | `Span
    | `Small
    | `Script
    | `Samp
    | `Img | `Img_interactive
    | `Ruby
    | `Q
    | `Meter
    | `Mark
    | `Label
    | `Kbd
    | `I
    | `Em
    | `Dfn
    | `Datalist
    | `Command
    | `Code
    | `Cite
    | `Button
    | `Br
    | `Bdo
    | `B
    | `Abbr
    | `PCDATA
    | (phrasing_without_interactive, phrasing_without_noscript,
       phrasing_without_progress, phrasing_without_media) transparent
  ]

type phrasing_without_time =
  [
    | labelable
    | submitable
    | `Wbr
    | `Var
    | `U
    | `Template
    | `Sup
    | `Sub
    | `Strong
    | `Img | `Img_interactive
    | `Span
    | `Small
    | `Script
    | `Samp
    | `Ruby
    | `Q
    | `Mark
    | `Label
    | `Kbd
    | `I
    | `Em
    | `Dfn
    | `Datalist
    | `Command
    | `Code
    | `Cite
    | `Br
    | `Bdo
    | `B
    | `Abbr
    | `PCDATA
    | (phrasing_without_interactive, phrasing_without_noscript,
       phrasing_without_time, phrasing_without_media) transparent
  ]

type phrasing_without_meter =
  [
    | submitable
    | resetable
    | `Progress
    | `Button
    | `Wbr
    | `Var
    | `U
    | `Time
    | `Template
    | `Sup
    | `Img | `Img_interactive
    | `Sub
    | `Strong
    | `Span
    | `Small
    | `Script
    | `Samp
    | `Ruby
    | `Q
    | `Mark
    | `Label
    | `Kbd
    | `I
    | `Em
    | `Dfn
    | `Datalist
    | `Command
    | `Code
    | `Cite
    | `Br
    | `Bdo
    | `B
    | `Abbr
    | `PCDATA
    | (phrasing_without_interactive, phrasing_without_noscript,
       phrasing_without_meter, phrasing_without_media) transparent
  ]

type core_flow5 =
  [
    | core_phrasing
    | formassociated
    | formatblock
    | `Ul
    | `Table
    | `Style
    | `Ol
    | `Menu
    | `Hr
    | `Form
    | `Figure
    | `Dl
    | `Details
    | `Main
  ]

type core_flow5_without_interactive =
  [
    | core_phrasing_without_interactive
    | formassociated
    | formatblock
    | `Ul
    | `Table
    | `Style
    | `Ol
    | `Menu
    | `Hr
    | `Form
    | `Figure
    | `Dl
    | `Main
  ]

type core_flow5_without_noscript =
  [
    | core_phrasing_without_noscript
    | formassociated
    | formatblock
    | `Ul
    | `Table
    | `Style
    | `Ol
    | `Menu
    | `Hr
    | `Form
    | `Figure
    | `Dl
    | `Details
    | `Main
  ]
type core_flow5_without_media =
  [
    | core_phrasing_without_media
    | formassociated
    | formatblock
    | `Ul
    | `Table
    | `Style
    | `Ol
    | `Menu
    | `Hr
    | `Form
    | `Figure
    | `Dl
    | `Details
    | `Main
  ]

type flow5_without_interactive =
  [
    core_flow5_without_interactive
  | (flow5_without_noscript, flow5, flow5_without_media)
      transparent_without_interactive
  ]

and flow5_without_noscript =
  [ | core_flow5_without_noscript
    | (flow5_without_interactive,
       flow5,
       flow5_without_media) transparent_without_noscript
  ]

and flow5_without_media =
  [ core_flow5_without_media
  | (flow5_without_interactive,
     flow5_without_noscript,
     flow5) transparent_without_media ]
and flow5 =
  [
    | core_flow5
    | (flow5_without_interactive, flow5_without_noscript, flow5,
       flow5_without_media) transparent
  ]

type flow5_without_table =
  [
    | core_phrasing
    | formassociated
    | formatblock
    | `Ul
    | `Style
    | `Ol
    | `Menu
    | `Hr
    | `Form
    | `Figure
    | `Dl
    | `Details
    | `Main
    | (flow5_without_interactive, flow5_without_noscript, flow5,
       flow5_without_media) transparent
  ]

type flow5_without_interactive_header_footer =
  [
    | heading
    | sectioning
    | `Pre
    | `P
    | `Div
    | `Blockquote
    | `Address
    | core_phrasing_without_interactive
    | formassociated
    | `Ul
    | `Table
    | `Style
    | `Ol
    | `Menu
    | `Hr
    | `Form
    | `Figure
    | `Dl
    | `Main
    | (flow5_without_noscript, flow5, flow5_without_media)
        transparent_without_interactive
  ]

type flow5_without_header_footer =
  [
    | heading
    | sectioning
    | `Pre
    | `P
    | `Div
    | `Blockquote
    | `Address
    | core_phrasing
    | formassociated
    | `Ul
    | `Table
    | `Style
    | `Ol
    | `Menu
    | `Hr
    | `Form
    | `Figure
    | `Dl
    | `Details
    | `Main
    | (flow5_without_interactive_header_footer,
       flow5_without_noscript, flow5,
       flow5_without_media) transparent
  ]

type +'a between_flow5_and_flow5_without_interactive_header_footer =
  [< flow5  > `Abbr `Address `Article `Aside `Audio `B `Bdo `Blockquote `Br
       `Button `Canvas `Cite `Code `Command `Datalist `Del `Dfn `Div `Dl `Em
       `Fieldset `Figure `Form `H1 `H2 `H3 `H4 `H5 `H6 `Hgroup `Hr `I `Img
       `Input `Ins `Kbd `Keygen `Label `Map`Mark `Menu `Meter `Nav `Noscript
       `Object `Ol `Output `P `PCDATA `Pre `Progress `Q `Ruby `Samp `Script
       `Section `Select `Small `Span `Strong `Style `Sub `Sup `Svg `Table
       `Template `Textarea `Time `U `Ul `Var `Video `Wbr] as 'a

type (+'a, +'b) between_flow5_and_flow5_without_header_footer =
  [< core_flow5
  | ([< flow5_without_interactive ] as 'b,
                                       flow5_without_noscript, 'a,
                                       flow5_without_media)
      transparent
      > `A `Abbr `Address `Article `Aside `Audio `Audio_interactive `B
      `Bdo `Blockquote `Br `Button `Canvas `Cite `Code `Command
      `Datalist `Del `Details `Dfn `Div `Dl `Em `Embed `Fieldset
      `Figure `Form `H1 `H2 `H3 `H4 `H5 `H6 `Hgroup `Hr `I `Iframe
      `Img `Img_interactive `Input `Ins `Kbd `Keygen `Label `Map
      `Mark `Menu `Meter `Nav `Noscript `Object `Object_interactive
      `Ol `Output `P `PCDATA `Pre `Progress `Q `Ruby `Samp `Script
      `Section `Select `Small `Span `Strong `Style `Sub `Sup `Svg
      `Table `Template `Textarea `Time `U `Ul `Var `Video `Video_interactive
      `Wbr ] as 'a

type flow5_without_form =
  [
    | core_phrasing
    | formassociated
    | formatblock
    | `Ul
    | `Table
    | `Style
    | `Ol
    | `Menu
    | `Hr
    | `Figure
    | `Dl
    | `Details
    | `Main
    | (flow5_without_interactive, flow5_without_noscript, flow5,
       flow5_without_media) transparent
  ]

type flow5_without_sectioning_heading_header_footer_address =
  [
    | core_phrasing
    | formassociated
    | `Pre
    | `P
    | `Div
    | `Blockquote
    | `Ul
    | `Table
    | `Style
    | `Ol
    | `Menu
    | `Hr
    | `Form
    | `Figure
    | `Dl
    | `Details
    | `Main
    | (flow5_without_interactive, flow5_without_noscript, flow5,
       flow5_without_media) transparent
  ]

type flow5_without_sectioning_heading_header_footer =
  [
    | flow5_without_sectioning_heading_header_footer_address
    | `Address
  ]

(*
  Type for HTML for elements
*)
type pcdata = [ | `PCDATA ]
type txt = [ | `PCDATA ]

type notag

type no_attribute_allowed

type noattrib = [ `No_attribute_allowed of no_attribute_allowed ]

type html = [ | `Html ]

type html_content_fun = [ | `Head | `Body ]

type html_content = html_content_fun

type html_attrib = [ | common | `Manifest ]

type head = [ | `Head ]

type head_content = [ | metadata ]

type head_content_fun = [ | metadata_without_title ]

type head_attrib = [ | common ]

type body = [ | `Body ]

type body_attrib =
  [
    | common
    | `OnAfterPrint
    | `OnBeforePrint
    | `OneBeforeUnload
    | `OnHashChange
    | `OnMessage
    | `OnOffLine
    | `OnOnLine
    | `OnPageHide
    | `OnPageShow
    | `OnPopState
    | `OnRedo
    | `OnResize
    | `OnStorage
    | `OnUndo
    | `OnUnload
  ]

type body_content = flow5

type body_content_fun = flow5


type svg = [ `Svg ]
type svg_content = Svg_types.svg_content
type svg_attrib = Svg_types.svg_attr
(* NAME: base, KIND: nullary, TYPE: [= common | `Href | `Target], [= `Base ], ARG: notag, ATTRIB:  OUT: [= `Base ] *)
type base = [ | `Base ]

type base_content = notag

type base_content_fun = notag

type base_attrib = [ | common | `Href | `Target ]

type title = [ | `Title ]

type title_content = [ | `PCDATA ]

type title_content_fun = [ | `PCDATA ]

type title_attrib = noattrib

(* NAME: footer, KIND: star, TYPE: [= common ], [= flow5_without_header_footer ], [=`Footer], ARG: [= flow5_without_header_footer ], ATTRIB:  OUT: [=`Footer] *)
type footer = [ | `Footer ]

type footer_content = [ | flow5_without_header_footer ]

type footer_content_fun = [ | flow5_without_header_footer ]

type footer_attrib = [ | common ]

(* NAME: header, KIND: star, TYPE: [= common ], [= flow5_without_header_footer ], [=`Header], ARG: [= flow5_without_header_footer ], ATTRIB:  OUT: [=`Header] *)
type header = [ | `Header ]

type header_content = [ | flow5_without_header_footer ]

type header_content_fun = [ | flow5_without_header_footer ]

type header_attrib = [ | common ]

(* NAME: section, KIND: star, TYPE: [= common ], [= flow5 ], [=`Section], ARG: [= flow5 ], ATTRIB:  OUT: [=`Section] *)
type section = [ | `Section ]

type section_content = [ | flow5 ]

type section_content_fun = [ | flow5 ]

type section_attrib = [ | common ]

(* NAME: nav, KIND: star, TYPE: [= common ], [= flow5 ], [=`Nav], ARG: [= flow5 ], ATTRIB:  OUT: [=`Nav] *)
type nav = [ | `Nav ]

type nav_content = [ | flow5 ]

type nav_content_fun = [ | flow5 ]

type nav_attrib = [ | common ]

(* NAME: h, KIND: star, TYPE: [= common ], [= phrasing ], [=`H1], ARG: [= phrasing ], ATTRIB:  OUT: [=`H1] *)
type h1 = [ | `H1 ]

type h1_content = [ | phrasing ]

type h1_content_fun = [ | phrasing ]

type h1_attrib = [ | common ]

(* NAME: h, KIND: star, TYPE: [= common ], [= phrasing ], [=`H2], ARG: [= phrasing ], ATTRIB:  OUT: [=`H2] *)
type h2 = [ | `H2 ]

type h2_content = [ | phrasing ]

type h2_content_fun = [ | phrasing ]

type h2_attrib = [ | common ]

(* NAME: h, KIND: star, TYPE: [= common ], [= phrasing ], [=`H3], ARG: [= phrasing ], ATTRIB:  OUT: [=`H3] *)
type h3 = [ | `H3 ]

type h3_content = [ | phrasing ]

type h3_content_fun = [ | phrasing ]

type h3_attrib = [ | common ]

(* NAME: h, KIND: star, TYPE: [= common ], [= phrasing ], [=`H4], ARG: [= phrasing ], ATTRIB:  OUT: [=`H4] *)
type h4 = [ | `H4 ]

type h4_content = [ | phrasing ]

type h4_content_fun = [ | phrasing ]

type h4_attrib = [ | common ]

(* NAME: h, KIND: star, TYPE: [= common ], [= phrasing ], [=`H5], ARG: [= phrasing ], ATTRIB:  OUT: [=`H5] *)
type h5 = [ | `H5 ]

type h5_content = [ | phrasing ]

type h5_content_fun = [ | phrasing ]

type h5_attrib = [ | common ]

(* NAME: h, KIND: star, TYPE: [= common ], [= phrasing ], [=`H6], ARG: [= phrasing ], ATTRIB:  OUT: [=`H6] *)
type h6 = [ | `H6 ]

type h6_content = [ | phrasing ]

type h6_content_fun = [ | phrasing ]

type h6_attrib = [ | common ]

(* NAME: hgroup, KIND: plus, TYPE: [= common ], [= `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ], [=`Hgroup], ARG: [= `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ], ATTRIB:  OUT: [=`Hgroup] *)
type hgroup = [ | `Hgroup ]

type hgroup_content = [ | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ]

type hgroup_content_fun = [ | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 ]

type hgroup_attrib = [ | common ]

(* NAME: address, KIND: star, TYPE: [= common ], [= flow5_without_sectioning_heading_header_footer_address ], [=`Address], ARG: [= flow5_without_sectioning_heading_header_footer_address ], ATTRIB:  OUT: [=`Address] *)
type address = [ | `Address ]

type address_content =
  [ | flow5_without_sectioning_heading_header_footer_address
  ]

type address_content_fun =
  [ | flow5_without_sectioning_heading_header_footer_address
  ]

type address_attrib = [ | common ]

(* NAME: article, KIND: star, TYPE: [= common ], [= flow5 ], [=`Article], ARG: [= flow5 ], ATTRIB:  OUT: [=`Article] *)
type article = [ | `Article ]

type article_content = [ | flow5 ]

type article_content_fun = [ | flow5 ]

type article_attrib = [ | common ]

(* NAME: aside, KIND: star, TYPE: [= common ], [= flow5 ], [=`Aside], ARG: [= flow5 ], ATTRIB:  OUT: [=`Aside] *)
type aside = [ | `Aside ]

type aside_content = [ | flow5 ]

type aside_content_fun = [ | flow5 ]

type aside_attrib = [ | common ]

(* NAME: main, KIND: star, TYPE: [= common ], [= flow5 ], [=`Main], ARG: [= flow5 ], ATTRIB:  OUT: [=`Main] *)
type main = [ | `Main ]

type main_content = [ | flow5 ]

type main_content_fun = [ | flow5 ]

type main_attrib = [ | common ]

(* NAME: p, KIND: star, TYPE: [= common ], [=phrasing ], [=`P], ARG: [=phrasing ], ATTRIB:  OUT: [=`P] *)
type p = [ | `P ]

type p_content = [ | phrasing ]

type p_content_fun = [ | phrasing ]

type p_attrib = [ | common ]

(* NAME: pre, KIND: star, TYPE: [= common ],[= phrasing ], [=`Pre], ARG: [= phrasing ], ATTRIB:  OUT: [=`Pre] *)
type pre = [ | `Pre ]

type pre_content = [ | phrasing ]

type pre_content_fun = [ | phrasing ]

type pre_attrib = [ | common ]

(* NAME: blockquote, KIND: star, TYPE: [= common | `Cite ],[= flow5 ], [=`Blockquote], ARG: [= flow5 ], ATTRIB:  OUT: [=`Blockquote] *)
type blockquote = [ | `Blockquote ]

type blockquote_content = [ | flow5 ]

type blockquote_content_fun = [ | flow5 ]

type blockquote_attrib = [ | common | `Cite ]

(* NAME: div, KIND: star, TYPE: [= common ], [= flow5 ], [=`Div], ARG: [= flow5 ], ATTRIB:  OUT: [=`Div] *)
type div = [ | `Div ]

type div_content = [ | flow5 ]

type div_content_fun = [ | flow5 ]

type div_attrib = [ | common ]

(* NAME: ol, KIND: star, TYPE: [= common | `Reserved |`Start ], [= `Li of [= common | `Int_Value ]], [=`Ol], ARG: [= `Li of [= common | `Int_Value ]], ATTRIB:  OUT: [=`Ol] *)
type ol = [ | `Ol ]

type ol_content = [ | `Li of [ | common | `Int_Value ] ]

type ol_content_fun = [ | `Li of [ | common | `Int_Value ] ]

type ol_attrib = [ | common | `Reversed | `Start ]

(* NAME: li, KIND: star, TYPE: [= common | `Int_Value] as 'a, [=flow5 ], [=`Li of 'a], ARG: [=flow5 ], ATTRIB:  OUT: [=`Li of 'a] *)
type li_content = [ | flow5 ]

type li_content_fun = [ | flow5 ]

type li_attrib = [ | common | `Int_Value ]

type li = [ | `Li of li_attrib ]
(* NAME: ul, KIND: star, TYPE: [= common ], [= `Li of [= common] ], [=`Ul], ARG: [= `Li of [= common] ], ATTRIB:  OUT: [=`Ul] *)
type ul = [ | `Ul ]

type ul_content = [ | `Li of [ | li_attrib ] ]

type ul_content_fun = [ | `Li of [ | li_attrib ] ]

type ul_attrib = [ | common ]

(* NAME: dd, KIND: star, TYPE: [= common ], [= flow5 ], [=`Dd], ARG: [= flow5 ], ATTRIB:  OUT: [=`Dd] *)
type dd = [ | `Dd ]

type dd_content = [ | flow5 ]

type dd_content_fun = [ | flow5 ]

type dd_attrib = [ | common ]

(* NAME: dt, KIND: star, TYPE: [= common ], [= phrasing], [=`Dt], ARG: [= phrasing], ATTRIB:  OUT: [=`Dt] *)
type dt = [ | `Dt ]

type dt_content = [ | flow5_without_sectioning_heading_header_footer ]

type dt_content_fun = [ | flow5_without_sectioning_heading_header_footer ]

type dt_attrib = [ | common ]


type dl = [ | `Dl ]

type dl_content = [ | `Dt | `Dd ]

type dl_content_fun = [ | `Dt | `Dd ]

type dl_attrib = [ | common ]


(* NAME: figcaption, KIND: star, TYPE: [= common ], [= flow5], [=`Figcaption], ARG: [= flow5], ATTRIB:  OUT: [=`Figcaption] *)
type figcaption = [ | `Figcaption ]

type figcaption_content = [ | flow5 ]

type figcaption_content_fun = [ | flow5 ]

type figcaption_attrib = [ | common ]


(* figure *)
type figure = [ | `Figure ]

type figure_content = [ | flow5 ]

type figure_content_fun = [ | flow5 ]

type figure_attrib = [ | common ]


(* Rp, Rt and ruby *)
type rp = [ | `Rp ]
type rp_content = [ | phrasing ]
type rp_content_fun = [ | phrasing ]
type rp_attrib = [ | common ]

type rt = [ | `Rt ]
type rt_content = [ | phrasing ]
type rt_content_fun = [ | phrasing ]
type rt_attrib = [ | common ]

type ruby = [ | `Ruby ]
type ruby_content = [ | phrasing | rp | rt ]
type ruby_content_fun = [ | phrasing | rp | rt ]
type ruby_attrib = [ | common ]


(* NAME: hr, KIND: nullary, TYPE: [= common ], [=`Hr], ARG: notag, ATTRIB:  OUT: [=`Hr] *)
type hr = [ | `Hr ]

type hr_content = notag

type hr_content_fun = notag

type hr_attrib = [ | common ]

(* NAME: b, KIND: star, TYPE: [= common ], [= phrasing ], [=`B], ARG: [= phrasing ], ATTRIB:  OUT: [=`B] *)
type b = [ | `B ]

type b_content = [ | phrasing ]

type b_content_fun = [ | phrasing ]

type b_attrib = [ | common ]

(* NAME: i, KIND: star, TYPE: [= common ], [= phrasing ], [=`I], ARG: [= phrasing ], ATTRIB:  OUT: [=`I] *)
type i = [ | `I ]

type i_content = [ | phrasing ]

type i_content_fun = [ | phrasing ]

type i_attrib = [ | common ]

(* NAME: u, KIND: star, TYPE: [= common ], [= phrasing ], [=`U], ARG: [= phrasing ], ATTRIB:  OUT: [=`U] *)
type u = [ | `U ]

type u_content = [ | phrasing ]

type u_content_fun = [ | phrasing ]

type u_attrib = [ | common ]

(* NAME: small, KIND: star, TYPE: [= common ], [= phrasing ], [=`Small], ARG: [= phrasing ], ATTRIB:  OUT: [=`Small] *)
type small = [ | `Small ]

type small_content = [ | phrasing ]

type small_content_fun = [ | phrasing ]

type small_attrib = [ | common ]

(* NAME: sub, KIND: star, TYPE: [= common ], [= phrasing ], [=`Sub], ARG: [= phrasing ], ATTRIB:  OUT: [=`Sub] *)
type sub = [ | `Sub ]

type sub_content = [ | phrasing ]

type sub_content_fun = [ | phrasing ]

type sub_attrib = [ | common ]

(* NAME: sup, KIND: star, TYPE: [= common ], [= phrasing ], [=`Sup], ARG: [= phrasing ], ATTRIB:  OUT: [=`Sup] *)
type sup = [ | `Sup ]

type sup_content = [ | phrasing ]

type sup_content_fun = [ | phrasing ]

type sup_attrib = [ | common ]

(* NAME: mark, KIND: star, TYPE: [= common ],[= phrasing ],[= `Mark ], ARG: [= phrasing ], ATTRIB:  OUT: [= `Mark ] *)
type mark = [ | `Mark ]

type mark_content = [ | phrasing ]

type mark_content_fun = [ | phrasing ]

type mark_attrib = [ | common ]

(* NAME: wbr, KIND: nullary, TYPE: [= common ],[= `Wbr ], ARG: notag, ATTRIB:  OUT: [= `Wbr ] *)
type wbr = [ | `Wbr ]

type wbr_content = notag

type wbr_content_fun = notag

type wbr_attrib = [ | common ]

(* NAME: bdo, KIND: star, TYPE: [= common ],[= phrasing ],[= `Bdo ], ARG: [= phrasing ], ATTRIB:  OUT: [= `Bdo ] *)
type bdo = [ | `Bdo ]

type bdo_content = [ | phrasing ]

type bdo_content_fun = [ | phrasing ]

type bdo_attrib = [ | common ]

(* NAME: abbr, KIND: star, TYPE: [= common ], [=phrasing ], [=`Abbr], ARG: [=phrasing ], ATTRIB:  OUT: [=`Abbr] *)
type abbr = [ | `Abbr ]

type abbr_content = [ | phrasing ]

type abbr_content_fun = [ | phrasing ]

type abbr_attrib = [ | common ]

(* NAME: br, KIND: nullary, TYPE: [= common ], [=`Br], ARG: notag, ATTRIB:  OUT: [=`Br] *)
type br = [ | `Br ]

type br_content = notag

type br_content_fun = notag

type br_attrib = [ | common ]

(* NAME: cite, KIND: star, TYPE: [= common ], [= phrasing ], [=`Cite], ARG: [= phrasing ], ATTRIB:  OUT: [=`Cite] *)
type cite = [ | `Cite ]

type cite_content = [ | phrasing ]

type cite_content_fun = [ | phrasing ]

type cite_attrib = [ | common ]

(* NAME: code, KIND: star, TYPE: [= common ], [= phrasing ], [=`Code], ARG: [= phrasing ], ATTRIB:  OUT: [=`Code] *)
type code = [ | `Code ]

type code_content = [ | phrasing ]

type code_content_fun = [ | phrasing ]

type code_attrib = [ | common ]

(* NAME: dfn, KIND: star, TYPE: [= common ], [= phrasing_without_dfn ], [=`Dfn], ARG: [= phrasing_without_dfn ], ATTRIB:  OUT: [=`Dfn] *)
type dfn = [ | `Dfn ]

type dfn_content = [ | phrasing_without_dfn ]

type dfn_content_fun = [ | phrasing_without_dfn ]

type dfn_attrib = [ | common ]

(* NAME: em, KIND: star, TYPE: [= common ], [= phrasing ], [=`Em], ARG: [= phrasing ], ATTRIB:  OUT: [=`Em] *)
type em = [ | `Em ]

type em_content = [ | phrasing ]

type em_content_fun = [ | phrasing ]

type em_attrib = [ | common ]

(* NAME: kbd, KIND: star, TYPE: [= common ], [= phrasing ], [=`Kbd], ARG: [= phrasing ], ATTRIB:  OUT: [=`Kbd] *)
type kbd = [ | `Kbd ]

type kbd_content = [ | phrasing ]

type kbd_content_fun = [ | phrasing ]

type kbd_attrib = [ | common ]

(* NAME: q, KIND: star, TYPE: [= common | `Cite ], [= phrasing ], [=`Q], ARG: [= phrasing ], ATTRIB:  OUT: [=`Q] *)
type q = [ | `Q ]

type q_content = [ | phrasing ]

type q_content_fun = [ | phrasing ]

type q_attrib = [ | common | `Cite ]

(* NAME: samp, KIND: star, TYPE: [= common ], [= phrasing ], [=`Samp], ARG: [= phrasing ], ATTRIB:  OUT: [=`Samp] *)
type samp = [ | `Samp ]

type samp_content = [ | phrasing ]

type samp_content_fun = [ | phrasing ]

type samp_attrib = [ | common ]

(* NAME: span, KIND: star, TYPE: [= common ], [= phrasing ], [=`Span], ARG: [= phrasing ], ATTRIB:  OUT: [=`Span] *)
type span = [ | `Span ]

type span_content = [ | phrasing ]

type span_content_fun = [ | phrasing ]

type span_attrib = [ | common ]

(* NAME: strong, KIND: star, TYPE: [= common ], [= phrasing ], [=`Strong], ARG: [= phrasing ], ATTRIB:  OUT: [=`Strong] *)
type strong = [ | `Strong ]

type strong_content = [ | phrasing ]

type strong_content_fun = [ | phrasing ]

type strong_attrib = [ | common ]

(* NAME: time, KIND: star, TYPE: [= common |`Datetime |`Pubdate], [= phrasing_without_time ], [=`Time], ARG: [= phrasing_without_time ], ATTRIB:  OUT: [=`Time] *)
type time = [ | `Time ]

type time_content = [ | phrasing_without_time ]

type time_content_fun = [ | phrasing_without_time ]

type time_attrib = [ | common | `Datetime | `Pubdate ]

(* NAME: var, KIND: star, TYPE: [= common ], [= phrasing ], [=`Var], ARG: [= phrasing ], ATTRIB:  OUT: [=`Var] *)
type var = [ | `Var ]

type var_content = [ | phrasing ]

type var_content_fun = [ | phrasing ]

type var_attrib = [ | common ]

(* NAME: a, KIND: star, TYPE: [= common | `Href | `Hreflang | `Media | `Rel | `Target | `Mime_type ], 'a, [= `A of 'a ], ARG: 'a, ATTRIB:  OUT: [= `A of 'a ] *)
type a_content = flow5_without_interactive

type a_content_fun = flow5_without_interactive

type 'a a = [ | `A of 'a ]
type a_ = [ `A of a_content ] (* should not be used as it may break *)
type a_attrib =
  [ | common | `Href | `Hreflang | `Media | `Rel | `Target | `Mime_type
    | `Download
  ]

(* NAME: del, KIND: star, TYPE: [= common | `Cite | `Datetime ], 'a,[=`Del of 'a], ARG: 'a, ATTRIB:  OUT: [=`Del of 'a] *)
type 'a del = [ | `Del of 'a ]
type del_content = flow5
type del_ = del_content del
type del_content_fun = flow5

type del_attrib = [ | common | `Cite | `Datetime ]

(* NAME: ins, KIND: star, TYPE: [= common | `Cite | `Datetime ],'a ,[=`Ins of 'a], ARG: 'a , ATTRIB:  OUT: [=`Ins of 'a] *)
type 'a ins = [ | `Ins of 'a ]

type ins_content = flow5
type ins_ = ins_content ins
type ins_content_fun = flow5

type ins_attrib = [ | common | `Cite | `Datetime ]

(* NAME: iframe, KIND: ndbox, TYPE: *| `Srcdoc*, ARG: , ATTRIB:  OUT:  *)
type iframe = [ | `Iframe ]

type iframe_content = [ | `PCDATA ]

type iframe_content_fun = [ | `PCDATA ]

type iframe_attrib =
  [
    | common
    | `Allowfullscreen
    | `Allowpaymentrequest
    | `Src
    | (*| `Srcdoc*)
      `Name
    | `Sandbox
    | `Seamless
    | `Width
    | `Height
    | `Referrerpolicy
  ]

type object__content = [ | flow5 | `Param ]

type object__content_fun = flow5

type 'a object_ = [ | `Object of 'a | `Object_interactive of 'a]
type object__ = object__content object_
type object__attrib =
  [
    | common
    | `Data
    | `Form
    | `Mime_type
    | `Height
    | `Width
    | `Name
    | `Usemap
  ]

(* NAME: param, KIND: nullary, TYPE: [= common | `Name | `Text_Value ],[= `Param ], ARG: notag, ATTRIB:  OUT: [= `Param ] *)
type param = [ | `Param ]

type param_content = notag

type param_content_fun = notag

type param_attrib = [ | common | `Name | `Text_Value ]

(* NAME: embed, KIND: nullary, TYPE: [= common | `Src | `Height | `Mime_type | `Width], [=`Embed], ARG: notag, ATTRIB:  OUT: [=`Embed] *)
type embed = [ | `Embed ]

type embed_content = notag

type embed_content_fun = notag

type embed_attrib = [ | common | `Src | `Height | `Mime_type | `Width ]

type img = [ `Img ]
type img_interactive = [ `Img | `Img_interactive ]
type img_content = notag
type img_content_fun = notag
type img_attrib = [ | common | `Height | `Ismap | `Width | `Srcset | `Img_sizes]

(* Attributes used by audio and video. *)
type media_attrib =
  [ | `Crossorigin
    | `Preload
    | `Autoplay
    | `Mediagroup
    | `Loop
    | `Muted
    | `Controls
  ]

type 'a audio = [ | `Audio of 'a ]
type 'a audio_interactive = [ | `Audio of 'a | `Audio_interactive of 'a ]

type audio_content = flow5_without_media
type audio_ = audio_content audio
type audio_content_fun = flow5_without_media

type audio_attrib =
  [ | common
    | media_attrib
  ]


type 'a video = [ | `Video of 'a ]
type 'a video_interactive = [ | `Video of 'a | `Video_interactive of 'a ]

type video_content = flow5_without_media
type video_ = video_content video
type video_content_fun = flow5_without_media

type video_attrib =
  [ | common
    | media_attrib
    | `Poster
    | `Width
    | `Height
  ]

(* NAME: canvas, KIND: star, TYPE: [= common |`Width |`Height],'a, [=`Canvas of 'a], ARG: 'a, ATTRIB:  OUT: [=`Canvas of 'a] *)
type 'a canvas = [ | `Canvas of 'a ]

type canvas_content = flow5
type canvas_ = canvas_content canvas
type canvas_content_fun = flow5

type canvas_attrib = [ | common | `Width | `Height ]

(* NAME: source, KIND: nullary, TYPE: [= common |`Src |`Mime_type |`Media ], [=`Source], ARG: notag, ATTRIB:  OUT: [=`Source] *)
type source = [ | `Source ]

type source_content = notag

type source_content_fun = notag

type source_attrib = [ | common | `Src | `Mime_type | `Media ]

(* NAME: area, KIND: nullary, TYPE: [= common | `Alt | `Coords | `Shape| `Target | `Rel | `Media| `Hreflang | `Mime_type],[=`Area], ARG: notag, ATTRIB:  OUT: [=`Area] *)
type area = [ | `Area ]

type area_content = notag

type area_content_fun = notag

type area_attrib =
  [
    | common
    | `Alt
    | `Coords
    | `Shape
    | `Target
    | `Rel
    | `Media
    | `Hreflang
    | `Mime_type
    | `Download
  ]

(* NAME: map, KIND: plus, TYPE: [=common | `Name ],'a, [=`Map of 'a], ARG: 'a, ATTRIB:  OUT: [=`Map of 'a] *)
type 'a map = [ | `Map of 'a ]

type map_content = flow5
type map_ = map_content map

type map_content_fun = flow5

type map_attrib = [ | common | `Name ]

(* NAME: caption, KIND: star, TYPE: [= common ], [= flow5_without_table], [=`Caption], ARG: [= flow5_without_table], ATTRIB:  OUT: [=`Caption] *)
type caption = [ | `Caption ]

type caption_content = [ | flow5_without_table ]

type caption_content_fun = [ | flow5_without_table ]

type caption_attrib = [ | common ]

(* NAME: table, KIND: plus, TYPE: [= common | `Summary ], [= `Tr ], [=`Table], ARG: [= `Tr ], ATTRIB:  OUT: [=`Table] *)
type table = [ | `Table ]

type table_content = [ | `Tr ]

type table_content_fun = [ | `Tr ]

type table_attrib = [ | common | `Summary ]

(* NAME: tablex, KIND: star, TYPE: [= common | `Summary ], [= `Tbody ], [=`Table], ARG: [= `Tbody ], ATTRIB:  OUT: [=`Table] *)
type tablex = [ | `Table ]

type tablex_content = [ | `Tbody ]

type tablex_content_fun = [ | `Tbody ]

type tablex_attrib = [ | common | `Summary ]

(* NAME: colgroup, KIND: star, TYPE: [= common | `Span ],[= `Col ], [=`Colgroup], ARG: [= `Col ], ATTRIB:  OUT: [=`Colgroup] *)
type colgroup = [ | `Colgroup ]

type colgroup_content = [ | `Col ]

type colgroup_content_fun = [ | `Col ]

type colgroup_attrib = [ | common | `Span ]

(* NAME: col, KIND: nullary, TYPE: [= common | `Span], [=`Col], ARG: notag, ATTRIB:  OUT: [=`Col] *)
type col = [ | `Col ]

type col_content = notag

type col_content_fun = notag

type col_attrib = [ | common | `Span ]

(* NAME: thead, KIND: star, TYPE: [= common],[= `Tr ], [=`Thead], ARG: [= `Tr ], ATTRIB:  OUT: [=`Thead] *)
type thead = [ | `Thead ]

type thead_content = [ | `Tr ]

type thead_content_fun = [ | `Tr ]

type thead_attrib = [ | common ]

(* NAME: tbody, KIND: star, TYPE: [= common],[= `Tr ], [=`Tbody], ARG: [= `Tr ], ATTRIB:  OUT: [=`Tbody] *)
type tbody = [ | `Tbody ]

type tbody_content = [ | `Tr ]

type tbody_content_fun = [ | `Tr ]

type tbody_attrib = [ | common ]

(* NAME: tfoot, KIND: star, TYPE: [= common],[= `Tr ], [=`Tfoot], ARG: [= `Tr ], ATTRIB:  OUT: [=`Tfoot] *)
type tfoot = [ | `Tfoot ]

type tfoot_content = [ | `Tr ]

type tfoot_content_fun = [ | `Tr ]

type tfoot_attrib = [ | common ]

(* NAME: td, KIND: star, TYPE: [= common | `Colspan | `Headers | `Rowspan ], [= flow5 ], [=`Td], ARG: [= flow5 ], ATTRIB:  OUT: [=`Td] *)
type td = [ | `Td ]

type td_content = [ | flow5 ]

type td_content_fun = [ | flow5 ]

type td_attrib = [ | common | `Colspan | `Headers | `Rowspan ]

(* NAME: th, KIND: star, TYPE: [= common | `Colspan | `Headers | `Rowspan | `Scope], [= flow5], [=`Th], ARG: [= flow5], ATTRIB:  OUT: [=`Th] *)
type th = [ | `Th ]

type th_content = [ | flow5 ]

type th_content_fun = [ | flow5 ]

type th_attrib = [ | common | `Colspan | `Headers | `Rowspan | `Scope ]

(* NAME: tr, KIND: star, TYPE: [= common ],[= `Td | `Th ], [=`Tr], ARG: [= `Td | `Th ], ATTRIB:  OUT: [=`Tr] *)
type tr = [ | `Tr ]

type tr_content = [ | `Td | `Th ]

type tr_content_fun = [ | `Td | `Th ]

type tr_attrib = [ | common ]

(* NAME: form, KIND: plus, TYPE: [= common |`Accept_charset | `Action | `Enctype | `Method | `Name | `Target | `Autocomplete | `Novalidate ], [= flow5_without_form ], [=`Form], ARG: [= flow5_without_form ], ATTRIB:  OUT: [=`Form] *)
type form = [ | `Form ]

type form_content = [ | flow5_without_form ]

type form_content_fun = [ | flow5_without_form ]

type form_attrib =
  [
    | common
    | `Accept_charset
    | `Action
    | `Enctype
    | `Method
    | `Name
    | `Target
    | `Autocomplete
    | `Novalidate
  ]

(* NAME: fieldset, KIND: star, TYPE: [= common | `Disabled | `Form | `Name], [= flow5 ], [=`Fieldset], ARG: [= flow5 ], ATTRIB:  OUT: [=`Fieldset] *)
type fieldset = [ | `Fieldset ]

type fieldset_content = [ | flow5 ]

type fieldset_content_fun = [ | flow5 ]

type fieldset_attrib = [ | common | `Disabled | `Form | `Name ]

(* NAME: legend, KIND: star, TYPE: [= common ],[= phrasing], [=`Legend], ARG: [= phrasing], ATTRIB:  OUT: [=`Legend] *)
type legend = [ | `Legend ]

type legend_content = [ | phrasing ]

type legend_content_fun = [ | phrasing ]

type legend_attrib = [ | common ]

(* NAME: label, KIND: star, TYPE: [= common | `Label_for | `Form ],[= phrasing_without_label], [=`Label], ARG: [= phrasing_without_label], ATTRIB:  OUT: [=`Label] *)
type label = [ | `Label ]

type label_content = [ | phrasing_without_label ]

type label_content_fun = [ | phrasing_without_label ]

type label_attrib = [ | common | `Label_for | `Form ]

(* NAME: input, KIND: nullary, TYPE: [= input_attr ], [=`Input], ARG: notag, ATTRIB:  OUT: [=`Input] *)
type input = [ | `Input ]

type input_content = notag

type input_content_fun = notag

type input_attrib =
  [
    | common
    | `Accept
    | `Alt
    | `Autocomplete
    | `Autofocus
    | `Checked
    | `Disabled
    | `Form
    | `Formation
    | `Formenctype
    | `Method
    | `Formnovalidate
    | `Formtarget
    | `Height
    | `List
    | `Input_Max
    | `Maxlength
    | `Minlength
    | `Input_Min
    | `Multiple
    | `Name
    | `Pattern
    | `Placeholder
    | `ReadOnly
    | `Required
    | `Size
    | `Src
    | `Step
    | `Input_Type
    | `Value
    | `Width
    | `Inputmode
  ]

type textarea = [ | `Textarea ]

type textarea_attrib =
  [
    | common
    | `Autofocus
    | `Disabled
    | `Form
    | `Maxlength
    | `Minlength
    | `Name
    | `Placeholder
    | `ReadOnly
    | `Required
    | `Wrap
    | `Rows
    | `Cols
  ]

type textarea_content = [ | `PCDATA ]

type textarea_content_fun = textarea_content

(* NAME: button, KIND: star, TYPE:  [= button_attr ], [= phrasing_without_interactive ], [=`Button], ARG: [= phrasing_without_interactive ], ATTRIB:  OUT: [=`Button] *)
type button = [ | `Button ]

type button_content = [ | phrasing_without_interactive ]

type button_content_fun = [ | phrasing_without_interactive ]

type button_attrib =
  [
    | common
    | `Autofocus
    | `Disabled
    | `Form
    | `Formaction
    | `Formenctype
    | `Method
    | `Formnovalidate
    | `Formtarget
    | `Name
    | `Text_Value
    | `Button_Type
  ]

(* NAME: select, KIND: star, TYPE: [= common |`Autofocus | `Multiple | `Name | `Size | `Form | `Disabled ], [ `Optgroup | `Option ],[=`Select], ARG: [ `Optgroup | `Option ], ATTRIB:  OUT: [=`Select] *)
type select = [ | `Select ]

type select_content = [ | `Optgroup | `Option ]

type select_content_fun = [ | `Optgroup | `Option ]

type select_attrib =
  [ | common | `Autofocus | `Multiple | `Name | `Size | `Form | `Disabled | `Required
  ]

(* NAME: datalist, KIND: nullary, TYPE: [= common ], [=`Datalist], ARG: notag, ATTRIB:  OUT: [=`Datalist] *)
type datalist = [ | `Datalist ]

type datalist_content = notag

type datalist_content_fun = notag

type datalist_attrib = [ | common ]

(* NAME: optgroup, KIND: star, TYPE: [= common | `Disabled | `Label ], [= `Option ], [=`Optgroup], ARG: [= `Option ], ATTRIB:  OUT: [=`Optgroup] *)
type optgroup = [ | `Optgroup ]

type optgroup_content = [ | `Option ]

type optgroup_content_fun = [ | `Option ]

type optgroup_attrib = [ | common | `Disabled | `Label ]

type option_attrib =
  [ | common | `Selected | `Text_Value | `Disabled | `Label | `Value ]

type selectoption = [ | `Option ]

type option_content_fun = [ | `PCDATA ]

type option_content = [ | `PCDATA ]

(* NAME: keygen, KIND: nullary, TYPE: [= common | `Autofcus | `Challenge | `Disabled | `Form | `Keytype | `Name ], [=`Keygen], ARG: notag, ATTRIB:  OUT: [=`Keygen] *)
type keygen = [ | `Keygen ]

type keygen_content = notag

type keygen_content_fun = notag

type keygen_attrib =
  [ | common | `Autofcus | `Challenge | `Disabled | `Form | `Keytype | `Name
  ]

(* NAME: progress, KIND: star, TYPE: [= common | `Float_Value |`Max| `Form ],[= phrasing_without_progress], [=`Progress], ARG: [= phrasing_without_progress], ATTRIB:  OUT: [=`Progress] *)
type progress = [ | `Progress ]

type progress_content = [ | phrasing_without_progress ]

type progress_content_fun = [ | phrasing_without_progress ]

type progress_attrib = [ | common | `Float_Value | `Max | `Form ]

(* NAME: meter, KIND: star, TYPE: [= common |`Float_Value |`Min |`Max |`Low |`High |`Optimum |`Form],[= phrasing_without_meter ],[=`Meter], ARG: [= phrasing_without_meter ], ATTRIB:  OUT: [=`Meter] *)
type meter = [ | `Meter ]

type meter_content = [ | phrasing_without_meter ]

type meter_content_fun = [ | phrasing_without_meter ]

type meter_attrib =
  [ | common | `Float_Value | `Min | `Max | `Low | `High | `Optimum | `Form
  ]

(* NAME: output_elt, KIND: star, TYPE: [= common |`Form |`Output_for |`Name],[= phrasing ],[=`Output], ARG: [= phrasing ], ATTRIB:  OUT: [=`Output] *)
type output_elt = [ | `Output ]

type output_elt_content = [ | phrasing ]

type output_elt_content_fun = [ | phrasing ]

type output_elt_attrib = [ | common | `Form | `Output_for | `Name ]

(* NAME: details, KIND: star, TYPE: [= common | `Open ], [= flow5] elt, [= `Details], ARG: [= flow5] elt, ATTRIB:  OUT: [= `Details] *)
type details = [ | `Details ]

type details_content = [ | flow5 ]

type details_content_fun = [ | flow5 ]

type details_attrib = [ | common | `Open ]

(* NAME: summary, KIND: star, TYPE: [= common ],[= phrasing ], [=`Summary], ARG: [= phrasing ], ATTRIB:  OUT: [=`Summary] *)
type summary = [ | `Summary ]

type summary_content = [ | phrasing ]

type summary_content_fun = [ | phrasing ]

type summary_attrib = [ | common ]

(* NAME: command, KIND: nullary, TYPE: [= common |`Icon |`Disabled |`Checked|`Radiogroup |`Command_Type], [=`Command], ARG: notag, ATTRIB:  OUT: [=`Command] *)
type command = [ | `Command ]

type command_content = notag

type command_content_fun = notag

type command_attrib =
  [ | common | `Icon | `Disabled | `Checked | `Radiogroup | `Command_Type
  ]

(* NAME: menu, KIND: nullary, TYPE: [= common |`Label |`Menu_Type ],[=`Menu], ARG: notag, ATTRIB:  OUT: [=`Menu] *)
type menu = [ | `Menu ]

type menu_content = notag

type menu_content_fun = notag

type menu_attrib = [ | common | `Label | `Menu_Type ]

(* NAME: noscript, KIND: plus, TYPE: [= common ], 'a, [=`Noscript of 'a], ARG: 'a, ATTRIB:  OUT: [=`Noscript of 'a] *)
type noscript = [ | `Noscript of flow5_without_noscript ]

type noscript_content = flow5_without_noscript

type noscript_content_fun = flow5_without_noscript

type noscript_attrib = [ | common ]

(* NAME: meta, KIND: nullary, TYPE: [= common | `Http_equiv | `Name | `Content | `Charset ], [=`Meta], ARG: notag, ATTRIB:  OUT: [=`Meta] *)
type meta = [ | `Meta ]

type meta_content = notag

type meta_content_fun = notag

type meta_attrib = [ | common | `Http_equiv | `Name | `Content | `Charset | `Property ]

(* NAME: style, KIND: star, TYPE: [= common | `Media | `Mime_type | `Scoped ], [= `PCDATA ], [=`Style], ARG: [= `PCDATA ], ATTRIB:  OUT: [=`Style] *)
type style = [ | `Style ]

type style_content = [ | `PCDATA ]

type style_content_fun = [ | `PCDATA ]

type style_attrib = [ | common | `Media | `Mime_type | `Scoped ]

type script = [ | `Script ]

type script_attrib =
  [ | common | subressource_integrity
    | `Async | `Charset | `Src | `Defer | `Mime_type
  ]

type script_content = [ | `PCDATA ]

type script_content_fun = [ | `PCDATA ]

(* NAME: template, KIND: star, TYPE: [= common ], [= flow5 ], [=`Template], ARG: [= flow5 ], ATTRIB:  OUT: [=`Template] *)
type template = [ | `Template ]

type template_attrib = [ | common ]

type template_content = [ | flow5 ]

type template_content_fun = [ | flow5 ]

(* NAME: link, KIND: nullary, TYPE: [= common | `Hreflang | `Media | `Rel | `Href | `Sizes | `Mime_type ], [=`Link], ARG: notag, ATTRIB:  OUT: [=`Link] *)
type link = [ | `Link ]

type link_content = notag

type link_content_fun = notag

type link_attrib =
  [ | common | subressource_integrity
    | `Hreflang | `Media | `Rel | `Href | `Sizes | `Mime_type
  ]

type referrerpolicy = [
  | `Empty
  | `No_referrer
  | `No_referrer_when_downgrade
  | `Origin
  | `Origin_when_cross_origin
  | `Same_origin
  | `Strict_origin
  | `Strict_origin_when_cross_origin
  | `Unsafe_url
]

type big_variant =
  [ `W3_org_1999_xhtml
  | `Default
  | `Preserve
  | `Selected
  | `Get
  | `Post
  | `Checked
  | `Disabled
  | `ReadOnly
  | `Async
  | `Autofocus
  | `Autoplay
  | `Muted
  | `Anonymous
  | `Use_credentials
  | `Controls
  | `Ltr
  | `Rtl
  | `Formnovalidate
  | `Hidden
  | `Ismap
  | `Loop
  | `Novalidate
  | `Open
  | `Audio
  | `Metadata
  | `None
  | `Pubdate
  | `Required
  | `Reversed
  | `Scoped
  | `Seamless
  | `Hard
  | `Soft
  | `Multiple
  | `Checkbox
  | `Command
  | `Radio
  | `Context
  | `Toolbar
  | `Char
  | `Justify
  | `Left
  | `Right
  | `Col
  | `Colgroup
  | `Row
  | `Rowgroup
  | `All
  | `Cols
  | `Groups
  | `None
  | `Rows
  | `Rect
  | `Circle
  | `Poly
  | `Default
  | `One
  | `Zero
  | `Auto
  | `No
  | `Yes
  | `Defer
  | `Verbatim
  | `Latin
  | `Latin_name
  | `Latin_prose
  | `Full_width_latin
  | `Kana
  | `Katakana
  | `Numeric
  | `Tel
  | `Email
  | `Url
  ]

type sandbox_token =
  [ `Allow_forms
  | `Allow_pointer_lock
  | `Allow_popups
  | `Allow_top_navigation
  | `Allow_same_origin
  | `Allow_script ]


type input_type =
  [ `Button
  | `Checkbox
  | `Color
  | `Date
  | `Datetime
  | `Datetime_local
  | `Email
  | `File
  | `Hidden
  | `Image
  | `Month
  | `Number
  | `Password
  | `Radio
  | `Range
  | `Reset
  | `Search
  | `Submit
  | `Tel
  | `Text
  | `Time
  | `Url
  | `Week ]
