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

(** SVG types with variants, goes with {!Svg_sigs.T}. *)

(** This module defines basic data types for data, attributes
    and element occurring in SVG documents.
    It is based on the specification available at http://www.w3.org/TR/SVG/.

    This module is experimental, it may lack of some attributes,
    and the interface is very low level and do not take deeply into account
    the needs of SVG elements. *)

(* Some attribute and elements are tagged with XXX: they
   may be improved and do not match completely the SVG spec *)

(** {1 Categories of elements and attributes} *)

(** This part defines the categories of elements and attributes *)

(** {2 Elements} *)

type animation_element =
  [ | `AnimateColor | `AnimateMotion | `AnimateTransform | `Animate | `Set ]
type descriptive_element = [ | `Desc | `Metadata | `Title ]
type basic_shape_element =
  [ | `Circle | `Ellipse | `Line | `Polygon | `Polyline | `Rect ]

type container_element =
  [
    | `A
    | `Defs
    | `Glyph
    | `G
    | `Marker
    | `Mask
    | `Missing_glyph
    | `Pattern
    | `Svg
    | `Switch
    | `Symbol
  ]

type filter_primitive_element =
  [
    | `FeBlend
    | `FeColorMatrix
    | `FeComponentTransfer
    | `FeComposite
    | `FeConvolveMatrix
    | `FeDiffuseLighting
    | `FeDisplacementMap
    | `FeFlood
    | `FeGaussianBlur
    | `FeImage
    | `FeMerge
    | `FeMorphology
    | `FeOffset
    | `FeSpecularLighting
    | `FeTile
    | `FeTurbulence
  ]

type light_source_element =
  [
    | `FeDistantLight
    | `FePointLight
    | `FeSpotLight
  ]

type shape_element = [ | `Circle | `Ellipse | `Line | `Path | `Polyline | `Polygon | `Rect]

type structural_element = [ | `Defs | `G | `Svg | `Symbol | `Use ]

type text_content_element =
  [ | `AltGlyph | `TextPath | `Text | `Tref | `Tspan ]

type text_content_child_element =
  [ | `AltGlyph | `TextPath | `Tref | `Tspan ]

type gradient_element = [ | `Lineargradient | `Radialgradient ]

type graphics_element =
  [
    | `Circle
    | `Ellipse
    | `Image
    | `Line
    | `Path
    | `Polygon
    | `Polyline
    | `Rect
    | `Text
    | `Use
  ]

type graphics_ref_element = [ | `Image | `Use ]

(** {2 Attributes } *)

type conditional_processing_attr =
  [ | `RequiredExtensions | `RequiredFeatures | `SystemLanguage ]

type core_attr = [ | `Id | `Xml_base | `Xml_lang | `Xml_space | `User_data ]

type transfer_attr =
  [
    | `Type_transfert
    | `TableValues
    | `Slope
    | `Intercept
    | `Amplitude
    | `Exponent
    | `Offset_transfer
  ]

type document_event_attr =
  [ | `OnAbort | `OnError | `OnResize | `OnScroll | `OnUnload | `OnZoom ]

type filter_primitive_attr = [ | `Height | `Result | `Width | `X | `Y ]

type animation_event_attr = [ | `OnBegin | `OnEnd | `OnRepeat | `OnLoad ]

type animation_attr_target_attr = [ | `AttributeType | `AttributeName ]

type animation_timing_attr =
  [
    | `Begin
    | `Dur
    | `End
    | `Min
    | `Max
    | `Restart
    | `RepeatCount
    | `RepeatDur
    | `Fill_Animation
  ]

type animation_value_attr =
  [ | `CalcMode | `Valuesanim | `KeyTimes | `KeySplines | `From | `To | `By
  ]

type animation_addition_attr = [ | `Additive | `Accumulate ]

type presentation_attr =
  [
    | `Alignment_Baseline
    | `Baseline_Shift
    | `Clip
    | `Clip_Path
    | `Clip_Rule
    | `Color
    | `Color_Interpolation
    | `Color_interpolation_filters
    | `Color_profile
    | `Color_rendering
    | `Cursor
    | `Direction
    | `Display
    | `Dominant_Baseline
    | `Enable_background
    | `Fill
    | `Fill_opacity
    | `Fill_rule
    | `Filter
    | `Flood_Color
    | `Flood_Opacity
    | `Font_Family
    | `Font_Size
    | `Font_Size_Adjust
    | `Font_Stretch
    | `Font_Style
    | `Font_Variant
    | `Font_Weight
    | `Glyph_Orientation_Horizontal
    | `Glyph_Orientation_Vertical
    | `Image_Rendering
    | `Kerning
    | `Letter_Spacing
    | `Lighting_Color
    | `Marker_End
    | `Marker_Mid
    | `Marker_Start
    | `Mask
    | `Opacity
    | `Overflow
    | `Pointer_Events
    | `Shape_Rendering
    | `Stop_Color
    | `Stop_Opacity
    | `Stroke
    | `Stroke_Dasharray
    | `Stroke_Dashoffset
    | `Stroke_Linecap
    | `Stroke_Linejoin
    | `Stroke_Miterlimit
    | `Stroke_Opacity
    | `Stroke_Width
    | `Text_Anchor
    | `Text_Decoration
    | `Text_Rendering
    | `Unicode_Bidi
    | `Visibility
    | `Word_Spacing
    | `Writing_Mode
  ]

type graphical_event_attr =
  [
    | `OnActivate
    | `OnClick
    | `OnFocusIn
    | `OnFocusOut
    | `OnLoad
    | `OnMouseDown
    | `OnMouseMove
    | `OnMouseOut
    | `OnMouseOver
    | `OnMouseUp
  ]

type xlink_attr =
  [
    | `Xlink_href
    | `Xlink_type
    | `Xlink_role
    | `Xlink_arcrole
    | `Xlink_title
    | `Xlink_show
    | `Xlink_actuate
  ]

(** {2 Generic data types} *)

type iri = string
(** An IRI reference is an Internationalized Resource Identifier with
    an optional fragment identifier, as defined in Internationalized
    Resource Identifiers [RFC3987]. An IRI reference serves as a reference
    to a resource or (with a fragment identifier) to a secondary
    resource. See References and the ‘defs’ element.. *)

(** {2 Units} *)

(** SVG defines several units to measure time, length, angles. *)
module Unit : sig
  type 'a quantity = (float * 'a option)
  type angle = [ `Deg | `Grad | `Rad ] quantity
  type length = [ `Em | `Ex | `Px | `In | `Cm | `Mm | `Pt | `Pc | `Percent ] quantity
  type time = [ `S | `Ms ] quantity
  type frequency = [ `Hz | `KHz ] quantity
end

open Unit

type coord = length
type number = float
type number_optional_number = (number * (number option))
type percentage = float
type strings = string list

type color = string
type icccolor = string

type paint_whitout_icc =
  [ `None | `CurrentColor
  | `Color of (color * icccolor option)
  ]

type paint =
  [ paint_whitout_icc
  | `Icc of (iri * paint_whitout_icc option) ]

(* Transformation *)
type transform =
  [ `Matrix of (float * float * float * float * float * float)
  | `Translate of (float * (float option))
  | `Scale of (float * (float option))
  | `Rotate of (angle * ((float * float) option))
  | `SkewX of angle
  | `SkewY of angle ]

type spacestrings = string list
type commastrings = string list
type transforms = transform list
type fourfloats = (float * float * float * float)
type lengths = length list
type numbers = float list
type numbers_semicolon = float list
type coords = (float * float) list
type rotate = float list


type pcdata = [ `PCDATA ]
type txt = [ | `PCDATA ]

(** {1 Element} *)

(*-ELEMENTS-*)
type svg = [ | `Svg ]

(* star *)
type svg_content =
  [
    | animation_element
    | descriptive_element
    | shape_element
    | structural_element
    | gradient_element
    | `A
    | `AltGlyphDef
    | `ClipPath
    | `Color_Profile
    | `Cursor
    | `Filter
    | `Font
    | `Font_Face
    | `ForeignObject
    | `Image
    | `Marker
    | `Mask
    | `Pattern
    | `Script
    | `Style
    | `Switch
    | `Text
    | `View
  ]

type svg_attr =
  [
    | conditional_processing_attr
    | core_attr
    | document_event_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `X
    | `Y
    | `Width
    | `Height
    | `ViewBox
    | `PreserveAspectRatio
    | `ZoomAndPlan
    | `Version
    | `BaseProfile
    | `ContentScriptType
    | `ContentStyleType
    | `X
    | `Y
  ]

type g = [ | `G ]

(* star *)
type g_content =
  [
    | animation_element
    | descriptive_element
    | shape_element
    | structural_element
    | gradient_element
    | `A
    | `AltGlyphDef
    | `ClipPath
    | `Color_Profile
    | `Cursor
    | `Filter
    | `Font
    | `Font_Face
    | `ForeignObject
    | `Image
    | `Marker
    | `Mask
    | `Pattern
    | `Script
    | `Style
    | `Switch
    | `Text
    | `View
  ]

type g_attr =
  [
    | conditional_processing_attr
    | core_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Transform
  ]

type defs = [ | `Defs ]

(* star *)
type defs_content =
  [
    | animation_element
    | descriptive_element
    | shape_element
    | structural_element
    | gradient_element
    | `A
    | `AltGlyphDef
    | `ClipPath
    | `Color_Profile
    | `Cursor
    | `Filter
    | `Font
    | `Font_Face
    | `ForeignObject
    | `Image
    | `Marker
    | `Mask
    | `Pattern
    | `Script
    | `Style
    | `Switch
    | `Text
    | `View
  ]

type defs_attr =
  [
    | conditional_processing_attr
    | core_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Transform
  ]

type desc = [ | `Desc ]

(* unary *)
type desc_content = [ | `PCDATA ]

type desc_attr = [ | core_attr | `Class | `Style ]

type title = [ | `Title ]

(* unary *)
type title_content = [ | `PCDATA ]

type title_attr = desc_attr

type symbol = [ | `Symbol ]

(* star *)
type symbol_content =
  [
    | animation_element
    | descriptive_element
    | structural_element
    | gradient_element
    | `A
    | `AltGlyphDef
    | `ClipPath
    | `Color_Profile
    | `Cursor
    | `Filter
    | `Font
    | `Font_Face
    | `ForeignObject
    | `Image
    | `Marker
    | `Mask
    | `Pattern
    | `Script
    | `Style
    | `Switch
    | `Text
    | `View
  ]

type symbol_attr =
  [
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `PreserveAspectRatio
    | `ViewBox
  ]

type use = [ | `Use ]

(* star *)
type use_content = [ | animation_element | descriptive_element ]

type use_attr =
  [
    | core_attr
    | conditional_processing_attr
    | graphical_event_attr
    | presentation_attr
    | xlink_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Transform
    | `X
    | `Y
    | `Width
    | `Height
    | `Xlink_href
  ]

type image = [ | `Image ]

(* star *)
type image_content = [ | animation_element | descriptive_element ]

type image_attr =
  [
    | core_attr
    | conditional_processing_attr
    | graphical_event_attr
    | xlink_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `PreserveAspectRatio
    | `Transform
    | `X
    | `Y
    | `Width
    | `Height
    | `Xlink_href
  ]

type switch = [ | `Switch ]

(* star *)
type switch_content =
  [
    | animation_element
    | descriptive_element
    | shape_element
    | `A
    | `ForeignObject
    | `G
    | `Image
    | `Svg
    | `Switch
    | `Text
    | `Use
  ]

type switch_attr =
  [
    | conditional_processing_attr
    | core_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Transform
  ]

type style = [ | `Style ]

(* unary *)
type style_content = [ | `PCDATA ]

type style_attr = [ | core_attr | `Title | `Media | `Type ]

type path = [ | `Path ]

(* star *)
type path_content = [ | animation_element | descriptive_element ]

type path_attr =
  [
    | conditional_processing_attr
    | core_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Transform
    | `D
    | `PathLength
  ]

type rect = [ | `Rect ]

(* star *)
type rect_content = [ | animation_element | descriptive_element ]

type rect_attr =
  [
    | conditional_processing_attr
    | core_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Transform
    | `X
    | `Y
    | `Width
    | `Height
    | `Rx
    | `Ry
  ]

type circle = [ | `Circle ]

(* star *)
type circle_content = [ | animation_element | descriptive_element ]

type circle_attr =
  [
    | conditional_processing_attr
    | core_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Transform
    | `R
    | `Cx
    | `Cy
  ]

type ellipse = [ | `Ellipse ]

(* star *)
type ellipse_content = [ | animation_element | descriptive_element ]

type ellipse_attr =
  [
    | conditional_processing_attr
    | core_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Transform
    | `Rx
    | `Ry
    | `Cx
    | `Cy
  ]

type line = [ | `Line ]

(* star *)
type line_content = [ | animation_element | descriptive_element ]

type line_attr =
  [
    | conditional_processing_attr
    | core_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Transform
    | `X1
    | `Y1
    | `X2
    | `Y2
  ]

type polyline = [ | `Polyline ]

(* star *)
type polyline_content = [ | animation_element | descriptive_element ]

type polyline_attr =
  [
    | conditional_processing_attr
    | core_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Transform
    | `Points
  ]

type polygon = [ | `Polygon ]

(* star *)
type polygon_content = [ | animation_element | descriptive_element ]

type polygon_attr =
  [
    | conditional_processing_attr
    | core_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Transform
    | `Points
  ]

type text = [ | `Text ]

(* star *)
type text_content =
  [
    | animation_element
    | descriptive_element
    | text_content_child_element
    | `PCDATA
    | `A
  ]

type text_attr =
  [
    | conditional_processing_attr
    | core_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Transform
    | `LengthAdjust
    | `X_list
    | `Y_list
    | `Dx_list
    | `Dy_list
    | `Rotate
    | `TextLength
  ]

type tspan = [ | `Tspan ]

(* star *)
type tspan_content =
  [
    | descriptive_element
    | core_attr
    | `PCDATA
    | `A
    | `AltGlyph
    | `Animate
    | `AnimateColor
    | `Set
    | `Tref
    | `Tspan
  ]

type tspan_attr =
  [
    | core_attr
    | conditional_processing_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `X_list
    | `Y_list
    | `Dx_list
    | `Dy_list
    | `Rotate
    | `TextLength
    | `LengthAdjust
  ]

type tref = [ | `Tref ]

(* star *)
type tref_content =
  [ | descriptive_element | `Animate | `AnimateColor | `Set
  ]

type tref_attr =
  [
    | conditional_processing_attr
    | core_attr
    | graphical_event_attr
    | presentation_attr
    | xlink_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Xlink_href
  ]

type textpath = [ | `TextPath ]

(* star *)
type textpath_content =
  [
    | descriptive_element
    | `PCDATA
    | `A
    | `AltGlyph
    | `Animate
    | `AnimateColor
    | `Set
    | `Tref
    | `Tspan
  ]

type textpath_attr =
  [
    | conditional_processing_attr
    | core_attr
    | graphical_event_attr
    | presentation_attr
    | xlink_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Xlink_href
    | `StartOffset
    | `Method
    | `Spacing
  ]

type altglyph = [ | `AltGlyph ]

(* unary *)
type altglyph_content = [ | `PCDATA ]

type altglyph_attr =
  [
    | conditional_processing_attr
    | core_attr
    | graphical_event_attr
    | presentation_attr
    | xlink_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `X_list
    | `Y_list
    | `Dx_list
    | `Dy_list
    | `GlyphRef
    | `Format
    | `Rotate
    | `Xlink_href
  ]

type altglyphdef = [ | `AltGlyphDef ]

(* unary *)
type altglyphdef_attr = [ | core_attr ]

type altglyphitem = [ | `AltGlyphItem ]


type altglyphitem_content = [ | `glyphRef ]

type altglyphitem_attr = [ | core_attr ]

type glyphref = [ | `GlyphRef ]

(* nullary *)
type glyphref_attr =
  [
    | core_attr
    | presentation_attr
    | xlink_attr
    | `Class
    | `Style
    | `X
    | `Y
    | `Dx
    | `Dy
    | `GlyphRef
    | `Format
    | `Xlink_href
  ]

type marker = [ | `Marker ]

(* star *)
type marker_content =
  [
    | animation_element
    | descriptive_element
    | shape_element
    | structural_element
    | gradient_element
    | `A
    | `AltGlyphDef
    | `ClipPath
    | `Color_Profile
    | `Cursor
    | `Filter
    | `Font
    | `Font_Face
    | `ForeignObject
    | `Image
    | `Marker
    | `Mask
    | `Pattern
    | `Script
    | `Style
    | `Switch
    | `Text
    | `View
  ]

type marker_attr =
  [
    | core_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `ViewBox
    | `PreserveAspectRatio
    | `RefX
    | `RefY
    | `MarkerUnits
    | `MarkerWidth
    | `MarkerHeight
    | `Orient
  ]

type colorprofile = [ | `ColorProfile ]

(* star *)
type colorprofile_content = [ | descriptive_element ]

type colorprofile_attr =
  [
    | core_attr
    | xlink_attr
    | `Local
    | `Name
    | `Rendering_Intent
    | `Xlink_href
  ]

type lineargradient = [ | `Lineargradient ]

(* star *)
type lineargradient_content =
  [ | descriptive_element | `Animate | `AnimateTransform | `Set | `Stop
  ]

type lineargradient_attr =
  [
    | core_attr
    | presentation_attr
    | xlink_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `X1
    | `Y1
    | `X2
    | `Y2
    | `GradientUnits
    | `GradientTransform
    | `SpreadMethod
    | `Xlink_href
  ]

type radialgradient = [ | `Radialgradient ]

(* star *)
type radialgradient_content =
  [ | descriptive_element | `Animate | `AnimateTransform | `Set | `Stop
  ]

type radialgradient_attr =
  [
    | core_attr
    | presentation_attr
    | xlink_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Cx
    | `Cy
    | `R
    | `Fx
    | `Fy
    | `GradientUnits
    | `GradientTransform
    | `SpreadMethod
    | `Xlink_href
  ]

type stop = [ | `Stop ]

(* star *)
type stop_content = [ | `Animate | `Animate_Color | `Set ]

type stop_attr =
  [ | core_attr | presentation_attr | `Class | `Style | `Offset
  ]

type pattern = [ | `Pattern ]

(* star *)
type pattern_content =
  [
    | animation_element
    | descriptive_element
    | shape_element
    | structural_element
    | gradient_element
    | `A
    | `AltGlyphDef
    | `ClipPath
    | `Color_Profile
    | `Cursor
    | `Filter
    | `Font
    | `Font_Face
    | `ForeignObject
    | `Image
    | `Marker
    | `Mask
    | `Pattern
    | `Script
    | `Style
    | `Switch
    | `Text
    | `View
  ]

type pattern_attr =
  [
    | conditional_processing_attr
    | core_attr
    | presentation_attr
    | xlink_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `ViewBox
    | `PreserveAspectRatio
    | `X
    | `Y
    | `Width
    | `Height
    | `PatternUnits
    | `PatternContentUnits
    | `PatternTransform
    | `Xlink_href
  ]

type clippath = [ | `ClipPath ]

(* star *)
type clippath_attr =
  [
    | conditional_processing_attr
    | core_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Transform
    | `ClipPathUnits
  ]

type clippath_content =
  [ | descriptive_element | animation_element | shape_element | `Text | `Use
  ]

type mask = [ | `Mask ]

type mask_content =
  [
    | animation_element
    | descriptive_element
    | shape_element
    | structural_element
    | gradient_element
    | `A
    | `AltGlyphDef
    | `ClipPath
    | `Color_Profile
    | `Cursor
    | `Filter
    | `Font
    | `Font_Face
    | `ForeignObject
    | `Image
    | `Marker
    | `Mask
    | `Pattern
    | `Script
    | `Style
    | `Switch
    | `Text
    | `View
  ]

type mask_attr =
  [
    | conditional_processing_attr
    | core_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `X
    | `Y
    | `Width
    | `Height
    | `MaskUnits
    | `MaskContentUnits
  ]

type filter = [ | `Filter ]

(* star *)
type filter_content =
  [ | descriptive_element | filter_primitive_element | `Animate | `Set
  ]

type filter_attr =
  [
    | core_attr
    | presentation_attr
    | xlink_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `X
    | `Y
    | `Width
    | `Height
    | `FilterRes
    | `FilterUnits
    | `PrimitiveUnits
    | `Xlink_href
  ]

type fedistantlight = [ | `FeDistantLight ]

(* star *)
type fedistantlight_content = [ | `Animate | `Set ]

type fedistantlight_attr = [ | core_attr | `Azimuth | `Elevation ]

type fepointlight = [ | `FePointLight ]

(* star *)
type fepointlight_content = [ | `Animate | `Set ]

type fepointlight_attr = [ | core_attr | `X | `Y | `Z ]

type fespotlight = [ | `FeSpotLight ]

(* star *)
type fespotlight_content = [ | `Animate | `Set ]

type fespotlight_attr =
  [
    | core_attr
    | `X
    | `Y
    | `Z
    | `PointsAtX
    | `PointsAtY
    | `PointsAtZ
    | `SpecularExponent
    | `LimitingConeAngle
  ]

type feblend = [ | `FeBlend ]

(* star *)
type feblend_content = [ | `Animate | `Set ]

type feblend_attr =
  [
    | core_attr
    | presentation_attr
    | filter_primitive_attr
    | `Class
    | `Style
    | `In
    | `In2
    | `Mode
  ]

type fecolormatrix = [ | `FeColorMatrix ]

(* star *)
type fecolormatrix_content = [ | `Animate | `Set ]

type fecolormatrix_attr =
  [
    | core_attr
    | presentation_attr
    | filter_primitive_attr
    | `Class
    | `Style
    | `Typefecolor
    | `Values
    | `In
  ]

type fecomponenttransfer = [ | `FeComponentTransfer ]

(* star *)
type fecomponenttransfer_content =
  [ | `FeFuncA | `FeFuncB | `FeFuncG | `FeFuncR
  ]

type fecomponenttransfer_attr =
  [
    | core_attr
    | presentation_attr
    | filter_primitive_attr
    | `Class
    | `Style
    | `In
  ]

type fefunca = [ | `FeFuncA ]

(* star *)
type fefunca_content = [ | `Animate | `Set ]

type fefunca_attr = [ | core_attr | transfer_attr ]

type fefuncg = [ | `FeFuncA ]

(* star *)
type fefuncg_content = [ | `Animate | `Set ]

type fefuncg_attr = [ | core_attr | transfer_attr ]

type fefuncb = [ | `FeFuncA ]

(* star *)
type fefuncb_content = [ | `Animate | `Set ]

type fefuncb_attr = [ | core_attr | transfer_attr ]

type fefuncr = [ | `FeFuncA ]

(* star *)
type fefuncr_content = [ | `Animate | `Set ]

type fefuncr_attr = [ | core_attr | transfer_attr ]

type fecomposite = [ | `FeComposite ]

(* star *)
type fecomposite_content = [ | `Animate | `Set ]

type fecomposite_attr =
  [
    | core_attr
    | presentation_attr
    | filter_primitive_attr
    | `Class
    | `Style
    | `In
    | `In2
    | `OperatorComposite
    | `K1
    | `K2
    | `K3
    | `K4
  ]

type feconvolvematrix = [ | `FeConvolveMatrix ]

(* star *)
type feconvolvematrix_content = [ | `Animate | `Set ]

type feconvolvematrix_attr =
  [
    | core_attr
    | presentation_attr
    | filter_primitive_attr
    | `Class
    | `Style
    | `In
    | `Order
    | `KernelMatrix
    | `Divisor
    | `Bias
    | `TargetX
    | `TargetY
    | `EdgeMode
    | `KernelUnitLength
    | `PreserveAlpha
  ]

type fediffuselighting = [ | `FeDiffuseLighting ]

(* star *)
type fediffuselighting_content =
  [ | descriptive_element | light_source_element
  ]

(* XXX *)
type fediffuselighting_attr =
  [
    | core_attr
    | filter_primitive_attr
    | presentation_attr
    | `Class
    | `Style
    | `In
    | `SurfaceScale
    | `DiffuseConstant
    | `KernelUnitLength
  ]

type fedisplacementmap = [ | `FeDisplacementMap ]

(* star *)
type fedisplacementmap_content = [ | `Animate | `Set ]

type fedisplacementmap_attr =
  [
    | core_attr
    | filter_primitive_attr
    | presentation_attr
    | `Class
    | `Style
    | `In
    | `In2
    | `Scale
    | `XChannelSelector
    | `YChannelSelector
  ]

type feflood = [ | `FeFlood ]

(* star *)
type feflood_content = [ | `Animate | `AnimateColor | `Set ]

type feflood_attr =
  [ | core_attr | presentation_attr | filter_primitive_attr | `Class | `Style
  ]

type fegaussianblur = [ | `FeGaussianBlur ]

(* star *)
type fegaussianblur_content = [ | `Animate | `AnimateColor | `Set ]

type fegaussianblur_attr =
  [
    | core_attr
    | presentation_attr
    | filter_primitive_attr
    | `Class
    | `Style
    | `In
    | `StdDeviation
  ]

type feimage = [ | `FeImage ]

(* star *)
type feimage_content = [ | `Animate | `AnimateColor | `Set ]

type feimage_attr =
  [
    | core_attr
    | presentation_attr
    | filter_primitive_attr
    | xlink_attr
    | `Xlink_href
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `PreserveAspectRadio
  ]

type femerge = [ | `FeMerge ]

(* star *)
type femerge_content = [ | `FeMergeNode ]

type femerge_attr =
  [ | core_attr | presentation_attr | filter_primitive_attr | `Class | `Style
  ]

type femorphology = [ | `FeMorphology ]

(* star *)
type femorphology_content = [ | `Animate | `Set ]

type femorphology_attr =
  [
    | core_attr
    | presentation_attr
    | filter_primitive_attr
    | `OperatorMorphology
    | `Class
    | `Style
    | `In
    | `Radius
  ]

type feoffset = [ | `FeOffset ]

(* star *)
type feoffset_content = [ | `Animate | `Set ]

type feoffset_attr =
  [
    | core_attr
    | presentation_attr
    | filter_primitive_attr
    | `Class
    | `Style
    | `Dx
    | `Dy
    | `In
  ]

type fespecularlighting = [ | `FeSpecularLighting ]

(* star *)
type fespecularlighting_content =
  [ | descriptive_element | light_source_element
  ]

(* XXX *)
type fespecularlighting_attr =
  [
    | core_attr
    | presentation_attr
    | filter_primitive_attr
    | `Class
    | `Style
    | `In
    | `SurfaceScale
    | `SpecularConstant
    | `SpecularExponent
    | `KernelUnitLength
  ]

type fetile = [ | `FeTile ]

(* star *)
type fetile_content = [ | `Animate | `Set ]

type fetile_attr =
  [
    | core_attr
    | presentation_attr
    | filter_primitive_attr
    | `Class
    | `Style
    | `In
  ]

type feturbulence = [ | `FeTurbulence ]

(* star *)
type feturbulence_content = [ | `Animate | `Set ]

type feturbulence_attr =
  [
    | core_attr
    | presentation_attr
    | filter_primitive_attr
    | `Class
    | `Style
    | `BaseFrequency
    | `NumOctaves
    | `Seed
    | `StitchTiles
    | `TypeStitch
  ]

type cursor = [ | `Cursor ]

(* star *)
type cursor_content = descriptive_element

type cursor_attr =
  [
    | core_attr
    | conditional_processing_attr
    | xlink_attr
    | `X
    | `Y
    | `ExternalResourcesRequired
    | `Xlink_href
  ]

type a = [ | `A ]

(* star *)
type a_content =
  [
    | animation_element
    | descriptive_element
    | shape_element
    | structural_element
    | gradient_element
    | `A
    | `AltGlyphDef
    | `ClipPath
    | `Color_Profile
    | `Cursor
    | `Filter
    | `Font
    | `Font_Face
    | `ForeignObject
    | `Image
    | `Marker
    | `Mask
    | `Pattern
    | `Script
    | `Style
    | `Switch
    | `Text
    | `View
  ]

type a_attr =
  [
    | core_attr
    | conditional_processing_attr
    | xlink_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Transform
    | `Xlink_href
    | `Xlink_show
    | `Xlink_actuate
    | `Target
  ]

type view = [ | `View ]

(* star *)
type view_content = descriptive_element

type view_attr =
  [
    | core_attr
    | `ExternalResourcesRequired
    | `ViewBox
    | `PreserveAspectRatio
    | `ZoomAndPan
    | `ViewTarget
  ]

type script = [ | `Script ]

(* unary *)
type script_content = [ | `PCDATA ]

type script_attr =
  [
    | core_attr
    | xlink_attr
    | `ExternalResourcesRequired
    | `Type
    | `Xlink_href
  ]

type animation = [ | `Animation ]

(* star *)
type animation_content = descriptive_element

type animation_attr =
  [
    | conditional_processing_attr
    | core_attr
    | animation_event_attr
    | xlink_attr
    | animation_attr_target_attr
    | animation_timing_attr
    | animation_value_attr
    | animation_addition_attr
    | `ExternalResourcesRequired
  ]

type set = [ | `Set ]

(* star *)
type set_content = descriptive_element

type set_attr =
  [
    | core_attr
    | conditional_processing_attr
    | xlink_attr
    | animation_event_attr
    | animation_attr_target_attr
    | animation_timing_attr
    | `To
    | `ExternalResourcesRequired
  ]

type animatemotion = [ | `AnimateMotion ]

(* star *)
type animatemotion_content = [ | descriptive_element | `Mpath ]

(* XXX *)
type animatemotion_attr =
  [
    | conditional_processing_attr
    | core_attr
    | animation_event_attr
    | xlink_attr
    | animation_timing_attr
    | animation_value_attr
    | animation_addition_attr
    | `ExternalResourcesRequired
    | `Path
    | `KeyPoints
    | `Rotate
    | `Origin
  ]

(* XXX: rotate *)
type mpath = [ | `Mpath ]

(* star *)
type mpath_content = descriptive_element

type mpath_attr =
  [ | core_attr | xlink_attr | `ExternalResourcesRequired | `Xlink_href
  ]

type animatecolor = [ | `AnimateColor ]

(* star *)
type animatecolor_content = descriptive_element

type animatecolor_attr =
  [
    | conditional_processing_attr
    | core_attr
    | animation_event_attr
    | xlink_attr
    | animation_attr_target_attr
    | animation_timing_attr
    | animation_value_attr
    | animation_addition_attr
    | `ExternalResourcesRequired
  ]

type animatetransform = [ | `AnimateTransform ]

(* star *)
type animatetransform_content = descriptive_element

type animatetransform_attr =
  [
    | conditional_processing_attr
    | core_attr
    | animation_event_attr
    | xlink_attr
    | animation_attr_target_attr
    | animation_timing_attr
    | animation_value_attr
    | animation_addition_attr
    | `ExternalResourcesRequired
    | `Typeanimatetransform
  ]

type font = [ | `Font ]

(* star *)
type font_attr =
  [
    | core_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `HorizOriginX
    | `HorizOriginY
    | `HorizAdvX
    | `VertOriginX
    | `VertOriginY
    | `VertAdvY
  ]

type font_content =
  [
    | descriptive_element
    | `Font_Face
    | `Glyph
    | `Hkern
    | `MissingGlyph
    | `Vkern
  ]

type glyph = [ | `Glyph ]

(* star *)
type glyph_content =
  [
    | animation_element
    | descriptive_element
    | shape_element
    | structural_element
    | gradient_element
    | `A
    | `AltGlyphDef
    | `ClipPath
    | `Color_Profile
    | `Cursor
    | `Filter
    | `Font
    | `Font_Face
    | `ForeignObject
    | `Image
    | `Marker
    | `Mask
    | `Pattern
    | `Script
    | `Style
    | `Switch
    | `Text
    | `View
  ]

type glyph_attr =
  [
    | core_attr
    | presentation_attr
    | `Class
    | `Style
    | `D
    | `HorizAdvX
    | `VertOriginX
    | `VertOriginY
    | `VertAdvY
    | `Unicode
    | `GlyphName
    | `Orientation
    | `ArabicForm
    | `Lang
  ]

type missingglyph = [ | `MissingGlyph ]

(* star *)
type missingglyph_content =
  [
    | animation_element
    | descriptive_element
    | shape_element
    | structural_element
    | gradient_element
    | `A
    | `AltGlyphDef
    | `ClipPath
    | `Color_Profile
    | `Cursor
    | `Filter
    | `Font
    | `Font_Face
    | `ForeignObject
    | `Image
    | `Marker
    | `Mask
    | `Pattern
    | `Script
    | `Style
    | `Switch
    | `Text
    | `View
  ]

type missingglyph_attr =
  [
    | core_attr
    | presentation_attr
    | `Class
    | `Style
    | `D
    | `HorizAdvX
    | `VertOriginX
    | `VertOriginY
    | `VertAdvY
  ]

type hkern = [ | `Hkern ]

(* nullary *)
type hkern_attr = [ | core_attr | `U1 | `G1 | `U2 | `G2 | `K ]

type vkern = [ | `Vkern ]

(* nullary *)
type vkern_attr = [ | core_attr | `U1 | `G1 | `U2 | `G2 | `K ]

type font_face = [ | `Font_Face ]

(* nullary *)
type font_face_content = [ | descriptive_element | `Font_Face_Src ]

type font_face_attr =
  [
    | core_attr
    | `Font_Family
    | `Font_Style
    | `Font_Variant
    | `Font_Weight
    | `Font_Stretch
    | `Font_Size
    | `UnicodeRange
    | `UnitsPerEm
    | `Panose1
    | `Stemv
    | `Stemh
    | `Slope
    | `CapHeight
    | `XHeight
    | `AccentHeight
    | `Ascent
    | `Descent
    | `Widths
    | `Bbox
    | `Ideographic
    | `Alphabetic
    | `Mathematical
    | `Hanging
    | `VIdeographic
    | `VAlphabetic
    | `VMathematical
    | `VHanging
    | `UnderlinePosition
    | `UnderlineThickness
    | `StrikethroughPosition
    | `StrikethroughThickness
    | `OverlinePosition
    | `OverlineThickness
  ]

(* star *)
type font_face_src = [ | `Font_Face_Src ]

type font_face_src_content = [ | `Font_Face_Name | `Font_Face_Uri ]

type font_face_src_attr = core_attr

(* star *)
type font_face_uri = [ | `Font_Face_Uri ]

type font_face_uri_content = [ | `Font_Face_Format ]

type font_face_uri_attr = [ | core_attr | xlink_attr | `Xlink_href ]

(* nullary *)
type font_face_format = [ | `Font_Face_Format ]

type font_face_format_attr = [ | core_attr | `String ]

(* nullary *)
type font_face_name = [ | `Font_Face_Name ]

type font_face_name_attr = [ | core_attr | `Name ]

type metadata = [ | `Metadata ]

type metadata_attr = [ | core_attr ]

type foreignobject = [ | `ForeignObject ]

type foreignobject_attr =
  [
    | core_attr
    | conditional_processing_attr
    | graphical_event_attr
    | presentation_attr
    | `Class
    | `Style
    | `ExternalResourcesRequired
    | `Transform
    | `X
    | `Y
    | `Width
    | `Height
  ]

type alignment_baseline =
  [ `After_edge
  | `Alphabetic
  | `Auto
  | `Baseline
  | `Before_edge
  | `Central
  | `Hanging
  | `Ideographic
  | `Inherit
  | `Mathematical
  | `Middle
  | `Text_after_edge
  | `Text_before_edge ]

type dominant_baseline =
  [ `Auto
  | `Use_script
  | `No_change
  | `Reset_size
  | `Ideographic
  | `Alphabetic
  | `Hanging
  | `Mathematical
  | `Central
  | `Middle
  | `Text_after_edge
  | `Text_before_edge
  | `Inherit
  ]

type in_value =
  [ `SourceGraphic
  | `SourceAlpha
  | `BackgroundImage
  | `BackgroundAlpha
  | `FillPaint
  | `StrokePaint
  | `Ref of string ] [@@reflect.total_variant]

type offset =
  [ `Number of number
  | `Percentage of percentage ]

type big_variant =
  [ `A
  | `Absolute_colorimetric
  | `Align
  | `Always
  | `Atop
  | `Arithmetic
  | `Auto
  | `B
  | `Bever
  | `Blink
  | `Butt
  | `CSS
  | `Darken
  | `Default
  | `Dilate
  | `Disable
  | `Discrete
  | `Duplicate
  | `End
  | `Erode
  | `Exact
  | `FractalNoise
  | `Freeze
  | `HueRotate
  | `G
  | `Gamma
  | `GeometricPrecision
  | `H
  | `Identity
  | `In
  | `Inherit
  | `Initial
  | `Isolated
  | `Lighten
  | `Line_through
  | `Linear
  | `LuminanceToAlpha
  | `Magnify
  | `Matrix
  | `Medial
  | `Middle
  | `Miter
  | `Multiply
  | `Never
  | `New
  | `None
  | `Normal
  | `NoStitch
  | `ObjectBoundingBox
  | `OnLoad
  | `OnRequest
  | `OptimizeLegibility
  | `OptimizeSpeed
  | `Other
  | `Out
  | `Over
  | `Overline
  | `Paced
  | `Pad
  | `Perceptual
  | `Preserve
  | `R
  | `Reflect
  | `Remove
  | `Repeat
  | `Replace
  | `Relative_colorimetric
  | `Rotate
  | `Round
  | `Saturate
  | `Saturation
  | `Scale
  | `Screen
  | `SkewX
  | `SkewY
  | `Spacing
  | `SpacingAndGlyphs
  | `Spline
  | `Square
  | `Start
  | `Stitch
  | `Stretch
  | `StrokeWidth
  | `Sum
  | `Table
  | `Terminal
  | `Translate
  | `Turbulence
  | `Underline
  | `UserSpaceOnUse
  | `V
  | `WhenNotActive
  | `Wrap
  | `XML
  | `Xor
  ]
