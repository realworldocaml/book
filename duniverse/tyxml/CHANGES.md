# 4.4.0

* Add support for Reason's JSX syntax with a new `tyxml-jsx` package 
  (#254 by Joris Giovannangeli and Gabriel Radanne
   with help from Ulrik Strid and Louis Roché)
* Modernize the handling of toplevel printers for utop.
  (Gabriel Radanne)

## Elements and attributes

* Add `allowfullscreen`, `allowpaymentrequest`, `referrerpolicy` attributes 
  (#242 by Thibault Suzanne)
* Allow `crossorigin` attribute for script element
  (#243 by Thibault Suzanne)
* Greatly improved support of whitespaces in the PPX
  (#225 by Jules Aguillon)
* Add preliminary support for ARIA attributes
  (#253 by Stéphane Legrand and Gabriel Radanne)
* Add `template` element
  (#239 Stéphane Legrand)

* Several bug fixes for types and PPX

# 4.3.0

* Dunify
  This also removes all the deprecated libraries (`tyxml.syntax`, `tyxml.parser`)
  and removes the ocamlfind library `tyxml.ppx` in favor of `tyxml-ppx`.
  (#197 by Drup, Rudi Grinberg and Anton Bachin)
* Add simplistic indentation for the Format-based printer (#187 by Drup)
* Allow the ppx to be used for more exotic tyxml instances, such
  as reactive elements (#200 by Drup)
* Add `Html.of_seq` and `Svg.of_seq`, which allow to easily import
  HTML parsed with markup in TyXML (#221 by Drup)

## Elements and attributes
* Add Html.txt and Svg.txt as an alias for `pcdata` (#222 by Drup)
* Add noopener link types (#198 by Jérôme Vouillon)
* Slightly relax dt content type (#193 by Anton Bachin)
* Add touch events (#211 by Malthe Borch)
* Fix handling of figcaption in the PPX (#219 by Drup)

# 4.2.0

* Compatibility with OCaml 4.6.0.
* The ppx should now be compatible with driver-based workflows. In particular, jbuilder.
* Future breakage:
* The two camlp4-based packages (tyxml.syntax and tyxml.parser) are now deprecated and will be removed in the next major version.
* Introduction of the tyxml-ppx ocamlfind package. Usage of the tyxml.ppx package is discouraged, and it will be removed in the next major version.
* Various fixes in the Html_sigs.T module (contribution by Fabian Pijcke):
* Fixed the map element function signature.
* The elements functions now (almost) all make use of the types defined in Html_types, rather than redefining them.
* Html_sigs.T.fieldset now takes [< legend] elt wrap as optional argument rather than legend elt wrap.
* Add basic support for `aria-*` attributes (contribution by Armaël Guéneau)
(see https://www.w3.org/TR/wai-aria-1.1/#states_and_properties)
* Add support for the `role` attribute (contribution by Armaël Guéneau)
(see https://www.w3.org/TR/role-attribute/)
* Add support for the `minlength` form attribute (contribution by Armaël Guéneau)
(See https://www.w3.org/TR/html5/forms.html#attr-input-minlength)

# 4.1.0

* Uses uutf 1.0 (contribution by Daniel Bunzli)

# 4.0.1

* Fix handling of comments in the ppx.
* Fix printing of utf8 in attributes.
* Properly flush ppx errors. This bug was causing some blank error messages.
* Fix handling of whitespaces in `<select>` in the ppx.

# 4.0.0

## Features
* A new PPX has been added that allows to use tyxml with the HTML/SVG syntax, using the markup library (contribution by Anton 'aantron' Bachin).
* New Format-based printers are available, both as functors and has `pp` functions in the various implementations. Old printers are deprecated.
* Toplevel printers are now available for the `tyxml` library.
* The `str` library has been replaced by `re`.
* Various attributes arguments have been simplified. In particular:
* Constants arguments have been removed
* `` `On|`Off `` arguments are now replaced by booleans.
* Some arguments now use an option type.

* XML comments are now properly serialized (without escaping but with sanitization).

## Elements and attributes
* Add srcset and sizes attributes (contribution by Stéphane 'slegrand45' Legrand).
* The arguments of the `sandbox` attributes are now more consistent (contributino by Anton 'aantron' Bachin).
* Various SVG attributes and elements are now properly named (contributino by Anton 'aantron' Bachin).
* Add inputmode attribute.

## Documentation
* Both the API documentation and the manual have been completely rewritten! Do not hesitate to read them and provide feedback.
* Various examples have been added in the `example/` directory. (basic_website contributed by Edgar 'fxfactorial' Aroutiounian).

## Renaming and deprecations
* Files in the `tyxml` library are now packed in a `Tyxml` module.
If you were using one of `Html5`, `Svg` and `Xml` module, simply open `Tyxml`.
* All Html5 modules are now named Html
* Various attributes and elements have been renamed. The original versions have been kept and marked deprecated.
* Various elements that were both deprecated in the HTML specification and not usable due to typing constraints have been removed.


# 3.6.0

* Improves and simplify the wrapping interface. Breaking change.
* Add the possibility to specify converters, for constants functions.
See also eliom's shared react.
* Fix printing of floating numbers.
* Add the main element.
* Fix the accept attribute.

# 3.5.0

* Add Tyxml_name, which allows to derive tyxml identifiers from HTML
elements and attributes.
* Internally build the tool `autoname`, which applies the aftermentionned
transformation for the given elements/attributes.
* Fix typo in `datetime-local`.
* Add download attributes for area and tags.
* Add various svg `text` attributes.
* Fix namespaces issues related to svg elements inside html.

# 3.4.0

* Add `a_lang` for HTML. Deprecate `a_srclang` in favor of `a_xml_lang`.
* Fix a performance issue with `Xml_print.Utf8.{normalize, normalize_html}.
* Remove `Xml_print.Utf8.normalize_from`.
The function was not useful and not optimizable easily.
* Add missing parameters for the attributes xlink:actuate and xml:space.
* Svg elements use the xlink namespace (contribution by Florent Becker).
* Do not use the `url(...)` form when inappropriate (contribution by Florent Becker).
* Fix a typo in the `spellcheck` attribute (contribution by Kevin Brubeck Unhammer).
* Fix the `sizes` attributes and add missing attributes for the `sandbox` tag (contributions by Eyyüb Sari).
* Fix the `img` tag in the syntax extension.
* Fix compilation of the opam package under freeBSD.
* Fix typing for the various `font_` svg attributes.

# 3.3.0

* Add `Xml_print.Utf8` to encode html elements to utf8 properly.

# 3.2.1

* Add signature functors to ease export of module created with the functorial interface.
See the manual for more details.
* Fix variance for Svg.attrib.
* Fix export of Xml.list_wrap for Html5.M and Svg.M. Should fix syntax extension with those.

# 3.2.0

* Remove Xhtml.
* Remove plus elements.
For example, table doesn't enforce non-emptyness anymore.
* Add various types that were not exported (img, dl, figure, rp, rt and ruby types).
* Expose `string_of_number`, the better stringifier introduced in 3.1.0.

* Functorial interface breaking changes:
* Modify the functorized interface to export typed events.
You can now specify different handlers for keyboard and mouse events.
There are two new types and functions (keyboard and mous events) in the Xml signature.
* Add a wrapping type for lists of nodes. See reactiveData and new eliom wrapping.

# 3.1.0

* Replace fake booleans (`\`True | \`False`) by actual booleans. Breaking change.
* Camlp4 is now an optional dependency. The syntax extension is build only when the syntax flag is enabled (true by default).
* Use a better stringifier for float values (copied from js_of_ocaml).
* Add attributes `muted`, `crossorigin` and `mediagroup` for `<audio>` and `<video>`.
* Various misc fixes
* Fix in the svg syntax extension
* Typo "proress" -> "progress"

# 3.0.0

* In the functorial interface, allow to wrap xml nodes inside a monad by providing an additional wrapping module. Used by reactive nodes in eliom.
* Use oasis as build systems
* Various bug fixes, mostly related to the svg module.

# 2.3.0

* Adding module Unsafe for inserting missing nodes or attributes

# 2.2.0

* Adding tag <u>

# 2.1.0

* Rename all module names lower-case
* Explicit choice of implementation for syntax extension

# 2.0.2

* Add a simple printer: XML.print
* API change:
** Rename XML.event into XML.event_handler
** The functorized interface now export the concrete representation of XML.nodes
** Open types in SVG_sigs (closes #269).

# 2.0.1

* Allow compilation on win32/msvc
* Truly allow to abstract the XMl.uri representation
* Always print XHTML in a format that is "Html compatible":
** Add missing namespace in <html>.

# 2.0

* Allow the namespace attribute in HTML5 elements.
* Always print polyglot XML. ( document correct as HTML5 and as XML )
* Add new polymorphic types to HTML5 ( between_phrasing_and_phrasing_without_interactive, ... )

# 2.0-rc1

* Some fixes to match the latest HTML5 working draft (05/08/2011).
* Simplification of the functorial interface (use module substitution)
* Fix IFrame usage.

# 1.91

* First independent release (was released with ocsigen)
* Add a functorial interface for concrete XML representation
* Add a functorial interface for stream printer
* Rename XHTML5 into HTML5
* Change the default syntax to HTML5 instead of XHTML 1.1
