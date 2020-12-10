# How to contribute changes

Thanks for contributing!

Here is a set of guidelines for contributing new elements and attributes to TyXML.

## How to add new elements and attributes

There are several steps to add new elements and attributes.
First off, you need to equip yourself with both the specification ([HTML][]/[SVG][]) and Mozilla's developer manual ([HTML][MSDNHTML]/[SVG][MSDNSVG]).

1. Figure out the right name

    A small tool is available in TyXML sources for this purpose.
    For example for the `foo-bar` attribute:
    ```sh
    % ./autoname.byte -a "foo-bar"
    Original name: 	foo-bar
    OCaml name: 	a_foo_bar
    Poly variant: 	`Foo_bar
    ```

2. Figure out the right types

   Types go in
   [`Html_types`](lib/html_types.mli)/[`Svg_types`](lib/svg_types.mli).

   For elements, this can be delicate, and there is no universal recipe, except reading the specification carefully.
    For attributes, this is usually quite easy:
    - If the attribute is for the element `myelem`, you need to add the attribute's polymorphic variants to `Html_types.myelem_attrib`.
    - If the attributes is for every element, add it to `Html_types.common`.

3. Add the element/attribute

    The implementation goes in
    [`Html_f`](lib/html_f.ml)/[`Svg_f`](lib/svg_f.ml)
    and the signature in
    [`Html_sigs`](lib/html_sigs.mli)/[`Svg_sigs`](lib/svg_sigs.mli).

4. (Optional) If the attribute value uses a new data type, you might need to introduce a new function in [`Html_f.Wrapped_functions`](lib/html_f.ml) and a new value parser for the PPX in [`Ppx_attribute_value`](ppx/ppx_attribute_value.mli).

5. Document your change
   - Link to the paragraph in the specification ([HTML][]/[SVG][]) in your commit message.
   - Link to Mozilla's reference ([HTML][MSDNHTML]/[SVG][MSDNSVG]) in the `.mli`.
   - Add your change to the [change log](CHANGES).

6. Test your change
   - Regular tests go in [`test/test_html.ml`](test/test_html.ml).
   - Ppx tests go in [`test/test_ppx.ml`](test/test_html.ml).

[HTML]: https://www.w3.org/TR/html5/
[SVG]: https://www.w3.org/TR/SVG11/
[MSDNHTML]: https://developer.mozilla.org/en-US/docs/Web/HTML/Reference
[MSDNSVG]: https://developer.mozilla.org/en-US/docs/Web/SVG
