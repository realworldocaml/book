(** Ppx Tests

    This file is here to torture the ppx. Tests that are directly related to
    html or svg should go to the other files.
*)
open Tyxml_test

let basics = "ppx basics", HtmlTests.make Html.[

  "elems",
  [[%html "<p></p>"]],
  [p []] ;

  "child",
  [[%html "<p><span>foo</span></p>"]],
  [p [span [txt "foo"]]] ;

  "list",
  [%html "<p></p><span>foo</span>"],
  [p [] ; span [txt "foo"]] ;

  "attrib",
  [[%html "<p id=foo></p>"]],
  [p ~a:[a_id "foo"] []] ;

  "attribs",
  [[%html "<p id=foo class=bar></p>"]],
  [p ~a:[a_id "foo"; a_class ["bar"] ] []] ;

  "comment",
  [[%html "<!--foo-->"]],
  [tot @@ Xml.comment "foo"] ;

  "txt",
  [[%html "foo"]],
  [txt "foo"] ;

  "document",
  [[%html "<html><head><title>foo</title></head></html>"]],
  [html (head (title (txt "foo")) []) (body [])] ;

  "let",
  [let%html x = "<p></p>" in x],
  [p []] ;

  "nested let",
  [let%html _ = "<p></p>" in
   let%html y = "<p></p>" in
   y],
  [p []] ;

  "let and",
  (let%html x = "<p></p>" and y = "<a></a>" in [x;y]),
  [p []; a []] ;

  "let fun",
  [let%html f x = "<p>"x"</p>" in f [a []]],
  [p [a []]] ;

  "whitespace in html element",
  [[%html "<html><head><title>foo</title></head> </html>"]],
  [html (head (title (txt "foo")) []) (body [])] ;

  (* "whitespace around html element",
   * [[%html "  <html><head><title>foo</title></head></html>  "]],
   * [html (head (title (txt "foo")) []) (body [])] ; *)

  "whitespace around element",
  [[%html "   <p></p>   "]],
  [p []] ;

  "whitespace in element",
  [[%html "   <p>  </p>   "]],
  [p [txt "  "]] ;

  "whitespace around lists",
  [%html "   <p></p><span></span>   "],
  [p [] ; span []] ;

  "whitespace around txt",
  [%html "   bar<p></p>foo   "],
  [txt "   bar" ; p [] ; txt "foo   " ] ;

  "whitespace in ul",
  [[%html "<ul>   <li>foo</li>  <li>bar</li>   </ul>"]],
  [ul [li [txt "foo"] ; li [txt "bar"]]] ;

  "whitespace in ol",
  [[%html "<ol>   <li>foo</li>  <li>bar</li>   </ol>"]],
  [ol [li [txt "foo"] ; li [txt "bar"]]] ;

  "whitespace in tr",
  [[%html "<tr>   <td>foo</td>    <td>bar</td>   </tr>"]],
  [tr [td [txt "foo"] ; td [txt "bar"]]] ;

  "whitespace in table",
  [[%html "<table>    <tr><td>foo</td></tr>   <tr><td>bar</td></tr>   </table>"]],
  [tablex [tbody [tr [td [txt "foo"]] ; tr [td [txt "bar"]]]]] ;

  "whitespace in table, full example",
  [[%html "<table>
  <caption>txt</caption>
  <colgroup>
    <col span=\"2\">
  </colgroup>
  <thead>
    <tr>
      <th>h1</th>
      <th>h2</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>b1</td>
      <td>b2</td>
    </tr>
  </tbody>
  <tfoot>
    <tr>
      <td>f1</td>
      <td>f2</td>
    </tr>
  </tfoot>
</table>"]],
  [tablex ~caption:(caption [txt "txt"])
    ~columns:[colgroup [col ~a:[a_span 2] ()]]
    ~thead:(thead [tr [th [txt "h1"] ; th [txt "h2"]]])
    ~tfoot:(tfoot [tr [td [txt "f1"] ; td [txt "f2"]]])
    [tbody [tr [td [txt "b1"] ; td [txt "b2"]]]]] ;

  "whitespace in select",
  [[%html {|<select>  <option value="bar">bar</option>  </select>|}]],
  [select [option ~a:[a_value "bar"] @@ txt "bar"]] ;

  "datalist",
  [[%html {|<datalist>
	<option value="foo">foo</option>
</datalist>|}]],
  [datalist ~children:(`Options [option ~a:[a_value "foo"] (txt "foo")]) ()];
  
  "comments",
  [[%html {|<div><p>a</p><!-- b --><hr/></div>|}]],
  [div [p [txt "a"]; tot (Xml.comment " b "); hr ()]] ;

  "figcaption first",
  [[%html {|<figure> <figcaption> hello </figcaption> <img src="foo.jpg" alt="a" /> </figure>|}]],
  [figure ~figcaption:(`Top (figcaption [txt " hello "]))
     [txt " "; img ~src:"foo.jpg" ~alt:"a" () ; txt " " ]];

  "figcaption last",
  [[%html {|<figure> <img src="foo.jpg" alt="a" /> <figcaption> hello </figcaption> </figure>|}]],
  [figure ~figcaption:(`Bottom (figcaption [txt " hello "]))
     [txt " "; img ~src:"foo.jpg" ~alt:"a" () ; txt " " ]];

  "type in source",
  [[%html {|<source src="foo.mp3" type="audio/mpeg"/>|}]],
  [source ~a:[a_src "foo.mp3"; a_mime_type "audio/mpeg"] ()];
    
  "table with body",
  [[%html "<table><tbody></tbody></table>"]],
  [tablex [tbody []]];

  "table with antiquot body",
  [[%html "<table>"[tbody []]"</table>"]],
  [tablex [tbody []]];

  "table with tr",
  [[%html "<table><tr></tr></table>"]],
  [tablex [tbody [tr[]]]];

  "table with antiquot tr",
  [[%html "<table><tbody>"[tr []]"</tbody></table>"]],
  [tablex [tbody [tr[]]]];

]

let attribs = "ppx attribs", HtmlTests.make Html.[

  "unit absent",
  [[%html "<div hidden></div>"]],
  [div ~a:[a_hidden ()] []] ;

  "unit present",
  [[%html "<div hidden=hidden></div>"]],
  [div ~a:[a_hidden ()] []] ;

  "bool default",
  [[%html "<div draggable></div>"]],
  [div ~a:[a_draggable true] []] ;

  "bool true",
  [[%html "<div draggable=true></div>"]],
  [div ~a:[a_draggable true] []] ;

  "bool false",
  [[%html "<div draggable=false></div>"]],
  [div ~a:[a_draggable false] []] ;

  "onoff default",
  [[%html "<form autocomplete></form>"]],
  [form ~a:[a_autocomplete true] []] ;

  "bool true",
  [[%html "<form autocomplete=on></form>"]],
  [form ~a:[a_autocomplete true] []] ;

  "bool false",
  [[%html "<form autocomplete=off></form>"]],
  [form ~a:[a_autocomplete false] []] ;

  "link rel=canonical",
  [[%html "<link rel=canonical href='/'>"]],
  [link ~rel:[`Canonical] ~href:"/" ()] ;

  "embed type",
  [[%html "<embed type='text/plain'>"]],
  [embed ~a:[a_mime_type "text/plain"] ()] ;

  "output for",
  [[%html "<output for=foo></output>"]],
  [output_elt ~a:[a_output_for ["foo"]] []] ;

  "input min time",
  [[%html "<input min='2002-10-02T15:00:00Z'>"]],
  [input ~a:[a_input_min (`Datetime "2002-10-02T15:00:00Z")] ()] ;

  "aria attributes",
  [[%html "<div aria-hidden=true></div>"]],
  [div ~a:[a_aria "hidden" ["true"]] []] ;

  "touch events",
  [[%html "<div ontouchstart='alert()'></div>"]],
  [div ~a:[a_ontouchstart "alert()"] []] ;

  "empty string as referrer policy",
  [[%html "<iframe referrerpolicy=''></iframe>"]],
  [iframe ~a:[a_referrerpolicy `Empty] []];

  "dashes in referrer policy",
  [[%html "<iframe referrerpolicy='no-referrer-when-downgrade'></iframe>"]],
  [iframe ~a:[a_referrerpolicy `No_referrer_when_downgrade] []];

  "html data-*",
  [[%html "<div data-foo='valfoo'></div>"]],
  [div
    ~a:[a_user_data "foo" "valfoo"] []] ;
]

let ns_nesting = "namespace nesting" , HtmlTests.make Html.[

  "html/svg",
  [[%html "<svg><g></g></svg>"]],
  [svg [Svg.g []]] ;

  "nested svg",
  [[%html "<div><svg><g></g></svg></div>"]],
  [div [svg [Svg.g []]]] ;

  "with_neighbour",
  [[%html "<div><span></span><svg><g></g></svg>foo</div>"]],
  [div [span [] ; svg [Svg.g []] ; txt "foo" ]] ;

  "ambiguous tag",
  [[%html "<svg><a></a></svg>"]],
  [svg [Svg.a []]] ;

]

let svg = "svg", SvgTests.make Svg.[

  "basic",
  [[%svg "<svg/>"]],
  [svg []] ;

  "transform",
  [[%svg "<line transform='translate(1) translate(2)'/>"]],
  [line ~a:[a_transform [`Translate (1., None); `Translate (2., None)]] []] ;

  "offset percentage",
  [[%svg "<stop offset='50.1%'/>"]],
  [stop ~a:[a_offset (`Percentage 50.1)] []] ;

  "text x, y",
  [[%svg "<text x='1 2' y='3 4'/>"]],
  [text ~a:[a_x_list [1., None; 2., None]; a_y_list [3., None; 4., None]] []] ;

  "text dx, dy",
  [[%svg "<text dx='1 2' dy='3 4'/>"]],
  [text
    ~a:[a_dx_list [1., None; 2., None]; a_dy_list [3., None; 4., None]] []] ;

  "svg data-*",
  [[%svg "<text data-foo='valfoo' />"]],
  [text
    ~a:[a_user_data "foo" "valfoo"] []] ;

  "feColorMatrix type",
  [[%svg "<feColorMatrix type='matrix'/>"]],
  [feColorMatrix ~a:[a_feColorMatrix_type `Matrix] []] ;

  "feTurbulence type",
  [[%svg "<feTurbulence type='fractalNoise'/>"]],
  [feTurbulence ~a:[a_feTurbulence_type `FractalNoise] []] ;

  "animateTransform type",
  [[%svg "<animateTransform type='translate'/>"]],
  [animateTransform ~a:[a_animateTransform_type `Translate] []] ;

  "feFuncR type, offset",
  [[%svg "<feFuncR type='identity' offset='0'/>"]],
  [feFuncR ~a:[a_transfer_type `Identity; a_transfer_offset 0.] []] ;

  "feComposite operator",
  [[%svg "<feComposite operator='xor'/>"]],
  [feComposite ~a:[a_feComposite_operator `Xor] []] ;

  "feMorphology operator",
  [[%svg "<feMorphology operator='erode'/>"]],
  [feMorphology ~a:[a_feMorphology_operator `Erode] []] ;

  "animation fill, values",
  [[%svg "<animation fill='freeze' values='1 2'/>"]],
  [animation ~a:[a_animation_fill `Freeze; a_animation_values ["1"; "2"]] []] ;

]

let svg_element_names = "svg element names", SvgTests.make Svg.[

  "textPath", [[%svg "<textPath/>"]], [textPath []] ;
  "linearGradient", [[%svg "<linearGradient/>"]], [linearGradient []] ;
  "radialGradient", [[%svg "<radialGradient/>"]], [radialGradient []] ;
  "clipPath", [[%svg "<clipPath/>"]], [clipPath []] ;
  "feDistantLight", [[%svg "<feDistantLight/>"]], [feDistantLight []] ;
  "fePointLight", [[%svg "<fePointLight/>"]], [fePointLight []] ;
  "feSpotLight", [[%svg "<feSpotLight/>"]], [feSpotLight []] ;
  "feBlend", [[%svg "<feBlend/>"]], [feBlend []] ;
  "feColorMatrix", [[%svg "<feColorMatrix/>"]], [feColorMatrix []] ;
  "feComponentTransfer",
  [[%svg "<feComponentTransfer/>"]], [feComponentTransfer []] ;
  "feFuncA", [[%svg "<feFuncA/>"]], [feFuncA []] ;
  "feFuncG", [[%svg "<feFuncG/>"]], [feFuncG []] ;
  "feFuncB", [[%svg "<feFuncB/>"]], [feFuncB []] ;
  "feFuncR", [[%svg "<feFuncR/>"]], [feFuncR []] ;
  "feComposite", [[%svg "<feComposite/>"]], [feComposite []] ;
  "feConvolveMatrix", [[%svg "<feConvolveMatrix/>"]], [feConvolveMatrix []] ;
  "feDiffuseLighting", [[%svg "<feDiffuseLighting/>"]], [feDiffuseLighting []] ;
  "feDisplacementMap", [[%svg "<feDisplacementMap/>"]], [feDisplacementMap []] ;
  "feFlood", [[%svg "<feFlood/>"]], [feFlood []] ;
  "feGaussianBlur", [[%svg "<feGaussianBlur/>"]], [feGaussianBlur []] ;
  "feImage", [[%svg "<feImage/>"]], [feImage []] ;
  "feMerge", [[%svg "<feMerge/>"]], [feMerge []] ;
  "feMorphology", [[%svg "<feMorphology/>"]], [feMorphology []] ;
  "feOffset", [[%svg "<feOffset/>"]], [feOffset []] ;
  "feSpecularLighting",
  [[%svg "<feSpecularLighting/>"]], [feSpecularLighting []] ;
  "feTile", [[%svg "<feTile/>"]], [feTile []] ;
  "feTurbulence", [[%svg "<feTurbulence/>"]], [feTurbulence []] ;
  "animateMotion", [[%svg "<animateMotion/>"]], [animateMotion []] ;
  "animateColor", [[%svg "<animateColor/>"]], [animateColor []] ;
  "animateTransform", [[%svg "<animateTransform/>"]], [animateTransform []] ;

]


let wrapping =
  let module Html = HtmlWrapped in
  "wrapping", HtmlWrappedTests.make Html.[

  "elem",
  !:[%html "<p></p>"],
  !:(p (nil ())) ;

  "child",
  !:[%html "<p><span></span></p>"],
  !:(p (span (nil ()) @: nil ())) ;

  "list",
  [%html "<p></p><span>foo</span>"],
  (p (nil()) @: span (txt !"foo" @: nil ()) @: nil()) ;

  "attrib",
  !:[%html "<p id=foo></p>"],
  !:(p ~a:[a_id !"foo"] (nil())) ;

  "attribs",
  !:[%html "<p id=foo class=bar></p>"],
  !:(p ~a:[a_id !"foo"; a_class !["bar"] ] (nil())) ;

  "comment",
  !:[%html "<!--foo-->"],
  !:(tot @@ Xml.comment "foo") ;

  "txt",
  !:[%html "<p>foo</p>"],
  !:(p (txt !"foo" @: nil ())) ;

  "wrapped functions",
  !:[%html "<input method=get />"],
  !:(input ~a:[a_method !`Get] ())

]


let elt1() = !: HtmlWrapped.(span !: (txt !"one"))
let elt2() = !: HtmlWrapped.(b !: (txt !"two"))
let id = !"pata"

let antiquot =
  let module Html = HtmlWrapped in
  "ppx antiquot", HtmlWrappedTests.make Html.[

  "child",
  !:[%html "<p>" (elt1()) "</p>"],
  !:(p (elt1()));

  "list child",
  !:[%html "<p>" (elt2()) "</p>"],
  !:(p (elt2()));

  "children",
  !:[%html "<p>bar"(elt1())"foo"(elt2())"baz</p>"],
  !:(p (txt !"bar" @: elt1() @-
        txt !"foo" @: elt2() @-
        txt !"baz" @: nil ()));

  "insertion",
  !:[%html "<p><em>" (elt1()) "</em></p>"],
  !:(p !:(em (elt1())));

  "attrib",
  !:[%html "<p id="id">bla</p>"],
  !:(p ~a:[a_id id] !:(txt !"bla"));

  "first child",
  [%html (elt1()) "<p></p>"],
  ((elt1()) @-  p (nil()) @: nil ());

  "last child",
  [%html "<p></p>" (elt1()) ],
  (p (nil()) @: (elt1()));

  "wrapped functions",
  !:[%html "<input method="!`Get" />"],
  !:(input ~a:[a_method !`Get] ())

  (* should succeed *)
  (* "escape", *)
  (* [%tyxml "<p>(tyxml4)</p>"], *)
  (* [p [txt "(tyxml4)"]]; *)


]

let tests = [
  basics ;
  attribs ;
  ns_nesting ;
  antiquot ;
  svg ;
  svg_element_names ;
  wrapping ;
]

let () = Alcotest.run "tyxml-ppx" tests
