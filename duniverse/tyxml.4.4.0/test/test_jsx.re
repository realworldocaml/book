/** Ppx Tests

    This file is here to torture the jsx ppx. Tests that are directly related to
    html or svg should go to the other files.
*/
open Tyxml_test;

let basics = (
  "ppx basics",
  HtmlTests.make(
    Html.[
      ("elems", [<p />], [p([])]),
      (
        "child",
        [<p> <span> "foo" </span> </p>],
        [p([span([txt("foo")])])],
      ),
      (
        "list",
        <> <p /> <span> "foo" </span> </>,
        [p([]), span([txt("foo")])],
      ),
      ("attrib", [<p id="foo" />], [p(~a=[a_id("foo")], [])]),
      (
        "attribs",
        [<p id="foo" className="bar" />],
        [p(~a=[a_id("foo"), a_class(["bar"])], [])],
      ),
      ("txt", <> "foo" </>, [txt("foo")]),
      (
        "document",
        [<html> <head> <title> "foo" </title> </head> <body /> </html>],
        [html(head(title(txt("foo")), []), body([]))],
      ),
      (
        "let",
        [
          {
            let x = <p />;
            x;
          },
        ],
        [p([])],
      ),
      (
        "nested let",
        [
          {
            let _ = <p />;
            let y = <p />;
            y;
          },
        ],
        [p([])],
      ),
      (
        "let and",
        {
          let x = <p />
          and y = <a />;
          [x, y];
        },
        [p([]), a([])],
      ),
      (
        "let fun",
        [
          {
            let f = x => <p> x </p>;
            f(a([]));
          },
        ],
        [p([a([])])],
      ),
      (
        "let fun spread",
        [
          {
            let f = x => <p> ...{x} </p>;
            f([a([])]);
          },
        ],
        [p([a([])])],
      )      
    ],
  ),
);

let attribs = (
  "ppx attribs",
  HtmlTests.make(
    Html.[
      ( "unit",
        [<div hidden="" />],
        [div(~a=[a_hidden()], [])]
      ),
      ( "bool default",
        [<div draggable="true" />],
        [div(~a=[a_draggable(true)], [])],
      ),
      ( "bool true",
        [<div draggable=true />],
        [div(~a=[a_draggable(true)], [])],
      ),
      ( "bool false",
        [<div draggable=false />],
        [div(~a=[a_draggable(false)], [])],
      ),
      ( "onoff default",
        [<form autocomplete="" />],
        [form(~a=[a_autocomplete(true)], [])],
      ),
      ( "bool true",
        [<form autocomplete="on" />],
        [form(~a=[a_autocomplete(true)], [])],
      ),
      ( "bool false",
        [<form autocomplete="off" />],
        [form(~a=[a_autocomplete(false)], [])],
      ),
      ( "link rel=canonical",
        [<link rel=[`Canonical] href="/" />],
        [link(~rel=[`Canonical], ~href="/", ())],
      ),
      ( "embed type",
        [<embed type_="text/plain" />],
        [embed(~a=[a_mime_type("text/plain")], ())],
      ),
      ( "output for",
        [<output htmlFor="foo" />],
        [output_elt(~a=[a_output_for(["foo"])], [])],
      ),
      ( "input min time",
        [<input min="2002-10-02T15:00:00Z" />],
        [input(~a=[a_input_min(`Datetime("2002-10-02T15:00:00Z"))], ())],
      ),
      ( "aria attributes",
        [<div ariaHidden=["true"] />],
        [div(~a=[a_aria("hidden", ["true"])], [])],
      ),
      ( "touch events",
        [<div ontouchstart="alert()" />],
        [div(~a=[a_ontouchstart("alert()")], [])],
      ),
      ( "empty string as referrer policy",
        [<iframe referrerpolicy="" />],
        [iframe(~a=[a_referrerpolicy(`Empty)], [])],
      ),
      ( "dashes in referrer policy",
        [<iframe referrerpolicy="no-referrer-when-downgrade" />],
        [iframe(~a=[a_referrerpolicy(`No_referrer_when_downgrade)], [])],
      ),
    ],
  ),
);

let ns_nesting = (
  "namespace nesting",
  HtmlTests.make(
    Html.[
      ( "html/svg",
        [<Html.Svg> <g /> </Html.Svg>],
        [svg([Svg.g([])])]
      ),
      ( "nested svg",
        [<div> <svg> <g /> </svg> </div>],
        [div([svg([Svg.g([])])])],
      ),
      ( "with_neighbour",
        [<div> <span /> <svg> <g /> </svg> "foo" </div>],
        [div([span([]), svg([Svg.g([])]), txt("foo")])],
      ),
      ( "ambiguous tag",
        [<Html.Svg> <a /> </Html.Svg>],
        [svg([Svg.a([])])]
      ),
    ],
  ),
);

let svg = (
  "svg",
  SvgTests.make(
    Svg.[
      ( "basic",
        [<Svg.Svg />],
        [svg([])]
      ),
      ( "transform",
        [<line transform="translate(1) translate(2)" />],
        [
          line(
            ~a=[
              a_transform([`Translate((1., None)), `Translate((2., None))]),
            ],
            [],
          ),
        ],
      ),
      ( "offset percentage",
        [<stop offset="50.1%" />],
        [stop(~a=[a_offset(`Percentage(50.1))], [])],
      ),
      ( "text x, y",
        [<text x="1 2" y="3 4" />],
        [
          text(
            ~a=[
              a_x_list([(1., None), (2., None)]),
              a_y_list([(3., None), (4., None)]),
            ],
            [],
          ),
        ],
      ),
      ( "text dx, dy",
        [<text dx="1 2" dy="3 4" />],
        [
          text(
            ~a=[
              a_dx_list([(1., None), (2., None)]),
              a_dy_list([(3., None), (4., None)]),
            ],
            [],
          ),
        ],
      ),
      (
        "feColorMatrix type",
        [<feColorMatrix type_="matrix" />],
        [feColorMatrix(~a=[a_feColorMatrix_type(`Matrix)], [])],
      ),
      (
        "feTurbulence type",
        [<feTurbulence type_="fractalNoise" />],
        [feTurbulence(~a=[a_feTurbulence_type(`FractalNoise)], [])],
      ),
      (
        "animateTransform type",
        [<animateTransform type_="translate" />],
        [animateTransform(~a=[a_animateTransform_type(`Translate)], [])],
      ),
      (
        "feFuncR type, offset",
        [<feFuncR type_="identity" offset="0" />],
        [
          feFuncR(
            ~a=[a_transfer_type(`Identity), a_transfer_offset(0.)],
            [],
          ),
        ],
      ),
      (
        "feComposite operator",
        [<feComposite operator="xor" />],
        [feComposite(~a=[a_feComposite_operator(`Xor)], [])],
      ),
      (
        "feMorphology operator",
        [<feMorphology operator="erode" />],
        [feMorphology(~a=[a_feMorphology_operator(`Erode)], [])],
      ),
      (
        "animation fill, values",
        [<animation fill="freeze" values="1 2" />],
        [
          animation(
            ~a=[a_animation_fill(`Freeze), a_animation_values(["1", "2"])],
            [],
          ),
        ],
      ),
    ],
  ),
);

let svg_element_names = (
  "svg element names",
  SvgTests.make(
    Svg.[
      ("textPath", [<textPath />], [textPath([])]),
      ("linearGradient", [<linearGradient />], [linearGradient([])]),
      ("radialGradient", [<radialGradient />], [radialGradient([])]),
      ("clipPath", [<clipPath />], [clipPath([])]),
      ("feDistantLight", [<feDistantLight />], [feDistantLight([])]),
      ("fePointLight", [<fePointLight />], [fePointLight([])]),
      ("feSpotLight", [<feSpotLight />], [feSpotLight([])]),
      ("feBlend", [<feBlend />], [feBlend([])]),
      ("feColorMatrix", [<feColorMatrix />], [feColorMatrix([])]),
      (
        "feComponentTransfer",
        [<feComponentTransfer />],
        [feComponentTransfer([])],
      ),
      ("feFuncA", [<feFuncA />], [feFuncA([])]),
      ("feFuncG", [<feFuncG />], [feFuncG([])]),
      ("feFuncB", [<feFuncB />], [feFuncB([])]),
      ("feFuncR", [<feFuncR />], [feFuncR([])]),
      ("feComposite", [<feComposite />], [feComposite([])]),
      ("feConvolveMatrix", [<feConvolveMatrix />], [feConvolveMatrix([])]),
      (
        "feDiffuseLighting",
        [<feDiffuseLighting />],
        [feDiffuseLighting([])],
      ),
      (
        "feDisplacementMap",
        [<feDisplacementMap />],
        [feDisplacementMap([])],
      ),
      ("feFlood", [<feFlood />], [feFlood([])]),
      ("feGaussianBlur", [<feGaussianBlur />], [feGaussianBlur([])]),
      ("feImage", [<feImage />], [feImage([])]),
      ("feMerge", [<feMerge />], [feMerge([])]),
      ("feMorphology", [<feMorphology />], [feMorphology([])]),
      ("feOffset", [<feOffset />], [feOffset([])]),
      (
        "feSpecularLighting",
        [<feSpecularLighting />],
        [feSpecularLighting([])],
      ),
      ("feTile", [<feTile />], [feTile([])]),
      ("feTurbulence", [<feTurbulence />], [feTurbulence([])]),
      ("animateMotion", [<animateMotion />], [animateMotion([])]),
      ("animateColor", [<animateColor />], [animateColor([])]),
      ("animateTransform", [<animateTransform />], [animateTransform([])]),
    ],
  ),
);

let wrapping = {
  module Html = HtmlWrapped;
  (
    "wrapping",
    HtmlWrappedTests.make(
      Html.[
        ("elem", !:(<p />), !:p(nil())),
        ("child", !:(<p> <span /> </p>), !:p(span(nil()) @: nil())),
        (
          "list",
          <> <p /> <span> "foo" </span> </>,
          p(nil()) @: span(txt("foo"^) @: nil()) @: nil(),
        ),
        ("attrib", !:(<p id="foo" />), !:p(~a=[a_id("foo"^)], nil())),
        (
          "attribs",
          !:(<p id="foo" className="bar" />),
          !:p(~a=[a_id("foo"^), a_class(["bar"]^)], nil()),
        ),
        ("txt", !:(<p> "foo" </p>), !:p(txt("foo"^) @: nil())),
        (
          "wrapped functions",
          !:(<input method_="get" />),
          !:input(~a=[a_method(`Get^)], ()),
        ),
      ],
    ),
  );
};

let elt1 = () => !:HtmlWrapped.(span(!:txt("one"^)));
let elt2 = () => !:HtmlWrapped.(b(!:txt("two"^)));
let id = "pata"^;

let antiquot = {
  module Html = HtmlWrapped;
  (
    "ppx antiquot",
    HtmlWrappedTests.make(
      Html.[
        ( "child",
          !:(<p> ...{elt1 ()} </p>),
          !:p(elt1())
        ),
        ( "list child",
          !:(<p> ...{elt2 ()} </p>),
          !:p(elt2())
        ),
        /* ( "children",
          !:(<p> "bar" (elt1()) "foo" (elt2()) "baz" </p>),
          !:
            p(
              txt("bar"^)
              @: elt1()
              @- txt("foo"^)
              @: elt2()
              @- txt("baz"^)
              @: nil(),
            ),
        ), */
        ( "insertion",
          !:(<p> <em> ...{elt1 ()} </em> </p>),
          !:p(!:em(elt1()))
        ),
        (
          "attrib",
          !:(<p id> "bla" </p>),
          !:p(~a=[a_id(id)], !:txt("bla"^)),
        ),
        /* ( "first child",
          <> ...{elt1()} <p /> </>,
          elt1() @- p(nil()) @: nil()
        ),
        ( "last child",
          <> <p /> ...{elt1()} </>,
          p(nil()) @: elt1()
        ), */
        ( "wrapped functions",
          !:(<input method="get" />),
          !:input(~a=[a_method(`Get^)], ()),
        ),
      ],
    ),
  );
};

let foo (~children,()) = children

let enable = (
  "jsx basics",
  [ ("enable jsx",
     `Quick,
     () =>
       Alcotest.check
         (Alcotest.list(Alcotest.string),"enable",
          {module A = { [@tyxml.jsx false]; let a = <foo/> }; A.a},
          [])
    )
  ]
);

let tests = [
  basics,
  attribs,
  ns_nesting,
  antiquot,
  svg,
  svg_element_names,
  wrapping,
  enable
];

let () = Alcotest.run("tyxml-jsx", tests);
