# Markup.ml &nbsp; [![version 0.8.1][version]][releases] [![Travis status][travis-img]][travis] [![Coverage][coveralls-img]][coveralls]

[version]:       https://img.shields.io/badge/version-0.8.1-blue.svg
[travis]:        https://travis-ci.org/aantron/markup.ml/branches
[travis-img]:    https://img.shields.io/travis/aantron/markup.ml/master.svg
[coveralls]:     https://coveralls.io/github/aantron/markup.ml?branch=master
[coveralls-img]: https://img.shields.io/coveralls/aantron/markup.ml/master.svg

Markup.ml is a pair of parsers implementing the [HTML5][HTML5] and [XML][XML]
specifications, including error recovery. Usage is simple, because each parser
is a function from byte streams to parsing signal streams:

![Usage example][sample]

[sample]: https://github.com/aantron/markup.ml/blob/master/docs/sample.png

In addition to being error-correcting, the parsers are:

- **streaming**: parsing partial input and emitting signals while more input is
  still being received;
- **lazy**: not parsing input unless you have requested the next parsing signal,
  so you can easily stop parsing partway through a document;
- **non-blocking**: they can be used with [Lwt][lwt], but still provide a
  straightforward synchronous interface for simple usage; and
- **one-pass**: memory consumption is limited since the parsers don't build up a
  document representation, nor buffer input beyond a small amount of lookahead.

The parsers detect character encodings automatically, and emit everything in
UTF-8. The HTML parser understands SVG and MathML, in addition to HTML5.

Here is a breakdown showing the signal stream and errors emitted during the
parsing and pretty-printing of `bad_html`:

```ocaml
string bad_html         "<body><p><em>Markup.ml<p>rocks!"

|> parse_html           `Start_element "body"
|> signals              `Start_element "p"
                        `Start_element "em"
                        `Text ["Markup.ml"]
                        ~report (1, 10) (`Unmatched_start_tag "em")
                        `End_element                   (* /em: recovery *)
                        `End_element                   (* /p: not an error *)
                        `Start_element "p"
                        `Start_element "em"            (* recovery *)
                        `Text ["rocks!"]
                        `End_element                   (* /em *)
                        `End_element                   (* /p *)
                        `End_element                   (* /body *)

|> pretty_print         (* adjusts the `Text signals *)

|> write_html
|> to_channel stdout;;  "...shown above..."            (* valid HTML *)
```

The parsers are [tested][tests] thoroughly.

For a higher-level parser, see [Lambda Soup][lambdasoup], which is based on
Markup.ml, but can search documents using CSS selectors, and perform various
manipulations.

<br/>

## Overview and basic usage

The interface is centered around four functions between byte streams and signal
streams: [`parse_html`][parse_html], [`write_html`][write_html],
[`parse_xml`][parse_xml], and [`write_xml`][write_xml]. These have several
optional arguments for fine-tuning their behavior. The rest of the functions
either [input][input] or [output][output] byte streams, or
[transform][transform] signal streams in some interesting way.

Here is an example with an optional argument:

```ocaml
(* Show up to 10 XML well-formedness errors to the user. Stop after
   the 10th, without reading more input. *)
let report =
  let count = ref 0 in
  fun location error ->
    error |> Error.to_string ~location |> prerr_endline;
    count := !count + 1;
    if !count >= 10 then raise_notrace Exit

file "some.xml" |> fst |> parse_xml ~report |> signals |> drain
```

[input]: http://aantron.github.io/markup.ml/#2_Inputsources
[output]: http://aantron.github.io/markup.ml/#2_Outputdestinations
[transform]: http://aantron.github.io/markup.ml/#2_Utility

<br/>

## Advanced: [Cohttp][cohttp] + Markup.ml + [Lambda Soup][lambdasoup] + [Lwt][lwt]

This program requests a Google search, then does a streaming scrape of result
titles. It exits when it finds a GitHub link, without reading more input. Only
one `h3` element is converted into an in-memory tree at a time.

```ocaml
let () =
  Lwt_main.run begin
    (* Send request. Assume success. *)
    let url = "https://www.google.com/search?q=markup.ml" in
    let%lwt _, body = Cohttp_lwt_unix.Client.get (Uri.of_string url) in

    (* Adapt response to a Markup.ml stream. *)
    let body = body |> Cohttp_lwt.Body.to_stream |> Markup_lwt.lwt_stream in

    (* Set up a lazy stream of h3 elements. *)
    let h3s = Markup.(body
      |> strings_to_bytes |> parse_html |> signals
      |> elements (fun (_ns, name) _attrs -> name = "h3"))
    in

    (* Find the GitHub link. .iter and .load cause actual reading of data. *)
    h3s |> Markup_lwt.iter (fun h3 ->
      let%lwt h3 = Markup_lwt.load h3 in
      match Soup.(from_signals h3 $? "a[href*=github]") with
      | None -> Lwt.return_unit
      | Some anchor ->
        print_endline (String.concat "" (Soup.texts anchor));
        exit 0)
  end
```

This prints
`GitHub - aantron/markup.ml: Error-recovering streaming HTML5 and ...`. To run
it, do:

```sh
ocamlfind opt -linkpkg -package lwt.ppx,cohttp.lwt,markup.lwt,lambdasoup \
    scrape.ml && ./a.out
```

You can get all the necessary packages by

```
opam install lwt_ssl
opam install cohttp-lwt-unix lambdasoup markup
```

<br/>

## Installing

```
opam install markup
```

<br/>

## Documentation

The interface of Markup.ml is three modules: [`Markup`][Markup],
[`Markup_lwt`][Markup_lwt], and [`Markup_lwt_unix`][Markup_lwt_unix]. The last
two are available only if you have [Lwt][lwt] installed (OPAM package `lwt`).

The documentation includes a summary of the [conformance status][conformance] of
Markup.ml.

<br/>

## Depending

Markup.ml uses [semantic versioning][semver], but is currently in `0.x.x`. The
minor version number will be incremented on breaking changes.

<br/>

## Contributing

Contributions are very much welcome. Please see [`CONTRIBUTING`][contributing]
for instructions, suggestions, and an overview of the code. There is also a list
of [easy issues][easy].

<br/>

## License

Markup.ml is distributed under the [BSD license][license]. The Markup.ml source
distribution includes a copy of the HTML5 entity list, which is distributed
under the [W3C document license][w3c-license].

[releases]:        https://github.com/aantron/markup.ml/releases
[parse_html]:      http://aantron.github.io/markup.ml/#VALparse_html
[write_html]:      http://aantron.github.io/markup.ml/#VALwrite_html
[parse_xml]:       http://aantron.github.io/markup.ml/#VALparse_xml
[write_xml]:       http://aantron.github.io/markup.ml/#VALwrite_xml
[HTML5]:           https://www.w3.org/TR/html5/
[XML]:             https://www.w3.org/TR/xml/
[tests]:           https://github.com/aantron/markup.ml/tree/master/test
[signal]:          http://aantron.github.io/markup.ml/#TYPEsignal
[lwt]:             https://github.com/ocsigen/lwt
[lambdasoup]:      https://github.com/aantron/lambda-soup
[cohttp]:          https://github.com/mirage/ocaml-cohttp
[license]:         https://github.com/aantron/markup.ml/blob/master/LICENSE.md
[contributing]:    https://github.com/aantron/markup.ml/blob/master/docs/CONTRIBUTING.md
[email]:           mailto:antonbachin@yahoo.com
[Markup]:          http://aantron.github.io/markup.ml
[Markup_lwt]:      http://aantron.github.io/markup.ml/Markup_lwt.html
[Markup_lwt_unix]: http://aantron.github.io/markup.ml/Markup_lwt_unix.html
[conformance]:     http://aantron.github.io/markup.ml/#2_Conformancestatus
[w3c-license]: https://www.w3.org/Consortium/Legal/2002/copyright-documents-20021231
[semver]: http://semver.org/
[easy]: https://github.com/aantron/markup.ml/labels/easy
