`gen-book.sh` is the script that glues everything together.

`code/` contains all the code examples.  You can `(cd code && make)` to
rebuild all the files.  Every subdirectory in `code/` contains a set
of fragments that represent OCaml code or shell commands or a toplevel
fragment.

## Code blocks

The `code/` directory can contain the following file extensions:
* `ml` or `mli` are syntax-highlighted via COW.  They can contain a
  comment of the form `(* part 1 *)` which will split out that bit of
  the file into a new section.  Parts must be monotonically increasing.
* `json` is run through Pygments as a JSON file.
* `atd` is run through COW as an OCaml file, since it's pretty close.
* `topscript` is run through `./scripts/run_core_toplevel.byte`, which
  executes it as if it were running in a toplevel.  It's ok to have
  errors in the file as we sometimes want to display exceptions, so be
  very careful to look for errors in the output (see below).  You can
  break up a script into parts via a `#part 1` directive in the script.
* `sh` files are shell scripts that are run directly, with their output
  directed to a `out` file in the `code/` subdirectory.  If you need
  to have a dependency, place it in `code/Makefile` directly.
* `rawsh` files are assumed to be valid shell fragments, including the
  output, and are never executed.  Use this when you're sure that you
  wont need to regenerate the output, or want to manually tweak it.

When you `(cd code && make)`, `code/<subdir>/*.out` files are generated for all
fragments ending in `.sh` or `.rawsh`.  The `sh` files are executed and their
output redirected to `.out`.  The `rawsh` files are copied as-is and never
executed.

### HTML and Markdown output

The HTML and Markdown files are generated from the various files, and placed
into `code/_build`.  This directory will contain a set of files ending in
`.%d.md` or `.%d.html`, where the `%d` represents the part number of the source
fragment (this mainly applied to `ml` and `mli` and `topscript` files).

### Inserting a code fragment into the book

The Markdown is added by putting in a codeblock with `frag` as the language
type, and containing an sexp of type `script/code_frag.ml` (type t).  See the
`en/json.md` for some examples, also excerpted below:

```
((typ json)(name json/book.json))
((typ ocamltop)(name json/install.topscript)(header false))
((typ ocaml)(name json/yojson_basic.mli)(part 0))
((typ ocaml)(name json/yojson_basic.mli)(part 1))
((typ ocaml)(name json/yojson_basic_simple.mli))
((typ ocaml)(name json/read_json.ml))
((typ console)(name json/run_read_json.out))
((typ ocaml)(name json/parse_book.ml))
((typ console)(name json/run_parse_book.out))
((typ ocaml)(name json/list_excerpt.mli)(part 0))
((typ ocamltop)(name json/parse_book.topscript)(part 1))
((typ ocamltop)(name json/parse_book.topscript)(part 2))
((typ ocamltop)(name json/parse_book.topscript)(part 3))
((typ ocamltop)(name json/parse_book.topscript)(part 4))
((typ ocamltop)(name json/parse_book.topscript)(part 5))
((typ ocaml)(name json/yojson_basic.mli)(header false)(part 0))
((typ ocamltop)(name json/build_json.topscript)(part 1))
((typ ocamltop)(name json/build_json.topscript)(part 2))
((typ ocamltop)(name json/build_json.topscript)(part 3))
((typ ocamltop)(name json/build_json.topscript)(part 4))
((typ ocamltop)(name json/build_json.topscript)(part 5))
((typ atd)(name json/github.atd))
((typ console)(name json/run_github_org.out))
```

## Building the website

The website is built as follows:

* The sources in `en/` are passed through Pandoc, which outputs
  Docbook XML.
* The Docbook XML is run through `transform_pandocbook`, which 
  adds parts (not supported by Pandoc), and also part introductions.
* The XML is then passed through the Docbook XSL stylesheets,
  which generate chunked HTML (i.e. multiple HTML pages instead of
  one large one).
* The `commenting/` Python scripts convert the Docbook HTML into
  the pretty layout seen on the webpage.
* Finally, we run each HTML page through `html_syntax_highlight`,
  which hunts for any `<pre>` tags and substitutes the fragment
  with the correct file from `code/_build/<name>.<part>.html`.
  It assumes that these files have been built previously via the
  `make depend` step, or else an error occurs.

## Building the book

The book is built as follows:

* The Markdown in `en/` is passed through `./scripts/transform_markdown.native`, 
  which converts the fragment identifiers into concrete Markdown.  This
  also substitutes in `code/_build/<name>.<part>.md`.
* The XML is run through Pandoc and `transform_pandocbook` just
  as the web version is.
* The resulting `build/en/source/rwo-oreilly.xml` is SVN committed to
  O'Reilly's repo, and we anxiously wait 90 seconds for them to commit
  a PDF.  Any errors show up in `data/live_site/buildlog.txt`
