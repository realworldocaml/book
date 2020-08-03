1.5.1
-----

Additions

- Compatibility with OCaml 4.11 (#434, @kit-ty-kate)

1.5.0
-----

Additions

- Add option to turn warnings into errors (#398, Jules Aguillon)

Bugs fixed

- Emit quote before identifier in alias type expr (Fixes #391, Anton Bachin)
- Handle generalized open statements introduced in 4.08 (#393, Jon Ludlam)
- Refactor error reporting to avoid exiting the program in library code
  (#400, Jules Aguillon)
- Build on OCaml 4.10 (#408, Jon Ludlam)

1.4.2
-----

Bugs fixed

- Build on OCaml 4.09 (#383, Anil Madhavapeddy).
- Handle OCaml 4.08 type and module substitutions (#381, Jon Ludlam).
- Parser: better trimming of leading whitespace in code blocks (#370, Jules
  Aguillon).
- Parser: allow references to operators containing `:` characters (#384,
  reported Sylvain Le Gall).
- HTML: emit `<meta generator>` again (#378, Daniel Bünzli).
- CLI: `odoc html-targets` was ignoring deeply nested modules (#379, Daniel
  Bünzli).
- Fix bad internal usage of `List.iter2` (#376, Yotam Barnoy).

1.4.1
-----

Bugs fixed

- Messy formatting in large definitions due to lack of `<span>`s (#360, Thomas
  Refis).
- Missing table of contents on `.mld` pages (#361, Rizo Isrof).
- Missing space before polymorphic class names (#339, Kevin Ji).
- Module type definitions printed with `:` instead of `=` (#344, Geoff Reedy).
- Conjunctive types printed with `*` instead of `&` (#353, Florian Angeletti).
- Type extensions (`+=`) printed without CSS classes found in other items (#348,
  reported Stéphane Lavergne).
- High memory usage on large codebases (#361, Thomas Refis).
- Build: double underscores in internal filenames (#357, Thomas Refis).
- Development: test suite assumed that html5-tidy supports `--mute` (#345,
  Geoff Reedy).
- Internal: refactored AST (#351, #364, Jules Aguillon).

1.4.0
-----

Changes

- All parsing errors are now recoverable warnings (#238).
- Page titles are now level-0 headings (`{0 ...}`), and top-level sections
  within a page are level-1 headings (`{1 ...}`) (#217, Rizo Isrof).
- Don't render definitions of externals (#275, Nik Graf).
- Disable programming ligatures (#248).
- Rename `--root-uri` option to `--xref-base-uri` (#223, Rizo Isrof).
- Deprecate redundant reference kind annotations (#246).

Additions

- Preliminary compatibility with the current 4.08 beta releases (#309, Jon
  Ludlam).
- Paragraph headings (`{4 ...}`) and subparagraph headings (`{5 ...}`) (#217,
  Rizo Isrof).
- `odoc support-files-targets` command (#232).
- Recommend [`bsdoc`](https://ostera.github.io/bsdoc/docs/BsDoc/) for using
  odoc with BuckleScript (#269, Leandro Ostera).

Bugs fixed

- Improve breadcrumbs on `.mld` pages (#293, Daniel Buenzli).
- Display tables of contents in nested module and class pages (#261, Rizo
  Isrof).
- Uncaught exception when parsing references to operators with `-` in them,
  such as `@->` (#178).
- Incorrect parsing of references to operators with `.` in them, such as `*.`
  (#237).
- Assertion failure when processing optional arguments in an `.ml` file with a
  type annotation, when that type annotation uses an alias of `'a option`
  (#101).
- Assertion failure when two modules with the same name are found by odoc (#148,
  Jon Ludlam).
- Verbatim blocks (`{v ... v}`) can now only be terminated if the `v}` is
  immediately preceded by whitespace (#71, reported Daniel Buenzli).
- Wrong column numbers for errors reported in comments (#227, #253).
- Restore parsing of ocamldoc-style reference kind annotations (#244).
- Ordinary `type` keyword instead of `and` rendered in HTML for
  mutually-recursive types (#105, reported @Fourchaux).
- `nonrec` keyword not rendered (#249).
- `and` not rendered for mutually-recursive modules, classes, and class types
  (#251).
- Outer comment attached to a module rendered when the module is included (#87,
  Jon Ludlam).
- Polymorphic variant constructor documentation not rendered (#176, reported
  @steinuil).
- Variant constructor and record field documentation styled differently (#260,
  Jon Ludlam).
- Sloppy keyword markup in HTML output (#319).
- Rendering of multiple `constraint` clauses (#321).
- Incorrect order of functor arguments (#261, Rizo Isrof).
- `odoc html` option `-o` now creates the output directory if it does not exist
  #171, #264 Rizo Isrof).
- `odoc html-targets` output now includes path prefix given through `-o` option
  (#173, Rizo Isrof).
- Allow `-I` and `-o` options to refer to non-existent directories (#32, #170,
  Daniel Buenzli).
- Make `odoc compile-targets` match `odoc compile` (#273, Daniel Buenzli).
- `odoc compile-deps` does not work on `.cmt` files (#162, Daniel Buenzli).
- `odoc html-deps` now scans for `.odoc` files recursively (#307, Daniel
  Buenzli).
- `odoc html-targets` ignores stop comments (#276, Daniel Buenzli).
- `odoc html-targets` and `odoc html-deps` segfault on `.mld` pages (#277, #282,
  Daneil Buenzli).
- `--theme-uri` option not propagated to some subpages (#318, Thomas Refis).
- Binary files not opened in binary mode (#281, Ulrik Strid).

Build and development

- Always print backtraces for unhandled exceptions (3d10feb).
- CI on macOS (#216, Leandro Ostera).
- Test runner improvements (#266, Rizo Isrof).
- Fix esy builds in Travis (#301, Jon Ludlam).
- Don't require `make` in the esy build (#308, Leandro Ostera).
- Get rid of some large GADTs (#292, Jon Ludlam).
- Remove dependency on `bos` (#305, Daniel Buenzli).
- Remove dependency on `rresult` (#306, Daniel Buenzli).
- Remove dependency on `bisect_ppx`, previously present in development
  checkouts (#316).

1.3.0
-----

Additions

- Reason syntax output (#156, Patrick Stapfer).
- BuckleScript support (#179, Leandro Ostera).
- New CSS, appearance (#139, Rizo Isrof).
- Table of contents for the sections in each page (fe26392).
- Navigation breadcrumbs, and limit length of module paths in page
  titles (#150, Yotam Barnoy).
- Syntax highlighting of code blocks (99f2be9).
- Compiled `odoc` binary is now self-contained and requires no external
  files (bd3b53c).
- `--theme-uri` option (#154, Rizo Isrof).
- Option to convert `.mld` to HTML fragments rather than complete pages
  (#166, Rizo Isrof).

Bugs fixed

- Use regular dashes in arrows to support ligature fonts (#180, Leandro
  Ostera).
- Do not excessively indent code blocks (#133, Bobby Priambodo).
- Always prepend `page-` to output file name when compiling `.mld` files
  (#183, Rizo Isrof).
- Support `floatarray` type introduced in OCaml 4.06 (eb36158, Thomas
  Refis).
- Support destructive type substitution (57cbb4e, Thomas Refis).
- Render `<i>` tags in italics (#104, Thibault Suzanne).
- Flush HTML output correctly (#167, Rizo Isrof).
- Make HTML output more valid (#185, Leandro Ostera).
- Various improvements to parsing, output, documentation, and the
  development workflow (Yotam Barnoy, Luke Czyszczonik, Mohamed
  Elsharnouby, Rudi Grinberg, Rizo Isrof, Leandro Ostera, Bobby
  Priambodo, Thomas Refis, Patrick Stapfer).

Build and development

- `odoc` is now one repo.
- Dropped several dependencies.
- Considerable refactoring.
- New commnt parser (78a6699).
- Improved development workflow, including `CONTRIBUTING.md`, tests,
  coverage analysis, CI, and issue organization.
- Initial NPM packaging (Leandro Ostera, #214).
- Skeleton of odoc manual (Leandro Ostera, #203).

1.2.0
-----

- Support for standalone documentation pages (`.mld` files) (#61).
- Display `[@@deprecated]` attributes as the `@deprecated` tag (#57).
- Allow each component of OCaml paths to be disambiguated using the `kind-identifer` syntax (part of #61).
- Support OCaml 4.06.
- Fix spurious leading blank lines in verbatim text (ocaml-doc/octavius#6).

1.1.1
-----

- make odoc more noisy when generating html for hidden units

- changed `html-deps` subcommand behavior: it now expects to be given a
  directory, not a single odoc file.

1.1.0
-----

- switch to jbuilder

1.0.0
-----

Initial release.
