### 1.4.0 (2019-06-11)
- Add `--force-output` option to force generation of diff file (#118 @clecat)
- Support OCaml 4.08.0 (#121 @xclerc)
- README and documentation fixes (#122 #118 @andreypopp @clecat @samoht)
- Use latest ocaml-migrate-parsetree interfaces (@avsm)

### 1.3.0 (2019-03-01)
- Updated readme file with the new features: dune rules, named environment and
  ocaml versions, Some grammar correction too (@gpetiot, #101, aantron, #102)
- Better lexer error messages (@avsm, #103)
- Added cram syntax parsing (@trefis, #106)
- Renamed mdx to ocaml-mdx to avoid conflicts/for more precision (@clecat, #110, #115)
- Fix blank spaces causing parsing errors (@gpetiot, #97)
- Fix empty lines causing a String.sub (@clecat, #107)

### 1.2.0 (2018-01-03)

- Support end-of-line ellipsis (@dra27, #85)
- Support OCaml 4.02.3 (@gpetiot, #86)
- Support `version=..`, `version<=..` and `version>=..` keywords to run
  a code-block depending on the currently installed OCaml version
  (@gpetiot, #87, #90)
- Upgrade Travis tests to use opam 2.0.2 (@avsm, #89)
- Do not depend on `ppx_tools` for toplevel (@avsm, #89)
- Fix embedding in a larger Dune project with a cppo override (@avsm, #89)
- `mdx output`: escape HTML entities in code blocks (#91, @samoht)

### 1.1.0 (2018-11-16)

- Add a mechanism to promote files to blocks and blocks to file
  (@gpetiot, #37)
- Support multiple toplevel environments (@gpetiot, #38)
- Use ocaml-migrate-parsetree to compile in 4.06.1 & 4.07.0 (@gpetiot, #41)
- Add a `mdx rule` command to generate dune rules (@gpetiot, #44)
- Add a `mdx output` command to generate an HTML document (@samoht, #45)
- Support empty code blocks (@samoht, #46)
- Fix detection of OCaml code/toplevel (@samoht, #47)
- Better handling of multi-line shell scripts (@samoht, #48)
- Fix regression in toplevel blocks when creating newtype (@samoht, #49)
- Fix evaluation of non-determinitic test (@samoht, #50)
- Improve mdx rules to take into account more precise dependencies (@samoht, #51)
- Fix promotion of blocks to complete ML files (@samoht, #52)
- mdx does not use the `cppo` library, just the binary (@samoht, #53)
- fix ellipsis in code blocks (@samoht, #57)
- Fix relative paths for promoted blocks to files (@samoht, #58)
- Fix location of errors for multi-line commands (@samoht, #60)
- improve the parser for shell blocks (@samoht, #61)
- Allow to load preludes in specific environments (@samoht, #63)
- Fix evaluation of code after directives in prelude (@samoht, #64)
- Improve promotion to ml files (@samoht, #66)
- mdx rule: generates (source_tree) dependencies for directory metadata (@samoht, #67)
- Fix handling of 'module type' in multiple toplevel environment (@samoht, #68)
- Add an eval=false label to skip the evaluation of a code block (@samoht, #69)
- Fix parsing of shell blocks with multiple exit codes (@samoht, #71)
- Support source-tree as extra block metadata (@samoht, #72)
- Better formatting of non-compiling promoted contents (@samoht, #73)
- Be sure to remove the .corrected files if the promotion to ML file works (mdx74)
- Add missing dependency in test/dune (@samoht, #75)
- Support `dir=..` labels in ml code blocks (@samoht, #76)
- Allow to promote to mli files too (@samoht, #77)
- Support multi-line strings (@samoht, #78)
- fail (and exit 1) if prelude and ml blocks cannot be evaluated properly
  (@samoht, #80, @samoht, #83)
- Allow to pass --root to `mdx rule` (@samoht, #81)
- mdx rule: do not add (package mdx) in the dependencies (@samoht, #82)

## 1.0.0 (2018-09-23)

- Initial release
