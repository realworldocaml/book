### 1.1.9
* Support for OCaml 4.08
* Fix issue with latest dune that led to silently compiling dummy binaries

### 1.1.8
* Fix lazy loading regression in 1.1.7

### 1.1.7
* Support for OCaml 4.07
* Drop support for OCaml < 4.02
* Emacs mode: fix a few issues (capitalisation of filenames)
* ocp-browser: sort output, add hints and show doc by default, skip `Foo__bar`
  when `Foo` exists

### 1.1.6
* Fixed escaping issue in tuareg doc mode
* Emacs mode: support eldoc
* Vim mode: add info printing on 'K'
* Split ocp-browser into a separate opam package
* Move build system to jbuilder and reorganise source tree

### 1.1.5
* Use preprocessing for compatibility with multiple OCaml versions
* Add compatibility with OCaml 4.03, 4.04
* Improve support for completion in Emacs using company-mode

### 1.1.4
* emacs: more interactive ocp-grep call (C-c /) ; better printing of type/doc
* use `cmt_builddir` information to more reliably point to the sources
* improved and extended format-strings and formatting options
* Some basic handling of distributive {Mod.x; y}

### 1.1.3
* some small elisp improvements
* add '%e' format string for enclosing type definition (useful to get "other
  variants", "other fields"). Repeat C-c C-t in emacs mode to use.
* compatible with new 4.02.3 doc attached to the parse-tree (4.02 branch)

### 1.1.2
* much improved vim script
* better output formatting
* give location of full modules (previously, only values within the module had
  location). Useful for in-editor file jumping
* sort results by location (to get the same source idents in mli order, rather
  than somewhat but not really alphabetical order)
* allow escapes in --format
* ocp-browser: add global toggle with Alt-a
* fixed some cases of recursive lookup within the same file

### 1.1.1
* fixed comment association for modules and one-per-line values
* better errors on cmi/cmt from incompatible compiler versions
* fixed version reporting

### 1.1.0
* completely rewritten ocp-browser by Gabriel Radanne
* supports OCaml 4.02 (with git branching)
* respect file loading order, which avoids some lookup loop bugs
* fixed and disabled type qualification by default
* fixed handling of functors
* get implementation positions through module aliases

### 1.0.3
* improved emacs mode, now supporting both auto-comlete and company
* fixed return value of ocp-grep
* added '--format' to the complete command
* fixed some case of laziness that could fail with some nested module aliases
* better man-page

### 1.0.2
* much more expressive command-line option "print" to access more known info
* cleaner command-line include options
* ocp-grep can now search in program strings (with -s "string" or -e "regex")
* ocp-grep: display relative paths
* more reliable printing of ident paths before/after opens
* better project root lookup
* emacs: more reliable process call
* emacs: added `C-c /`, find-uses-of-indent-under-point
* emacs: completion mode without auto-complete (contribution by Nicolas Ojeda Bar)
* emacs: show bubble on completed ident for quicker doc access

### 1.0.1
* add the ocp-grep tool, looking for uses of a given fully-qualified identifier in a source tree
* better vim mode contributed by Anyakichi
* fixed default lookup path
* emacs: better definition of keybindings, cleaner loading, workaround auto-complete bug
* refactored build system, installing through opam-installer and register as ocamlfind sub-modules
* emacs: replace `C-c t` by `C-c C-t` to follow emacs minor-mode guidelines
* emacs: better handling of process environment to work properly with OPAM on OSX
