# ocp-index

`ocp-index` is designed as a simple and light-weight documentation extractor for
OCaml, for command-line use or integrated in other tools (e.g. for
completion). It gathers information from `.cmi` (à la ocamlbrowser) and
`.cmt`/`cmti` files, including structure, location, type, and ocamldoc comments
when available.

<hr/>
<div class="span12">
<h3>Ressources</h3>
<table class="table table-striped">
  <tr><td><a href="http://www.github.com/OCamlPro/ocp-index">ocp-index on Github</a></td>
  <td>Latest sources in the official GIT repository</td></tr>
</table>
</div>
<br/><br/>

## Usage

`ocp-index COMMAND params OPTIONS`

Examples:
* `ocp-index type Module.ident`
* `ocp-index complete iden`
* `ocp-index locate Module.ident`
* `ocp-index print Module.ident <format>`

Options:
* `-I` include dirs / loaded libraries
* `-O` consider given module as open
* `-F` open the given module, not limiting to its exported interface
* `--context` automatically open/bind modules according to the given source file position.
* `--root` specify the root of the current project, for finding cmt files and source lookups. It's generally safe to let ocp-index guess.

* output format: `--color`, `--show`/`--hide` to control the kinds of idents to
  display
* `--format FORMAT`, display using the given format string:
    <table>
    <tr><th>string<th>contents<th>examples
    <tr><td><code>%n</code><td>name<td>"map"
    <tr><td><code>%q</code><td>qualified ident in context<td>"List.map", "map"
    <tr><td><code>%p</code><td>full ident path<td>"List.map"
    <tr><td><code>%k</code><td>ident kind<td>"type", "val", "exception", "field(<type>)"...
    <tr><td><code>%t</code><td>type<td>('a -> 'b) -> 'a list -> 'a list
    <tr><td><code>%d</code><td>ocamldoc comment<td>"Applies the function..."
    <tr><td><code>%l</code><td>Implementation location<td>"src/list.ml:83:0"
    <tr><td><code>%s</code><td>Interface location<td>"src/list.mli:51:0"
    <tr><td><code>%f</code><td>File of origin<td>"_build/list.cmti"
    <tr><td><code>%i</code><td>Summary<td>List.map val ('a -> 'b) -> 'a list -> 'b list
    </table>

## Build

```
./configure
make
make install
```
See below to compile and install the optional `ocp-browser`.

## Other tools

### Emacs mode

A script `ocp-index.el` is included under `tools/`, and can be used together
with tuareg-mode or ocaml-mode and
[auto-complete](https://github.com/auto-complete/auto-complete) (packaged as
`auto-complete-el` in Debian) to get completions and types in a popup menu.

You can run the script `tools/emacs-setup.sh` to get hints on the configuration
of emacs for ocp-index (it won't modify any files). Adding the following
line to your `.emacs`:
```lisp
(add-to-list 'load-path "/path/to/ocp-index.el/")
(require 'ocp-index)
```
Will give you:
- `C-c TAB` to auto-complete (`(global-set-key (kbd "KEY") 'auto-complete)` to add
  your own binding)
- `C-c t` to print the type of the identifier under cursor
- `C-c ;` to jump to the definition of the identifier under cursor (use `C-c C-;` to do that in the current window)
- `C-c :` to jump to the interface of the identifier under cursor (use `C-c C-:` to do that in the current window)
- `C-c /` to lookup all occurences of the ident under point in the current project (ocp-grep)

See `M-x customize ocp-index` for more options.

### Vim

A script `ocp-index.vim`, contributed by Daisuke Inajima, is available under
`tools`. It supports:
* omni completion
* type information printing
* jump to definitions

To use, add vim-ocp-index directory to runtimepath:
```
:set runtimepath^=/path/to/ocp-index.vim
```

Then create your own `after/ftplugin/ocaml.vim` to override vim's
builtin ocaml settings::
```
if exists('b:did_ftplugin_after')
    finish
endif
let b:did_ftplugin_after = 1

call ocpindex#init()

nmap <buffer> K         <Plug>(ocpindex-print)
nmap <buffer> <C-]>     <Plug>(ocpindex-jump)
nmap <buffer> <C-t>     <Plug>(ocpindex-jump-back)
```
You get:
- `K` Echo type information and documentation of the identifier under the cursor
- `C-]` Push the current position to the jump stack and jump to the definition of the identifier under the cursor
- `C-t` Pop the previons position from the jump stack and jump back there
- `C-x C-o` Omni completion

If needed, you can specify ocp-index path explicitly: `let g:ocpindex_program = "/path/to/ocp-index"`


### Sublime Text

There is a binding written by Peter Zotov on [Github](https://github.com/whitequark/sublime-ocp-index)

### ocp-browser

<img src="ocp-browser.gif" alt="ocp-browser animated screenshot"></img>

A nice terminal interface leveraging the power of ocp-index is included. It
provides a quick way to browse external and in-project interfaces. Thanks to
the contribution from [Gabriel Radanne](https://gihub.com/Drup).

To compile it, make sure that you have lambda-term installed, e.g. `opam install
lambda-term ocp-index`.
