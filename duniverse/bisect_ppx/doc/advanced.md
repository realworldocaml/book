# Bisect_ppx advanced usage

Several sections below give options that can be passed to the Bisect_ppx
preprocessor using the Ocamlfind option `-ppxopt`. The same options can be
passed using Ocamlbuild, using the tag `ppxopt`, and to Dune, by simply listing
them after `bisect_ppx` in the `pps` list.

#### Table of contents

- [Building with coverage](#Building)
  - [With Dune](#Dune)
  - [With Ocamlfind](#Ocamlfind)
  - [With Ocamlbuild](#Ocamlbuild)
  - [With OASIS](#OASIS)
- [Excluding code from coverage](#Excluding)
  - [Individual lines and line ranges](#ExcludingLines)
  - [Files and top-level values](#ExcludingValues)
- [Environment variables](#EnvironmentVariables)
  - [Naming the output files](#OutFiles)
  - [Logging](#Logging)
- [File formats](#FileFormats)
  - [Output files](#OutputFileFormat)
- [Installing without OPAM](#WithoutOPAM)



<br>

<a id="Building"></a>
## Building with coverage

While developing your project, you typically have some source files, for example
in `src/`, and some files with tests, for example in `tests/`. For testing, you
will want Bisect_ppx to preprocess the files in `src/`, but *not* the files in
`tests/`. You will also want to link the testing binary with Bisect_ppx. On the
other hand, when building for release, you will want to make sure that *nothing*
is preprocessed by, or linked with, Bisect_ppx. The way to achieve this depends
on your build system.

<a id="Jbuilder"></a>
<a id="Dune"></a>
#### With Dune

Dune currently doesn't support Bisect_ppx very well. There isn't a good way to
turn Bisect_ppx on and off conditionally during development, nor to permanently
disable it for release. However, Bisect_ppx provides a pretty decent workaround:

1. List Bisect_ppx in your `dune` files, and pass the `-conditional` flag to it:

        (library
         (name my_lib)
         (preprocess (pps bisect_ppx -conditional)))

2. `-conditional` is what turns on the workaround. It makes Bisect_ppx *not*
   instrument your code by default. You can conditionally enable instrumentation
   using the `BISECT_ENABLE` environment variable. Just set it to `YES` in your
   `Makefile`, or whatever you use to trigger Dune:

        .PHONY : coverage
        coverage :
            rm -f `find . -name 'bisect*.out'`
            BISECT_ENABLE=YES dune runtest --force
            bisect-ppx-report -I _build/default/ -html _coverage/ \
              `find . -name 'bisect*.out'`

   Your other rules are not affected.

   Note that `--force` is used to make sure Dune runs all the tests. Normally,
   Dune doesn't rerun tests if the code they are testing hasn't changed.

   The `find . -name 'bisect*.out'` is needed because Dune changes directory
   while running the tests, and the `.out` files end up written there. The
   directory usually ends up being something like `_build/default/tests/`, but
   if you have multiple test targets, there will be multiple directories.

3. For release, we recommend manually removing `bisect_ppx -conditional` from
   your `dune` files. If you don't want to do that, you can add a dependency on
   package `bisect_ppx` to your `opam` files:

        depends: [
          "bisect_ppx" {build & >= "1.3.0"}
        ]

   We hope to eliminate this chore in the future.

See [Dune issue #57][dune-bisect] for discussion of Dune/Bisect_ppx
compatibility.

[dune-bisect]: https://github.com/ocaml/dune/issues/57

<a id="Ocamlfind"></a>
#### With Ocamlfind

You will have to make your build script issue the right commands. When building
tests:

```
ocamlfind c -package bisect_ppx -c src/source.ml
ocamlfind c -linkpkg -package bisect_ppx src/source.cmo tests/tests.ml
```

<a id="Ocamlbuild"></a>
#### With Ocamlbuild

The easiest way with Ocamlbuild is to use
[`Bisect_ppx_plugin`][Bisect_ppx_plugin]. This gives you a new tag, `coverage`,
which you can use to mark your source files for coverage analysis by Bisect_ppx.
The plugin is in the [public domain][unlicense], so you can freely link with it,
customize and incorporate it, and/or include it in releases.

It is used like this:

1. Install the plugin:

        opam install bisect_ppx-ocamlbuild

1. Create a `myocamlbuild.ml` file in your project root, with the following
   contents:

        open Ocamlbuild_plugin
        let () = dispatch Bisect_ppx_plugin.dispatch

   If you already have `myocamlbuild.ml`, you just need to call
   `Bisect_ppx_plugin.handle_coverage ()` somewhere in it.
2. Add `-use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'` to your
   Ocamlbuild invocation.
3. <a id="Tagging"></a> Now, you have a new tag available, called `coverage`.
   Make your `_tags` file look something like this:

        <src/*>: coverage                           # For instrumentation
        <tests/test.{byte,native}>: coverage        # For linking

4. Now, if you build while the environment variable `BISECT_COVERAGE` is set to
   `YES`, the files in `src` will be instrumented for coverage analysis.
   Otherwise, the tag does nothing, so you can build the files for release. So,
   to build, you will have two targets with commands like these:

        # For tests
        BISECT_COVERAGE=YES ocamlbuild -use-ocamlfind \
            -plugin-tag 'package(bisect_ppx-ocamlbuild)' tests/test.native --

        # For release
        ocamlbuild -use-ocamlfind \
            -plugin-tag 'package(bisect_ppx-ocamlbuild)' src/my_project.native

If you don't want to make Bisect_ppx a hard build dependency just for the
`coverage` tag, you can work the [contents][plugin-code] of `Bisect_ppx_plugin`
directly into your `myocamlbuild.ml`. Use them to replace the call to
`Bisect_ppx_plugin.dispatch`. In that case, you should omit the second step.

<a id="OASIS"></a>
#### With OASIS

Since OASIS uses Ocamlbuild, the instructions are similar:

1. Install the plugin:

        opam install bisect_ppx-ocamlbuild

1. At the top of your `_oasis` file are the *package fields*, such as the name
   and version. Add these:

        OCamlVersion:           >= 4.01
        AlphaFeatures:          ocamlbuild_more_args
        XOCamlbuildPluginTags:  package(bisect_ppx-ocamlbuild)

   Then, run `oasis setup`.
2. You should have a `myocamlbuild.ml` file in your project root. Near the
   bottom, after `(* OASIS_STOP *)`, you will have a line like this one, if you
   have not yet modified it:

        Ocamlbuild_plugin.dispatch dispatch_default;;

   replace it with

        let () =
          dispatch
            (MyOCamlbuildBase.dispatch_combine
               [MyOCamlbuildBase.dispatch_default conf package_default;
                Bisect_ppx_plugin.dispatch])

3. This enables the `coverage` tag. Tag your source files as
   [described in the Ocamlbuild instructions](#Tagging). Insert the tags after
   the line `# OASIS STOP`.
4. Use the `BISECT_COVERAGE` environment variable to enable coverage analysis:

        # For tests
        BISECT_COVERAGE=YES ocaml setup.ml -build && test.native

        # For release
        ocaml setup.ml -build

As in the Ocamlbuild instructions, if you don't want to make Bisect_ppx a build
dependency, you can work the [contents][plugin-code] of `Bisect_ppx_plugin`
directly into `myocamlbuild.ml`. Use them to replace the call to
`Bisect_ppx_plugin.dispatch`. In that case, you don't want to put the package
fields in the first step into your `_oasis` file.



<br>

<a id="Excluding"></a>
## Excluding code from coverage

The easiest way to exclude a file from coverage is simply not to build it with
`-package bisect_ppx`, or not to tag it with `coverage`. However, sometimes you
need finer control. There are several ways to disable coverage analysis for
portions of code.

<a id="ExcludingLines"></a>
#### Individual lines and line ranges

If a comment `(*BISECT-IGNORE*)` is found on a line, that line is excluded from
coverage analysis. If `(*BISECT-VISIT*)` is found, all points on that line are
unconditionally marked as visited.

Note that both comments affect the entire line they are found on. For example,
if you have an `if`-`then`-`else` on one line, the comments will affect the
overall expression and both branches.

If there is a range of lines delimited by `(*BISECT-IGNORE-BEGIN*)` and
`(*BISECT-IGNORE-END*)`, all the lines in the range, including the ones with the
comments, are excluded.

<a id="ExcludingValues"></a>
#### Files and top-level values

You can pass the `-exclude-file` option to the Bisect_ppx preprocessor:

```
ocamlfind c \
  -package bisect_ppx -ppxopt "bisect_ppx,-exclude-file .exclude" -c my_code.ml
```

Here is what the `.exclude` file can look like:

```
(* OCaml-style comments are okay. *)

(* Exclude the file "foo.ml": *)
file "foo.ml"

(* Exclude all files whose names start with "test_": *)
file regexp "test_.*"

(* Exclude the top-level values "foo" and "bar" in "baz.ml": *)
file "baz.ml" [
  name "foo"
  name "bar"
]

(* Exclude all top-level values whose names begin with "dbg_" in all
   files in "src/": *)
file regexp "src/.*" [ regexp "dbg_.*" ]
```

All regular expressions are in the syntax of the [`Str`][Str] module.



<br>

<a id="EnvironmentVariables"></a>
## Environment variables

A program instrumented by Bisect_ppx writes `.out` files, which contain the
numbers of times various points in the program's code were visited during
execution. Two environment variables are available to control the writing of
these files.

<a id="OutFiles"></a>
#### Naming the output files

By default, the counts files are called  `bisect0001.out`, `bisect0002.out`,
etc. The prefix `bisect` can be changed by setting the environment variable
`BISECT_FILE`. In particular, you can set it to something like
`_coverage/bisect` to put the counts files in a different directory, in this
example `_coverage/`.

`BISECT_FILE` can also be used to control the prefix programmatically. For
example, the following code bases the prefix on the program name, and puts the
`.out` files into the system temporary directory:

    let () =
      let (//) = Filename.concat in
      let tmpdir = Filename.get_temp_dir_name () in
      Unix.putenv "BISECT_FILE"
        (tmpdir // Printf.sprintf "bisect-%s-" Sys.executable_name)

<a id="Logging"></a>
#### Logging

If the instrumented program fails to write an `.out` file, it will log a
message. By default, these messages go to a file `bisect.log`. `BISECT_SILENT`
can be set to `YES` to turn off logging completely. Alternatively, it can be set
to another filename, or to `ERR` in order to log to `STDERR`.



<br>

<a id="FileFormats"></a>
## File Formats

<a id="OutputFileFormat"></a>
#### Output files

After running your test binary, the binary will generate one or more files with
the extension `.out`, in addition to testing the code. Bisect's
`bisect-ppx-report` command can be used to generate a code coverage report from
these files. This section describes the format of these `.out` files.

Each `.out` file is a magic identifier(see
[common module code][common-code] for more information) followed by a
`Marshal`ed `(string * int array) array`, an array of pairs of:

- source filename, and
- array of point visitation counts.

In addition, each file contains a version number which is a `Marshal`ed pair
`(1,0)`.

If you want to read these files, depending on your environment, the current best
way might be to link with package `bisect_ppx.runtime` and call
`Bisect.Common.read_runtime_data`.

<br>

<a id="WithoutOPAM"></a>
## Installing without OPAM

If you are not using OPAM, clone or extract Bisect_ppx to a directory, then run
`make build install`. Usage should be unaffected, with the exception that
instead of running `bisect-ppx-report`, you will have to run

```
ocamlfind bisect_ppx/bisect-ppx-report
```

unless you add the `bisect_ppx` package directory to your `PATH`, or symlink the
`bisect-ppx-report` binary from a directory in your `PATH`.



[Str]:               http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html#VALregexp
[Bisect_ppx_plugin]: https://github.com/aantron/bisect_ppx/blob/master/src/ocamlbuild/bisect_ppx_plugin.mli
[plugin-code]:       https://github.com/aantron/bisect_ppx/blob/master/src/ocamlbuild/bisect_ppx_plugin.ml
[unlicense]:         http://unlicense.org/
[common-code]:       https://github.com/aantron/bisect_ppx/blob/1.1.0/src/library/common.ml
