# Bisect_ppx advanced usage

<br>

#### Table of contents

- [Exhaustiveness checking](#Exhaustiveness)
- [Excluding generated files from coverage](#Excluding)
- [Environment variables](#EnvironmentVariables)
  - [Naming the output files](#OutFiles)
  - [Logging](#Logging)
  - [Setting at compile time](#CompileTime)



<br>

<a id="Exhaustiveness"></a>
## Exhaustiveness checking

It is easy to accidentally fail to preprocess part of your project, by leaving
out `(preprocess (pps bisect_ppx))` in one of its subdirectories. If this
happens, and goes unnoticed, you may see misleading coverage statistics, because
Bisect_ppx will simply not be aware of the missing files at all. It will not
report them as either covered or not covered &mdash; they will just not be
present in the report.

To sanity-check this, you can pass the `--expect` option to `bisect-ppx-report`.
For example,

```
bisect-ppx-report html --expect src/
```

`bisect-ppx-report` will then recursively scan `src/` for any `.ml` and `.re`
files, and check that all of them were included in the report.

You may have a subdirectory of `src/` that should not be included. You can
exclude it from the recursive scan with `--do-not-expect`:

```
bisect-ppx-report html --expect src/ --do-not-expect src/build_tool/
```

You can also specify individual files with `--expect` and `--do-not-expect` by
omitting the trailing path separator:

```
bisect-ppx-report html --expect src/ --do-not-expect src/build_tool.ml
```



<br>

<a id="Excluding"></a>
## Excluding generated files from coverage

Whole files can be excluded by placing `[@@@coverage exclude_file]` anywhere in
their top-level module.

If you have generated code that you cannot easily place an attribute into, nor
is it easy to avoid preprocessing it, you can pass the `--exclusions` option to
the Bisect_ppx preprocessor:

```
(preprocess (pps bisect_ppx --exclusions bisect.exclude))
(preprocessor_deps (file bisect.exclude))
```

Note that the paths to `bisect.exclude` might be different between the
`preprocess` and `preprocessor_deps` stanzas, because `pps bisect_ppx` looks for
the file relative to the root directory of your project, while
`preprocessor_deps` looks in the same directory that the `dune` file is in.

Here is what the `bisect.exclude` file can look like:

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

A program instrumented by Bisect_ppx writes `.coverage` files, which contain the
numbers of times various points in the program's code were visited during
execution. Two environment variables are available to control the writing of
these files.

<a id="OutFiles"></a>
#### Naming the output files

By default, the counts files are called  `bisect0001.coverage`,
`bisect0002.coverage`, etc. The prefix `bisect` can be changed by setting the
environment variable `BISECT_FILE`. In particular, you can set it to something
like `_coverage/bisect` to put the counts files in a different directory, in
this example `_coverage/`.

`BISECT_FILE` can also be used to control the prefix programmatically. For
example, the following code bases the prefix on the program name, and puts the
`.coverage` files into the system temporary directory:

    let () =
      let (//) = Filename.concat in
      let tmpdir = Filename.get_temp_dir_name () in
      Unix.putenv "BISECT_FILE"
        (tmpdir // Printf.sprintf "bisect-%s-" Sys.executable_name)

<a id="Logging"></a>
#### Logging

If the instrumented program fails to write an `.coverage` file, it will log a
message. By default, these messages go to a file `bisect.log`. `BISECT_SILENT`
can be set to `YES` to turn off logging completely. Alternatively, it can be set
to another filename, or to `ERR` in order to log to `STDERR`.

<a id="CompileTime"></a>
#### Setting at compile time

If your testing environment doesn't allow you to easily specify these
environment variables at testing runtime, you can specify default values
for them at compile time by passing the `--bisect-file` and `--bisect-silent`
options to the Bisect_ppx preprocessor:

```
(preprocess
 (pps bisect_ppx --
  --bisect-file /tmp/mycoverage/ --bisect-silent /tmp/coverage.log))
```

If different values are specified in different files, one of them is chosen.
Which one is unspecified.



[Str]: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html#VALregexp
