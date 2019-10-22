[![Build Status](https://travis-ci.org/realworldocaml/mdx.svg?branch=master)](https://travis-ci.org/realworldocaml/mdx)

## mdx -- executable code blocks inside markdown files

`mdx` allows to execute code blocks inside markdown files.
There are (currently) two sub-commands, corresponding
to two modes of operations: pre-processing (`ocaml-mdx pp`)
and tests (`ocaml-mdx test`).

The pre-processor mode allows to mix documentation and code,
and to practice "literate programming" using markdown and OCaml.

The test mode allows to ensure that shell scripts and OCaml fragments
in the documentation always stays up-to-date.

`mdx` is released as a single binary (called `ocaml-mdx`) and
can be installed using opam:

```sh
$ opam install mdx
```

### Supported Extensions

#### Shell Scripts

`ocaml-mdx` interprets shell scripts inside `sh` code blocks as cram-like tests. The
syntax is the following:

- Lines beginning with a dollar sign and a space are
  *commands* and will be run in the shell.
- Multi-lines commands end by `\` and continue with two spaces and
  a `>` sign on the next line:

       ```sh
       $ <line1> \
       > <line2> \
       > <line3>
       ```
- Commands support the heredoc syntax (`<<`):

       ```sh
       $ cat <<EOF \
       > hello\
       > world\
       > EOF
       hello
       world
       ```
- Lines beginning without a dollar sign are considered command *outputs*.
- Command outputs can contain *ellipses*: `...`. These will
  match any possible outputs (on zero, one or multiple lines).
- Arbitrary padding with whitespace is supported, as long as it is consistent
  inside a code block.

Here is an example of a markdown file using shell scripts inside code blocks,
with a padding of 3:

    ```sh
       $ for i in `seq 1 10`
       1
       ...
       10
    ```

`ocaml-mdx` will also consider exit codes when the syntax `[<exit code>]`is used:

    ```sh
    $ exit 1
    [1]
    ```

Note that nothing will be displayed when the exit code is 0 (e.g. in case
of success).

#### OCaml Code

`ocaml-mdx` interprets OCaml fragments. It understands _normal_ code fragments and
_toplevel_ code fragments (starting with a `#` sign and optionally ending with
`;;`). Arbitrary whitespace padding is supported, at long as it stays
consistent within a code block.

Toplevel fragments interleave OCaml code and their corresponding outputs.

Here is an example of normal OCaml code:

    ```ocaml
    print_endline "42"
    ```

Here is an examples of toplevel OCaml code:

    ```ocaml
    # print_endline "42"
    42
    ```

### Pre-processing

`ocaml-mdx pp` allows to transform a markdown file into a valid
OCaml file, which can be passed to OCaml using the `-pp`
option.

For instance, given the following `file.md` document:

    ```ocaml
    # print_endline "42"
    42
    ```

Can be compiled and executed using:

```sh
$ ocamlc -pp 'ocaml-mdx pp' -impl file.md -o file.exe
$ ./file.exe
42
```

This can be automated using `dune`:

```
(rule
 ((targets (file.ml))
  (deps    (file.md))
  (action  (with-stdout-to ${@} (run ocaml-mdx pp ${<})))))

(executable ((name file)))
```

### Tests

#### Cram Tests

Cram tests can be executed and checked with `ocaml-mdx test <file.md>`.

    ```sh
     $ for i in `seq 1 10`; do echo $i; done
     1
     ...
     10
     ```

If the output is not consistent with what is expected,
`<file.md>.corrected` is generated.

#### OCaml

To execute OCaml code and toplevel fragments, uses `ocaml-mdx test <file.md>`.

    ```ocaml
    # print_endline "42"
    42
    ```

If the output is not consistent with what is expected
`<file.md>.corrected` is generated.

#### Integration with Dune

To test that the code blocks of `file.md` stay consistent, one can use
dune's `diff?` stanza:

```
(alias
  (name runtest)
  (deps (:test file.md))
  (action (progn
           (run ocaml-mdx test %{test})
           (diff? %{test} %{test}.corrected))))
```

This allows to test the consistency of a markdown file using the normal dev
workflow:

```
$ dune runtest
```

will display a diff of the output if something has changed. For instance:

```
$ dune runtest
------ file.md
++++++ file.md.corrected
File "file.md", line 23, characters 0-1:
 |
 |```sh
-| $ for i in `seq 1 3`; do echo $i; done
+| $ for i in `seq 1 4`; do echo $i; done
 | 1
 | 2
 | 3
+| 4
 |```
```

And the changes can then be accepted using:

```
$ dune promote
```

#### Non-deterministic Tests

**Non-deterministic Outputs**

`ocaml-mdx test` supports non-deterministic outputs:

    ```sh non-deterministic=output
    $ <command>
    <output>
    ```

In that case, `ppx test <file>` will run the command but will not
generate `<file>.corrected` if the new output differs from the one
described in the file. Use `ocaml-mdx test --non-deterministic <file>` to come
back to the default behaviour.

**Non-deterministic Commands**

`ocaml-mdx test` supports non-deterministic commands:

    ```ocaml non-deterministic=command
    # Random.int 10;;
    - : int = 5
    ```

In that case, `ocaml-mdx test <file>` will *not* run the command. Use `ocaml-mdx test
--non-deterministic <file>` to come back to the default behaviour.

#### Named execution environments (since mdx 1.1.0)

Separate environments can be defined for blocks:

`x` holds the value `1` in the environment `e1`.

    ```ocaml env=e1
    let x = 1;;
    ```

    ```ocaml env=e1
    module M = struct let k = 42 let f x = x * k end;;
    ```

`x` holds the value `3` in the environment `e2`.

    ```ocaml env=e2
    let x = 3;;
    ```

We can retrieve the value of `x` in environment `e1`:

    ```ocaml env=e1
    # print_int x;;
    1
    - : unit = ()
    # print_int M.k;;
    42
    - : unit = ()
    # M.f;;
    - : int -> int = <fun>
    ```

#### Matching on the OCaml version (since mdx 1.2.0)

Blocks can be processed or ignored depending on the current version of OCaml.

For example to have a different outcome whether we are past OCaml 4.06:

    ```ocaml version<4.06
    # let f x = x + 1
    val f : int -> int = <fun>
    # let f y =
      y^"foo"
    val f : bytes -> bytes = <fun>
    ```

    ```ocaml version>=4.06
    # let f x = x + 1
    val f : int -> int = <fun>
    # let f y =
      y^"foo"
      val f : string -> string = <fun>
      ```

The available operators are `<>`, `>=`, `>`, `<=`, `<` and `=`.
The version number can be of the following forms:
- `*`
- `X`
- `X.Y`
- `X.Y.Z`

#### Environment variables declaration

Environment variables can be declared at the beginning of a block:

    ```ocaml set-FOO=bar,set-BAR=foo
    # print_endline (Sys.getenv "FOO")
    bar
    - : unit = ()
    # print_endline (Sys.getenv "BAR")
    foo
    - : unit = ()
    ```

Those variables are then available in the subsequent blocks

    ```ocaml
    # print_endline (Sys.getenv "FOO")
    bar
    - : unit = ()
    ```

### Sections

It is possible to test or execute only a subset of the file using
sections using the `--section` option (short name is `-s`). For
instance `ocaml-mdx pp -s foo` will only consider the section matching the
perl regular expression `foo`.

### Dune rules (since mdx 1.1.0)

`ocaml-mdx` can generate `dune` rules to synchronize .md files with .ml files.

Consider the test/dune_rules.md file that contains blocks referring to files
dune_rules_1.ml and dune_rules_2.ml, running:

```
$ ocaml-mdx rule test/dune_rules.md
```

generates the following `dune` rules on the standard output:
```
(alias
 (name   runtest)
 (deps   (:x test/dune_rules.md)
         (:y1 dune_rules_1.ml)
         (:y0 dune_rules_2.ml)
         (source_tree foo))
 (action (progn
           (run ocaml-mdx test --direction=infer-timestamp %{x})
           (diff? %{x} %{x}.corrected)
           (diff? %{y1} %{y1}.corrected)
           (diff? %{y0} %{y0}.corrected))))
```
