# Bisect_ppx &nbsp; [![Travis status][travis-img]][travis] [![AppVeyor status][appveyor-img]][appveyor] [![Coverage][coveralls-img]][coveralls]

[Bisect_ppx][self] is a code coverage tool for OCaml and Reason. It helps you
test thoroughly by showing what's **not** tested.

[![Bisect_ppx usage example][sample]][gh-pages-report]

You can browse the report seen above [online here][gh-pages-report].

[self]: https://github.com/aantron/bisect_ppx
[travis]: https://travis-ci.org/aantron/bisect_ppx
[travis-img]: https://img.shields.io/travis/aantron/bisect_ppx/master.svg?label=travis
[appveyor]: https://ci.appveyor.com/project/aantron/bisect-ppx/branch/master
[appveyor-img]: https://img.shields.io/appveyor/ci/aantron/bisect-ppx/master.svg?label=appveyor
[sample]: https://raw.githubusercontent.com/aantron/bisect_ppx/master/doc/sample.gif
[coveralls]: https://coveralls.io/github/aantron/bisect_ppx?branch=master
[coveralls-img]: https://img.shields.io/coveralls/aantron/bisect_ppx/master.svg



<br>

#### Table of contents

- [**Usage**](#Usage)
  - [**Dune**](#Dune) &nbsp; ([starter repo][dune-repo], [report][dune-report])
  - [**esy**](#esy) &nbsp; ([starter repo][esy-repo], [report][esy-report])
  - [**ReScript**](#ReScript) &nbsp; ([starter repo][rescript-repo], [report][rescript-report])
  - [**Js_of_ocaml**](#Js_of_ocaml) &nbsp; ([starter repo][jsoo-repo], [report][jsoo-report])
  - [**Ocamlfind, Ocamlbuild, and OASIS**](#Ocamlbuild)
- [**Sending to Coveralls**](#Coveralls)
- [**Controlling coverage with `[@coverage off]`**](#Exclusion)
- [**Other topics**](#Other)
- [**Bisect_ppx users**](#Users)
- [**Contributing**](#Contributing)



<br>

<a id="Usage"></a>
## Usage

<a id="Dune"></a>
### Dune

Refer to [**aantron/bisect-starter-dune**][dune-repo], which produces
[this report][dune-report].

1. [Depend on Bisect_ppx](https://github.com/aantron/bisect-starter-dune/blob/03cb827553d1264559eab19fdaa8c0056c9b2019/bisect-starter-dune.opam#L10-L11)
   in your `opam` file:

    ```
    depends: [
      "bisect_ppx" {dev & >= "2.5.0"}
      "dune" {>= "2.7.0"}
    ]
    ```

2. [Mark the code under test for preprocessing by
   `bisect_ppx`](https://github.com/aantron/bisect-starter-dune/blob/03cb827553d1264559eab19fdaa8c0056c9b2019/dune#L4) in your `dune` file:

    ```ocaml
    (library
     (public_name my_lib)
     (instrumentation (backend bisect_ppx)))
    ```

3. Build and run your test binary. In addition to testing your code, when
   exiting, it will write one or more files with names like
   `bisect0123456789.coverage`:

    ```
    find . -name '*.coverage' | xargs rm -f
    dune runtest --instrument-with bisect_ppx --force
    ```

    The `--force` flag forces all your tests to run, which is needed for an
    accurate coverage report.

    To run tests without coverage, do

    ```
    dune runtest
    ```

4. Generate the [coverage report][dune-report] in `_coverage/index.html`:

    ```
    bisect-ppx-report html
    ```

    You can also generate a short summary in the terminal:

    ```
    bisect-ppx-report summary
    ```

[dune-repo]: https://github.com/aantron/bisect-starter-dune#readme
[dune-report]: https://aantron.github.io/bisect-starter-dune/



<br>

<a id="esy"></a>
### esy

Refer to [**aantron/bisect-starter-esy**][esy-repo], which produces [this
report][esy-report].

The instructions are the same as for regular [Dune](#Dune) usage, but...

1. [Depend on Bisect_ppx in `package.json`](https://github.com/aantron/bisect-starter-esy/blob/fc9707a641ec598b6849087841d63fa140bd7118/package.json#L8),
instead of in an `opam` file:

    ```json
    "devDependencies": {
      "@opam/bisect_ppx": "^2.5.0"
    },
    "dependencies": {
      "@opam/dune": "^2.7.0"
    }
    ```

2. Use the `esy` command for the build and for running binaries:

    ```
    esy install
    esy dune runtest --instrument-with bisect_ppx --force
    esy bisect-ppx-report html
    ```

[esy-repo]: https://github.com/aantron/bisect-starter-esy
[esy-report]: https://aantron.github.io/bisect-starter-esy/



<br>

<a id="BuckleScript"></a>
<a id="ReScript"></a>
### ReScript

Refer to [**aantron/bisect-starter-bsb**][rescript-repo], which produces
[this report][rescript-report].

1. [Depend on Bisect_ppx in `package.json`](https://github.com/aantron/bisect-starter-rescript/blob/master/package.json#L3-L6),
and install it:

    ```json
    "devDependencies": {
      "bisect_ppx": "^2.0.0"
    },
    "dependencies": {
      "bs-platform": "*"
    }
    ```

    ```
    npm install
    ```

    If you are using Yarn, you need to run an extra command because of
    [yarnpkg/pkg#3421](https://github.com/yarnpkg/yarn/issues/3421):

    ```
    yarn add bisect_ppx
    yarn --check-files
    ```

    If pre-built binaries aren't available for your system, the build will
    automatically fall back to building Bisect_ppx from source using
    [esy](https://esy.sh), which will take a few minutes the first time. If this
    happens, you may need to install esy, if it is not already installed:

    ```
    npm install -g esy
    npm install
    ```

2. [Add Bisect_ppx to your `bsconfig.json`](https://github.com/aantron/bisect-starter-rescript/blob/master/bsconfig.json#L3-L8):

    ```json
    "bs-dependencies": [
      "bisect_ppx"
    ],
    "ppx-flags": [
      "bisect_ppx/ppx"
    ]
    ```

3. If you are using Jest, add this to your `package.json`:

    ```json
    "jest": {
      "setupFilesAfterEnv": [
        "bisect_ppx/lib/js/src/runtime/bucklescript/jest.js"
      ]
    }
    ```

    Or, if you have enabled the `package-specs.in-source` flag in
    `bsconfig.json`, replace the path by

    ```json
    "bisect_ppx/src/runtime/bucklescript/jest.js"
    ```

    You can exclude your test cases from the coverage report by adding this to
    `bsconfig.json`:

    ```json
    "ppx-flags": [
      ["bisect_ppx/ppx", "--exclude-files", ".*_test\\.res$$"]
    ]
    ```

    Usage with Jest requires Bisect_ppx version 2.4.0 or higher. See the
    [**aantron/bisect-starter-jest**][jest-repo] for a complete minimal example
    project. The repo produces [this report][jest-report].

    If the tests will be running in the browser, at the end of testing, call

    ```reason
    Bisect.Runtime.get_coverage_data();
    ```

    This returns binary coverage data in a `string option`, which you should
    upload or otherwise get out of the browser, and write into a `.coverage`
    file.

4. Build in development with `BISECT_ENABLE=yes`, run tests, and generate the
[coverage report][rescript-report] in `_coverage/index.html`:

    ```
    BISECT_ENABLE=yes npm run build
    npm run test
    npx bisect-ppx-report.exe html
    ```

    To exclude your test files from the report, change your PPX flags like so:

    ```json
    "ppx-flags": [
      ["bisect_ppx/ppx", "--exclude-files", ".*test\\.re"]
    ]
    ```

    The last argument is a regular expression in the syntax of OCaml's [`Str`
    module](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html). Note
    that backslashes need to be escaped both inside the regular expression, and
    again because they are inside a JSON string.

    Multiple `--exclude-files` option can be specified if you want to provide
    multiple patterns.

5. If your project uses both ReScript and native Dune, native Dune will start
   picking up OCaml files that are part of the ReScript `bisect_ppx` package.
   To prevent this, add a `dune` with the following contents to the root of
   your project:

   ```
   (data_only_dirs node_modules)
   ```

[rescript-repo]: https://github.com/aantron/bisect-starter-rescript#readme
[rescript-report]: https://aantron.github.io/bisect-starter-rescript/
[jest-repo]: https://github.com/aantron/bisect-starter-jest#readme
[jest-report]: https://aantron.github.io/bisect-starter-jest/



<br>

<a id="Js_of_ocaml"></a>
### Js_of_ocaml

Refer to [**aantron/bisect-starter-jsoo**][jsoo-repo], which produces
[this report][jsoo-report].

1. Follow the [Dune instructions](#Dune) above, except that [the final test
script must be linked with `bisect_ppx.runtime`](https://github.com/aantron/bisect-starter-jsoo/blob/dcb2688017c9f322a992bbacc24f6d86ce4c2dc6/dune#L10)
(but not instrumented):

    ```scheme
    (executable
     (name my_tester)
     (modes js)
     (libraries bisect_ppx.runtime))
    ```

2. If the tests will run on Node, [call this function](https://github.com/aantron/bisect-starter-jsoo/blob/dcb2688017c9f322a992bbacc24f6d86ce4c2dc6/tester.ml#L3)
at the end of testing to write `bisect0123456789.coverage`:

    ```ocaml
    Bisect.Runtime.write_coverage_data ()
    ```

    If the tests will run in the browser, call

    ```ocaml
    Bisect.Runtime.get_coverage_data ()
    ```

    to get binary coverage data in a string option. Upload this string or
    otherwise extract it from the browser to create a `.coverage` file.

3. Build the usual Js_of_ocaml target, including the instrumented code under
test, then run the reporter to generate the [coverage report][jsoo-report] in
`_coverage/index.html`:

    ```
    dune build my_tester.bc.js --instrument-with bisect_ppx
    node _build/default/my_tester.bc.js   # or in the browser
    bisect-ppx-report html
    ```

[jsoo-repo]: https://github.com/aantron/bisect-starter-jsoo#readme
[jsoo-report]: https://aantron.github.io/bisect-starter-jsoo/



<br>

<a id="Ocamlbuild"></a>
### Ocamlfind, Ocamlbuild, and OASIS

- [Ocamlbuild](https://github.com/aantron/bisect_ppx-ocamlbuild#using-with-ocamlbuild)
and [OASIS](https://github.com/aantron/bisect_ppx-ocamlbuild#using-with-oasis)
instructions can be found at
[aantron/bisect_ppx-ocamlbuild](https://github.com/aantron/bisect_ppx-ocamlbuild#readme).

- With Ocamlfind, you must have your build script issue the right commands, to
instrument the code under test, but not the tester:

    ```
    ocamlfind opt -package bisect_ppx -c src/source.ml
    ocamlfind opt -c test/test.ml
    ocamlfind opt -linkpkg -package bisect_ppx src/source.cmx test/test.cmx
    ```

    Running the tester will then produce `bisect0123456789.coverage` files,
    which you can process with `bisect-ppx-report`.



<br>

<a id="Coveralls"></a>
## Sending to [Coveralls](https://coveralls.io)

`bisect-ppx-report` can send reports to **Coveralls** and **Codecov** directly
from **Travis**, **CircleCI**, and **GitHub Actions**. To do this, run

```
bisect-ppx-report send-to Coveralls
```

or

```
bisect-ppx-report send-to Codecov
```

When sending specifically from GitHub Actions to Coveralls, use

```
- run: bisect-ppx-report send-to Coveralls
  env:
    COVERALLS_REPO_TOKEN: ${{ secrets.GITHUB_TOKEN }}
    PULL_REQUEST_NUMBER: ${{ github.event.number }}
```

Put these commands in your CI script in the same place you would run
`bisect-ppx-report html` locally. See
[bisect-ci-integration-megatest](https://github.com/aantron/bisect-ci-integration-megatest#readme)
for example CI scripts and current status of these integrations.

If you'd like Bisect_ppx to support other CI and/or coverage services, please
send a pull request!

As a workaround for missing CI/coverage integrations, and for development,
`bisect-ppx-report` can also generate a JSON report in Coveralls format, which
can be uploaded to a service of your choice using a separate command. For
example, to send manually from Travis to Coveralls:

```
bisect-ppx-report \
  coveralls coverage.json \
  --service-name travis-ci \
  --service-job-id $TRAVIS_JOB_ID
curl -L -F json_file=@./coverage.json https://coveralls.io/api/v1/jobs
```

For other CI services, replace `--service-name` and `--service-job-id` as
follows:

| CI service | `--service-name` | `--service-job-id` |
| ---------- | ---------------- | -------------------- |
| Travis | `travis-ci` | `$TRAVIS_JOB_ID` |
| CircleCI | `circleci` | `$CIRCLE_BUILD_NUM` |
| Semaphore | `semaphore` | `$REVISION` |
| Jenkins | `jenkins` | `$BUILD_ID` |
| Codeship | `codeship` | `$CI_BUILD_NUMBER` |
| GitHub Actions | `github` | `$GITHUB_RUN_NUMBER` |

Note that Coveralls-style reports are less precise than the HTML reports
generated by Bisect_ppx, because Coveralls considers entire lines as visited or
not visited. There can be many expressions on a single line, and the HTML
report separately considers each expression as visited or not visited.



<br>

<a id="Exclusion"></a>
## Controlling coverage with `[@coverage off]`

You can tag expressions with `[@coverage off]`, and neither they, nor their
subexpressions, will be instrumented by Bisect_ppx.

Likewise, you can tag module-level `let`-declarations with `[@@coverage off]`,
and they won't be instrumented.

You can also turn off instrumentation for blocks of declarations inside a
module with `[@@@coverage off]` and `[@@@coverage on]`.

Finally, you can exclude an entire file by putting `[@@@coverage exclude_file]`
into its top-level module. However, whenever possible, it is recommended to
exclude files by not preprocessing with Bisect_ppx to begin with.



<br>

<a id="Other"></a>
## Other topics

See [advanced usage][advanced] for:

- Exhaustiveness checking.
- Excluding generated files from coverage.
- Environment variables.

[advanced]: https://github.com/aantron/bisect_ppx/blob/master/doc/advanced.md#readme



<br>

<a id="Users"></a>
## Bisect_ppx users

A small sample of projects using Bisect_ppx:

<!-- Sort OCaml and Reason first if Bisect_ppx usage is merged. -->

- Core tools
  - [Lwt][lwt] &nbsp; ([report](https://coveralls.io/github/ocsigen/lwt))
  - [Odoc][odoc]
  - [ocamlformat][ocamlformat]
  - [OCaml][ocaml]
  - [Reason][reason]
  - [ctypes][ctypes]

- Libraries
  - [Markup.ml][markupml] &nbsp; ([report][markupml-coveralls])
  - [Lambda Soup][soup] &nbsp; ([report](https://coveralls.io/github/aantron/lambdasoup))
  - [Trie](https://github.com/brendanlong/ocaml-trie) &nbsp; ([report](https://coveralls.io/github/brendanlong/ocaml-trie?branch=master))
  - [ocaml-ooxml](https://github.com/brendanlong/ocaml-ooxml) &nbsp; ([report](https://coveralls.io/github/brendanlong/ocaml-ooxml?branch=master))
  - [routes](https://github.com/anuragsoni/routes) &nbsp; ([report](https://codecov.io/gh/anuragsoni/routes))

- Applications

  - [XAPI](https://xenproject.org/developers/teams/xen-api/) &nbsp; ([1](https://coveralls.io/github/xapi-project/xen-api?branch=master), [2](https://coveralls.io/github/xapi-project/nbd), [3](https://coveralls.io/github/xapi-project/xcp-idl), [4](https://coveralls.io/github/xapi-project/rrd-transport?branch=master), [5](https://github.com/xapi-project/xenopsd))
  - [Scilla](https://github.com/Zilliqa/scilla#readme) &nbsp; ([report](https://coveralls.io/github/Zilliqa/scilla?branch=master))
  - [Coda](https://github.com/CodaProtocol/coda)
  - [snarky](https://github.com/o1-labs/snarky)
  - [comby](https://github.com/comby-tools/comby) &nbsp; ([report](https://coveralls.io/github/comby-tools/comby?branch=master))
  - [ocaml-irc-client][ocaml-irc-client] &nbsp; ([report][irc-coveralls])

[lwt]: https://github.com/ocsigen/lwt
[odoc]: https://github.com/ocaml/odoc
[ocaml]: https://github.com/ocaml/ocaml/pull/8874
[reason]: https://github.com/facebook/reason/pull/1794#issuecomment-361440670
[ocamlformat]: https://github.com/ocaml-ppx/ocamlformat
[ctypes]: https://github.com/ocamllabs/ocaml-ctypes
[ocaml-irc-client]: https://github.com/johnelse/ocaml-irc-client#readme
[irc-coveralls]: https://coveralls.io/github/johnelse/ocaml-irc-client
[markupml]: https://github.com/aantron/markup.ml#readme
[markupml-coveralls]: https://coveralls.io/github/aantron/markup.ml
[soup]: https://github.com/aantron/lambdasoup#readme
[gh-pages-report]: http://aantron.github.io/bisect_ppx/demo/



<br>

<a id="Contributing"></a>
## Contributing

Bug reports and pull requests are warmly welcome. Bisect_ppx is developed on
GitHub, so please [open an issue][issues].

Bisect_ppx is developed mainly using opam. To get the latest development
version, run

```
opam source --dev-repo --pin bisect_ppx
```

You will now have a `bisect_ppx` subdirectory to work in. Try these `Makefile`
targets:

- `make test` for unit tests.
- `make usage` for build system integration tests, except ReScript.
- `make -C test/bucklescript full-test` for ReScript. This requires npm and esy.

[issues]: https://github.com/aantron/bisect_ppx/issues
