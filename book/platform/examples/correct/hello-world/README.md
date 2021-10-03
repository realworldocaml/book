# Hello

This project serves as a tutorial for your first OCaml project.

It will guide you through the typical project structure and OCaml development workflow.
Although the organisation and tools used by the community vary, the ones used here are fairly standard and are the recommended way to work with OCaml with the [OCaml Platform](https://ocaml.org/platform/).

If you are already familiar with the setup and are looking to start a new project, you should probably use one of Spin's official template (list them with `spin ls`).

## Setup your development environment

You need Opam, you can install it by following [Opam's documentation](https://opam.ocaml.org/doc/Install.html).

With Opam installed, you can install the dependencies in a new local switch with:

```bash
make switch
```

Or globally, with:

```bash
make deps
```

Both of these commands will install the tools you typically need for you IDE (e.g. `ocaml-lsp-server`, `ocamlformat`).
Once the installation is complete, you can open the project in your IDE with the `vscode-ocaml-platform` extension.

Finally, build the project with:

```bash
make build
```

### Running Binary

After building the project, you can run the main binary that is produced.

```bash
make start
```

### Running Tests

You can run the test compiled executable:

```bash
make test
```

### Building documentation

Documentation for the libraries in the project can be generated with:

```bash
make doc
make servedoc
```

### Project Structure

The following snippet describes the project structure.

```text
.
├── bin/
|   Source for the executable. This links to the library defined in `lib/`.
│
├── lib/
|   Source for library of the project. 
│
├── test/
|   Unit tests and integration tests.
│
├── dune-project
|   Dune file used to mark the root of the project and define project-wide parameters.
|   For the documentation of the syntax, see https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
│
├── LICENSE
│
├── Makefile
|   Make file containing common development command.
│
├── README.md
│
└── hello.opam
    Opam package definition.
    To know more about creating and publishing opam packages, see https://opam.ocaml.org/doc/Packaging.html.
```
