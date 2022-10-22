{ pkgs }: {
    deps = [
      pkgs.ocaml
      pkgs.opam
      pkgs.dune_3
      pkgs.ocamlPackages.utop
      pkgs.ocamlPackages.ocaml-lsp
      pkgs.texlive.combined.scheme-full
      pkgs.libffi
      pkgs.pandoc
      pkgs.python310Packages.pygments
    ];
}
