let
  pkgs = (import <nixpkgs> { });
  local = (import ./default.nix { });
  strings = pkgs.lib.strings;
  inherit (pkgs) stdenv lib;
in with local;

pkgs.mkShell {
  inputsFrom = [ re ];
  buildInputs = (with pkgs; [
    gnumake
    ocamlPackages.ocaml-lsp
  ]) ++ (with opam; [
    # test
    ounit
  ]);
}
