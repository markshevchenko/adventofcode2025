{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs.buildPackages; [
    ocaml
    ocamlPackages.ocaml-lsp
    ocamlPackages.ocamlformat
    chicken
  ];
}
