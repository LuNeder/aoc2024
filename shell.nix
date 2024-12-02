{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.fortran-fpm
    pkgs.gfortran14 # Unfortunately no LLVM Flang on nixpkgs :c
  ];

  shellHook = ''
    zsh
  '';

  NIX_ENFORCE_PURITY = "0";

}
