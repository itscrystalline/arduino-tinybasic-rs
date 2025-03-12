{ pkgs, lib, config, inputs, ... }:

{
  cachix.enable = false;
  
  # https://devenv.sh/packages/
  packages = with pkgs; [ 
    avrdude
    pkgsCross.avr.stdenv.cc
    pkgsCross.avr.stdenv.cc.libc
    ravedude
    cargo-generate
    minicom
  ];

  # https://devenv.sh/languages/
  languages.rust = {
    enable = true;
    channel = "nightly";
    components = [ "rustc" "cargo" "clippy" "rustfmt" "rust-analyzer" "rust-src" ];
  };

  scripts.serial.exec = ''
    minicom -D /dev/ttyUSB0 -b 57600 -w
  '';

  env.NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM = 1;
}
