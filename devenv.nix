{ pkgs, lib, config, inputs, ... }:

{
  cachix.enable = false;
  
  # https://devenv.sh/packages/
  packages = with pkgs; [ 
    avrdude
    arduino-cli
    arduino-ide
    pkgsCross.avr.stdenv.cc
    pkgsCross.avr.stdenv.cc.libc
    ravedude
    cargo-generate
  ];

  # https://devenv.sh/languages/
  languages.rust = {
    enable = true;
    channel = "stable";
    components = [ "rustc" "cargo" "clippy" "rustfmt" "rust-analyzer" "rust-src" ];
  };

  scripts.hello.exec = ''
    echo hello from $GREET
  '';

  env.NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM = 1;
}
