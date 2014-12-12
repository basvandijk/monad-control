let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./monad-control.nix {}
