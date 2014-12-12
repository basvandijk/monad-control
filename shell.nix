let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      monadControl = self.callPackage ./monad-control.nix {};
    };
  };
in pkgs.myEnvFun {
     name = haskellPackages.monadControl.name;
     buildInputs = [
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
       ] ++ hs.monadControl.propagatedNativeBuildInputs)))
     ];
   }