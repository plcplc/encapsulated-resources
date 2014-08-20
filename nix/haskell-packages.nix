# This nix-expression defines our local overrides to the nixpkgs haskellPackages.
{ pkgs } : self : super : with self;
  let cabalNix = src : cabalFile : super.callPackage (pkgs.nixGenCabal src cabalFile).outPath {};
  in {
    encapsulatedResources = cabalNix ./.. "encapsulated-resources.cabal";
  }
