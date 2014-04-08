let
  pkgs = (import <nixpkgs>) {};
  hsScope = pkgs.newScope pkgs.haskellPackages;
  encapsulatedResources = import ./encapsulated-resources.nix;
  cabalDeriv = hsScope encapsulatedResources {};
in
  # build encapsulatedresources with tests enabled
  pkgs.lib.overrideDerivation cabalDeriv
    ({extraConfigureFlags, ...} :
      {extraConfigureFlags = ["--enable-tests"];})
