{ cabal, dataDefault, hspec, network, stm, text, transformers }:

cabal.mkDerivation (self: {
  pname = "encapsulated-resources";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ dataDefault network stm text transformers ];
  testDepends = [ hspec stm text transformers ];
  meta = {
    homepage = "https://github.com/plcplc/encapsulated-resources";
    description = "Encapsulate system resources as processes";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
