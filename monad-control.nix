{ cabal, transformers, transformersBase }:

cabal.mkDerivation (self: {
  pname = "monad-control";
  version = "HEAD";
  src = ./.;
  buildDepends = [ transformers transformersBase ];
  meta = {
    homepage = "https://github.com/basvandijk/monad-control";
    description = "Lift control operations, like exception catching, through monad transformers";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
