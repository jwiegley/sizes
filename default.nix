{ cabal, cmdargs, deepseq, dlist, lens, parallelIo, regexPosix
, systemFileio, systemFilepath, text
}:

cabal.mkDerivation (self: {
  pname = "sizes";
  version = "2.3.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    cmdargs deepseq dlist lens parallelIo regexPosix systemFileio
    systemFilepath text
  ];
  meta = {
    homepage = "https://github.com/jwiegley/sizes";
    description = "Recursively show space (size and i-nodes) used in subdirectories";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})