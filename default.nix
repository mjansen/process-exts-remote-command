# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, filepath, processExts }:

cabal.mkDerivation (self: {
  pname = "process-exts-remote-command";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ filepath processExts ];
  meta = {
    description = "run a remote command via ssh";
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
