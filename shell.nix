{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell rec {
  nativeBuildInputs = with pkgs; [
    pkg-config
    (haskellPackages.ghcWithPackages (p: [
      p.cabal-install
      p.bimap
      p.filepath
      p.directory
      p.aeson
    ]))
  ];
  buildInputs = with pkgs; [
    libxkbcommon
  ];
  shellHook = ''
    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${toString (pkgs.lib.makeLibraryPath buildInputs)}";
  '';
}
