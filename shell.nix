{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    pkg-config
    wayland-scanner
    (haskellPackages.ghcWithPackages (p: [
      p.cabal-install
      p.bimap
      p.filepath
      p.directory
      p.aeson
    ]))
  ];
  buildInputs = with pkgs; [
    wayland
  ];
}
