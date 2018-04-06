with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  #propagatedBuildInputs = nixops.propagatedBuildInputs;
  #buildInputs = [ nix ] ++ nixops.buildInputs;
  buildInputs = [
    (nixops.overrideAttrs (old: rec {
      version = "1.5.2";
      src = fetchFromGitHub {
        owner = "NixOS";
        repo = "nixops";
        rev = "f4ce1f838810ed4214c1a4496bb3d4faf69b0400";
        sha256 = "1sgdcyidd2javcizgh304v43dsa8d66sxxzyf9aaq4jk9mc9n3gf";
      };

      pythonPath = old.pythonPath ++ [ 
        pythonPackages.libvirt
      ];

      postPatch = (old.postPatch or "") + ''
        sed -ie 's/@version@/${version}/' setup.py
        sed -ie 's/@version@/${version}/' doc/manual/manual.xml
        sed -ie 's/@version@/${version}/' scripts/nixops    
      '';
      postInstall = ''
        mkdir -p $out/share/nix/nixops
        cp -av "nix/"* $out/share/nix/nixops
        wrapProgram $out/bin/nixops --prefix PATH : "${openssh}/bin"
      '';
    }))
  ];
}
