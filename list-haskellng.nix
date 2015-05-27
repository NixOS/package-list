let pkgs = import <nixpkgs> {};
in
pkgs.haskellPackages.override {
  overrides = self: super: {    # disable name mangling
    mkDerivation = expr: super.mkDerivation (expr // { isLibrary = false; });
  };
}
