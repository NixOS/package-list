let pkgs = import <nixpkgs> {};
in
pkgs.haskellngPackages.override {
  overrides = self: super: {    # disable name mangling
    mkDerivation = expr: super.mkDerivation (expr // { isLibrary = false; });
  };
}
