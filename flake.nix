{
  inputs = {
    rsdd.url = "github:stites/rsdd/2a04a70?dir=nix";
    flake-parts.follows = "rsdd/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/release-24.11";
  };

  outputs = inputs @ {...}:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      perSystem = {
        inputs',
        config,
        pkgs,
        ...
      }: let
        rsdd = inputs'.rsdd.packages.rsdd;
        haskellPackages = pkgs.haskellPackages.override {
          overrides = final: prev: {
            rsdd-hs = pkgs.haskellPackages.callCabal2nix "rsdd-hs" ./rsdd-hs {
              inherit rsdd;
            };
            effectful-rsdd = pkgs.haskellPackages.callCabal2nix "effectful-rsdd" ./effectful-rsdd {
              inherit (final) rsdd-hs;
            };
          };
        };
      in {
        packages = {
          inherit rsdd;
          inherit (haskellPackages) rsdd-hs effectful-rsdd;
        };
        devShells.default =
          haskellPackages.shellFor {
            packages = p: with p; [rsdd-hs effectful-rsdd];
            withHoogle = true;
            genericBuilderArgsModifier = args: args // { doCheck = false; };
            extraDependencies = p: {
              libraryHaskellDepends = [ config.packages.rsdd ];
            }; 
            buildInputs = with pkgs; [
              (writeScriptBin "cabal" ''
                ${cabal-install}/bin/cabal --extra-lib-dirs=${config.packages.rsdd}/lib "$@"
              '')
              haskell-language-server
              pkg-config
              ghciwatch
              (writeScriptBin ", docs" ''
                echo http://127.0.0.1:8888
                hoogle serve -p 8888 --local
              '')
              (writeScriptBin ", repl" ''
                ghciwatch --command "cabal repl $*" --test-ghci=Main.main --clear --watch exe --watch lib --watch *.cabal
              '')
            ];
            # inputsFrom = [
            #   config.packages.rsdd-hs.env
            #   config.packages.effectful-rsdd.env
            # ];
          };
        };
    };
}
