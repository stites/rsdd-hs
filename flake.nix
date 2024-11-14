# file: flake.nix
{
  inputs = {
    #rsdd.url = "github:stites/rsdd/4969c0c?dir=nix";
    rsdd.url = "path:/home/stites/git/haskell/active/rsdd/nix";
    flake-parts.follows = "rsdd/flake-parts";
    nixpkgs.follows = "rsdd/nixpkgs";
    treefmt-nix.follows = "rsdd/nci/treefmt";

    haskell-flake.url = "github:srid/haskell-flake";
    # treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";
    #cachix-push.url = "github:juspay/cachix-push";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
      ];
      #flake.haskellFlakeProjectModules.default = { config, ... }: {
      #  # packages = {
      #  #   rsdd-hs.source = config.packages.rsdd;
      #  # };
      #};
      perSystem = { inputs', self', system, lib, config, pkgs, ... }: {
        #cachix-push.cacheName = "stites";
        packages = {
          #default = self'.packages.rsdd-hs;
          rsdd = inputs'.rsdd.packages.rsdd-nocheck;
        };
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc94.extend (
            final: prev: {
              # seems like a dirty hack since this is a rust package...
              #inherit (config.packages) rsdd;
            }
          );
          # NOTE: doesn't seem to work...
          # packages = { rsdd.source = self'.packages.rsdd; };
          #settings.rsdd = {...}: { custom = _: self'.packages.rsdd; };
          #   extraBuildDepends = [ config.packages.rsdd ];
          # };
          devShell = {
            hlsCheck.enable = false;
            tools = hp: {
              treefmt = config.treefmt.build.wrapper;
            } // config.treefmt.build.programs;
          };
          autoWire = [ "packages"
                       "apps"
                       "checks" ]; # Wire all but the devShell
        };

        treefmt.config = {
          projectRootFile = "flake.nix";
          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;
        };
        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
            '';
            category = "Dev Tools";
          };
          repl = {
            description = "Start the cabal repl";
            exec = ''
              cabal repl "$@"
            '';
            category = "Dev Tools";
          };
          fmt = {
            description = "Format the source tree";
            exec = config.treefmt.build.wrapper;
            category = "Dev Tools";
          };
          # run = {
          #   description = "Run the project with ghcid auto-recompile";
          #   exec = ''
          #     ghcid -c "cabal repl lib:rsdd-hs --extra-lib-dirs=${config.packages.rsdd}/lib" --warnings
          #   '';
          #   category = "Primary";
          # };
          # run-example = {
          #   description = "Run the project with ghcid auto-recompile";
          #   exec = ''
          #     ghcid -c "cabal repl exe:example --extra-lib-dirs=${config.packages.rsdd}/lib" --warnings -T :main
          #   '';
          #   category = "Primary";
          # };
        };

        #devShells.default = pkgs.mkShell {
        #  name = "development shell";
        #  inputsFrom = [
        #    config.haskellProjects.default.outputs.devShell
        #    config.treefmt.build.devShell
        #    config.flake-root.devShell
        #    config.mission-control.devShell
        #  ];
        #  nativeBuildInputs = [
        #  # config.packages.rsdd
        #  ];
        #};
      };
    };
}
