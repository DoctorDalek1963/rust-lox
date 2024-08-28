{
  description = "Rust implementations of the Lox interpreters in the Crafting Interpreters book ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-parts.url = "github:hercules-ci/flake-parts";

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.pre-commit-hooks.flakeModule
      ];

      systems = ["x86_64-linux" "aarch64-linux"];
      perSystem = {
        config,
        system,
        ...
      }: let
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [(import inputs.rust-overlay)];
        };

        rustToolchainNightlyWith = extra: pkgs.rust-bin.selectLatestNightlyWith (toolchain: toolchain.default.override extra);
        rustToolchain = rustToolchainNightlyWith {};

        craneLib = (inputs.crane.mkLib pkgs).overrideToolchain rustToolchain;
        src = craneLib.cleanCargoSource (craneLib.path ./.);

        commonArgs = {
          inherit src;
          strictDeps = true;
        };

        cargoArtifacts = craneLib.buildDepsOnly commonArgs;
      in rec {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            (rustToolchainNightlyWith {
              extensions = ["rust-analyzer" "rust-src" "rust-std"];
            })
            pkgs.just
            pkgs.cargo-nextest
          ];
          shellHook = ''
            ${config.pre-commit.installationScript}
            export RUST_BACKTRACE=1
          '';
        };

        # See https://flake.parts/options/pre-commit-hooks-nix and
        # https://github.com/cachix/git-hooks.nix/blob/master/modules/hooks.nix
        # for all the available hooks and options
        pre-commit.settings.hooks = {
          check-added-large-files.enable = true;
          check-merge-conflicts.enable = true;
          check-toml.enable = true;
          check-vcs-permalinks.enable = true;
          check-yaml.enable = true;

          end-of-file-fixer = {
            enable = true;
            excludes = ["lox-tests/*"];
          };

          trim-trailing-whitespace = {
            enable = true;
            excludes = ["lox-tests/*"];
          };

          rustfmt = {
            enable = true;
            packageOverrides = {
              cargo = rustToolchain;
              rustfmt = rustToolchain;
            };
          };
        };
      };
    };
}
