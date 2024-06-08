{
  description = "DBCaml is a database library for OCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    riot = {
      url = "github:emilpriver/riot";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    bytestring = {
      url = "github:riot-ml/bytestring";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    atacama = {
      url = "github:suri-framework/atacama";
    };
    serde = {
      url = "github:serde-ml/serde";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          pkgs = nixpkgs.legacyPackages."${system}".extend (self: super: {
            ocamlPackages = super.ocaml-ng.ocamlPackages_5_1;
          });
          inherit (pkgs) ocamlPackages mkShell;
          inherit (ocamlPackages) buildDunePackage;
          version = "0.0.2+dev";
        in
        {
          devShells = {
            default = mkShell {
              buildInputs = [
                ocamlPackages.dune_3
                ocamlPackages.ocaml
                ocamlPackages.utop
                ocamlPackages.ocamlformat
              ];
              inputsFrom = [
                self'.packages.default
                self'.packages.dbcaml_driver_postgres
                self'.packages.silo_postgres
                self'.packages.serde_postgres
              ];
              packages = builtins.attrValues {
                inherit (pkgs) clang_17 clang-tools_17 pkg-config;
                inherit (ocamlPackages) ocaml-lsp ocamlformat-rpc-lib;
              };
              dontDetectOcamlConflicts = true;
            };
          };
          packages = {
            randomconv = buildDunePackage {
              version = "0.2.0";
              pname = "randomconv";
              src = pkgs.fetchFromGitHub {
                owner = "hannesm";
                repo = "randomconv";
                rev = "b2ce656d09738d676351f5a1c18aff0ff37a7dcc";
                hash = "sha256-KIvx/UNtPTg0EqfwuJgzSCtr6RgKIXK6yv9QkUUHbJk=";
              };
            };
            random = buildDunePackage {
              version = "0.0.1";
              pname = "random";
              src = pkgs.fetchFromGitHub {
                owner = "leostera";
                repo = "random";
                rev = "abb07c253dbc208219ac1983b34c78dab5fe93fd";
                hash = "sha256-dcJDuWE3qLEanu+TBBSeJPxxQvAN9eq88R5W3XMEGiA=";
              };
              propagatedBuildInputs = with ocamlPackages; [
                mirage-crypto-rng
                mirage-crypto
                self'.packages.randomconv
              ];
            };
            default = buildDunePackage {
              inherit version;
              pname = "dbcaml";
              propagatedBuildInputs = with ocamlPackages; [
                inputs'.riot.packages.default
                self'.packages.randomconv
                alcotest
                uri
              ];
              src = ./.;
            };
            dbcaml_driver_postgres = buildDunePackage {
              inherit version;
              pname = "dbcaml-driver-postgres";
              propagatedBuildInputs = with ocamlPackages; [
                inputs'.riot.packages.default
                inputs'.bytestring.packages.default
                self'.packages.default
                self'.packages.random
                self'.packages.serde_postgres
                alcotest
                uri
                cryptokit
              ];
              src = ./.;
            };
            serde_postgres = buildDunePackage {
              inherit version;
              pname = "serde_postgres";
              propagatedBuildInputs = with ocamlPackages; [
                inputs'.serde.packages.default
                inputs'.serde.packages.serde_derive
                alcotest
              ];
              src = ./.;
            };
            silo_postgres = buildDunePackage {
              inherit version;
              pname = "silo_postgres";
              propagatedBuildInputs = with ocamlPackages; [
                self'.packages.default
                self'.packages.dbcaml_driver_postgres
                self'.packages.serde_postgres
                alcotest
              ];
              src = ./.;
            };
          };
          formatter = pkgs.nixpkgs-fmt;
        };
    };
}

