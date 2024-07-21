{
  description = "DBCaml is a database library for OCaml";

  inputs = {
    nixpkgs.url = "github:nix-ocaml/nix-overlays";
    riot = {
      url = "github:riot-ml/riot";
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

  outputs = inputs @ {
    flake-parts,
    nixpkgs,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: let
        # pkgs = nixpkgs.legacyPackages."${system}".extend (self: super: {
        #   ocamlPackages = super.ocaml-ng.ocamlPackages_5_1;
        # });
        inherit (pkgs) ocamlPackages mkShell;
        inherit (ocamlPackages) buildDunePackage;
        version = "0.0.6+dev";
      in {
        devShells = {
          default = mkShell {
            buildInputs = [
              ocamlPackages.dune_3
              ocamlPackages.ocaml
              ocamlPackages.utop
            ];
            inputsFrom = [
              self'.packages.default
              self'.packages.dbcaml_driver_postgres
              self'.packages.dbcaml_driver_sqlite
              self'.packages.silo
              self'.packages.serde_postgres
            ];
            packages = builtins.attrValues {
              inherit (pkgs) clang_17 clang-tools_17 pkg-config;
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
          sqlite3 = buildDunePackage {
            version = "5.1.0";
            pname = "sqlite3";
            src = pkgs.fetchFromGitHub {
              owner = "mmottl";
              repo = "sqlite3-ocaml";
              hash = "sha256-+Pd+PJhpujQuvKioZXgP/vAgtluYKnFJj/7CpZwvZ7o=";
              rev = "bdddd31cf201b2877600282345c695d71435cc35";
            };
            propagatedBuildInputs = with ocamlPackages; [
              dune-configurator
              pkgs.sqlite
            ];
            propagatedNativeBuildInputs = with pkgs; [
              pkg-config
            ];
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
          dbcaml_driver_sqlite = buildDunePackage {
            inherit version;
            pname = "dbcaml-driver-sqlite";
            propagatedBuildInputs = with ocamlPackages; [
              inputs'.riot.packages.default
              self'.packages.sqlite3
              self'.packages.default
              alcotest
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
          silo = buildDunePackage {
            inherit version;
            pname = "silo";
            propagatedBuildInputs = with ocamlPackages; [
              self'.packages.default
              self'.packages.dbcaml_driver_postgres
              self'.packages.serde_postgres
              alcotest
            ];
            src = ./.;
          };
        };
        formatter = pkgs.alejandra;
      };
    };
}
