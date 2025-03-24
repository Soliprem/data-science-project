{
  description = "flake for writing in r";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  } @ inputs:
    flake-utils.lib.eachDefaultSystem (system: let
      inherit (self) outputs;
      pkgs = nixpkgs.legacyPackages.x86_64-linux; # Home-manager requires 'pkgs' instance
    in {
      packages.x86_64-linux.default = pkgs.stdenv.mkDerivation {
        pname = "run-hook";
        version = "1.0";

        buildInputs = with pkgs; [
          R
          rPackages.tidyverse
          rPackages.kableExtra
          rPackages.GGally
          rPackages.gridExtra
          rPackages.rddensity
          rPackages.modelsummary
          pandoc
        ];

        # The hook you want to run
        buildPhase = ''
          Rscript main.R
          pandoc artifacts/gov_transfers.md -o artifacts/gov_transfers.typ
          pandoc artifacts/gov_transfers_fuzziness.md -o artifacts/gov_transfers_fuzziness.typ
        '';

        installPhase = "mkdir -p $out";

        src = ./.;
      };
      devShells.default = pkgs.mkShell {
        packages = with pkgs; [
          rstudio
          R
          rPackages.tidyverse
          rPackages.kableExtra
          rPackages.GGally
          rPackages.gridExtra
          rPackages.rddensity
          rPackages.modelsummary
          pandoc
        ];
        shellHook = ''
          nu
          Rscript main.R
          pandoc artifacts/gov_transfers.md -o artifacts/gov_transfers.typ
          pandoc artifacts/gov_transfers_fuzziness.md -o artifacts/gov_transfers_fuzziness.typ
        '';
      };
    });
}
