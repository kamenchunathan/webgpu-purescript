{

  description = "A Lambda Calculus parser";
  
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1.*.tar.gz";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    ...
  }@inputs:
    let 
      supportedSystems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];

      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        config = { };
        overlays = builtins.attrValues self.overlays;
      });
      
    in rec { 
      overlays = {
        purescript = inputs.purescript-overlay.overlays.default;
      };
      
      devShells = forAllSystems (system:
        let 
          pkgs = nixpkgsFor.${system};
        in { 
          default = pkgs.mkShell {
            name = "lambda-calculus-parser";
            buildInputs = with pkgs; [
              nodejs_20
              nodePackages.pnpm
              purs-bin.purs-0_15_14
              spago-unstable
              purescript-language-server
              purs-tidy-bin.purs-tidy-0_10_0
              purs-backend-es
            ];
          };
        });
  };
}
