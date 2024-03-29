{

  description = "A Lambda Calculus parser";
  
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
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
