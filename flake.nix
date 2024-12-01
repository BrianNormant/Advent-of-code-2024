{
	description = "devenv for idris2 and java";

	inputs = {
		nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
	};

	outputs = { nixpkgs, ... }: let
		system = "x86_64-linux";
		pkgs = import nixpkgs { inherit system; };
	in {
		devShells."${system}".default = pkgs.mkShell {
			packages = with pkgs; [
				gradle
				jdk23
				zsh
				idris2Packages.pack
				gmp
				chez
				rlwrap
				idris2
			];
			shellHook = ''
				exec zsh
			'';
		};
	};
}
