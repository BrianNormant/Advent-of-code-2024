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
                pkg-config
                SDL # TODO clone and fix the Makefile of
                # https://github.com/ECburx/Idris2GL/blob/main/src/c_src/Makefile
                # to link -lSDL
                # also override the package in ~/pack/user.toml
                SDL2
                SDL2_ttf
                SDL2_gfx
                SDL2_mixer
                SDL2_image
			];

			shellHook = ''
				export SHELL=zsh
                # override idris2 and idris2-lsp to the lastest version compiled by pack
                export PATH=$PATH:/home/brian/.pack/bin
				exec zsh
			'';
		};
	};
}
