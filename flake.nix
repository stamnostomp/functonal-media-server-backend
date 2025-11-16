{
  description = "Jellyfin-HS Backend - Haskell media server with ffmpeg streaming";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Using GHC 9.4 as required by the project (base ^>=4.18.0.0)
        haskellPackages = pkgs.haskell.packages.ghc94;
        
        # Override for your package - note: cabal file is named "jellyfin-hs"
        jellyfin-hs = haskellPackages.callCabal2nix "jellyfin-hs" ./. { };
        
      in
      {
        # Default package
        packages.default = jellyfin-hs;
        
        # Development shell
        devShells.default = haskellPackages.shellFor {
          packages = p: [ jellyfin-hs ];
          
          buildInputs = with pkgs; [
            # Haskell development tools
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.ghcid
            haskellPackages.hlint
            haskellPackages.ormolu
            haskellPackages.hoogle
            
            # System dependencies for jellyfin-hs
            zlib
            pkg-config
            postgresql        # Database backend
            ffmpeg-full       # Video transcoding
            
            # Additional utilities
            git
          ];
          
          withHoogle = true;
          
          shellHook = ''
            echo "======================================"
            echo "Jellyfin-HS Backend Dev Environment"
            echo "======================================"
            echo ""
            echo "GHC version: $(ghc --version)"
            echo "Cabal version: $(cabal --version | head -n1)"
            echo "PostgreSQL: $(postgres --version 2>/dev/null || echo 'available')"
            echo "FFmpeg: $(ffmpeg -version 2>/dev/null | head -n1 || echo 'available')"
            echo ""
            echo "Available commands:"
            echo "  cabal build          - Build the project"
            echo "  cabal run jellyfin-hs - Run the server"
            echo "  cabal test           - Run tests"
            echo "  ghcid                - Auto-recompiling daemon"
            echo "  hoogle server --local - Start local Hoogle server"
            echo ""
            echo "Environment variables (set in .env or export):"
            echo "  DB_HOST=${DB_HOST:-localhost}"
            echo "  DB_PORT=${DB_PORT:-5432}"
            echo "  DB_USER=${DB_USER:-jellyfin}"
            echo "  DB_NAME=${DB_NAME:-jellyfin}"
            echo "  SERVER_PORT=${SERVER_PORT:-8080}"
            echo "  MEDIA_ROOT=${MEDIA_ROOT:-./media}"
            echo ""
            
            # Set default environment variables
            export DB_HOST=''${DB_HOST:-localhost}
            export DB_PORT=''${DB_PORT:-5432}
            export DB_USER=''${DB_USER:-jellyfin}
            export DB_PASSWORD=''${DB_PASSWORD:-jellyfin}
            export DB_NAME=''${DB_NAME:-jellyfin}
            export SERVER_PORT=''${SERVER_PORT:-8080}
            export MEDIA_ROOT=''${MEDIA_ROOT:-./media}
          '';
        };
        
        # Minimal development shell (faster to build)
        devShells.minimal = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.ghc
            haskellPackages.cabal-install
            zlib
            postgresql
            ffmpeg-full
          ];
        };
        
        # Apps
        apps.default = flake-utils.lib.mkApp {
          drv = jellyfin-hs;
        };
        
        # Docker image (optional, for deployment)
        packages.dockerImage = pkgs.dockerTools.buildImage {
          name = "jellyfin-hs";
          tag = "latest";
          
          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths = [ 
              jellyfin-hs
              pkgs.ffmpeg-full
              pkgs.coreutils
            ];
            pathsToLink = [ "/bin" ];
          };

          config = {
            Cmd = [ "${jellyfin-hs}/bin/jellyfin-hs" ];
            ExposedPorts = {
              "8080/tcp" = {};
            };
            Env = [
              "DB_HOST=db"
              "DB_PORT=5432"
              "DB_USER=jellyfin"
              "DB_NAME=jellyfin"
              "SERVER_PORT=8080"
              "MEDIA_ROOT=/media"
            ];
          };
        };
      }
    );
}
