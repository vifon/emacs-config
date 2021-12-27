(define-skeleton nix-shell-skeleton
  "" ""
  "{ pkgs ? import <nixpkgs> {} }:\n"
  "\n"
  "with pkgs; mkShell {\n"
  "  buildInputs = [\n"
  "    " _ "\n"
  "  ];\n"
  "}\n"
  )

(define-skeleton nix-shell-python-skeleton
  "" ""
  "{ pkgs ? import <nixpkgs> {} }:\n"
  "\n"
  "with pkgs; mkShell {\n"
  "  buildInputs = let pyenv = ps: with ps; [\n"
  "    " _ "\n"
  "  ]; in [\n"
  "    (python3.withPackages pyenv)\n"
  "  ];\n"
  "}\n"
  )

(define-skeleton nix-docker-skeleton
  "" ""
  "{ pkgs ? import <nixpkgs> {} }:\n"
  "\n"
  "with pkgs;\n"
  "dockerTools.buildImage {\n"
  "  name = \"" (skeleton-read "Image name: "
                                (file-name-nondirectory
                                 (directory-file-name
                                  default-directory))) "\";\n"
  "  tag = \"latest\";\n"
  "\n"
  "  contents = [ coreutils" _ " ];\n"
  "\n"
  "  config = {\n"
  "  Cmd = [ \"${bash}/bin/bash\" ];\n"
  "  };\n"
  "}\n"
  )

(define-skeleton nix-package-skeleton
  "" ""
  "{ pkgs ? import <nixpkgs> {} }:\n"
  "\n"
  "with pkgs;\n"
  "stdenv.mkDerivation rec {\n"
  "  pname = \"" (skeleton-read "Package name: "
                                (file-name-nondirectory
                                 (directory-file-name
                                  default-directory))) "\";\n"
  "  version = \"" (skeleton-read "Package version: " "1.0") "\";\n"
  (if (y-or-n-p "Fetch from Github?")
      (concat "  src = fetchFromGitHub {\n"
              "    owner = \"" (skeleton-read "Repo owner: " (user-login-name)) "\";\n"
              "    repo = pname;\n"
              "    rev = \"v${version}\";\n"
              "    sha256 = \"\";\n"
              "  };\n")
    (concat "  src = fetchurl {\n"
            "    url = " (setq v1 (skeleton-read "Fetch URL: ")) ";\n"
            "    sha256 = \"" (when (and (not (string-empty-p v1))
                                         (y-or-n-p "Fetch & calculate the hash?"))
                                (shell-command-to-string (concat "nix-prefetch-url "
                                                                 "\"" v1 "\""
                                                                 " 2> /dev/null"))) "\";\n"
            "  };\n"))
  "\n"
  "  buildInputs = [\n"
  "    pkg-config" _ "\n"
  "  ];\n"
  "}\n"
  )

(define-skeleton nix-module-skeleton
  "" ""
  "{ config, pkgs, lib, ... }:\n"
  "\n"
  "let cfg = config.services." (setq v1 (skeleton-read "Module name: ")) ";\n"
  "in {\n"
  "  options = {\n"
  "    services." v1 " = {\n"
  "      enable = lib.mkOption {\n"
  "        default = false;\n"
  "        type = with lib.types; bool;\n"
  "        description = ''\n"
  "        '';\n"
  "      };\n"
  "    };\n"
  "  };\n"
  "\n"
  "  config = lib.mkIf cfg.enable {\n"
  "    environment.systemPackages = [ " _ "];\n"
  "  };\n"
  "}\n"
)

(define-auto-insert "/shell\\.nix\\'" #'nix-shell-skeleton)
(define-auto-insert "/default\\.nix\\'" #'nix-package-skeleton)
