(define-skeleton nix-shell-skeleton
  "" ""
  "#!/usr/bin/env nix-shell\n"
  "\n"
  "with import <nixpkgs> {};\n"
  "mkShell {\n"
  "  buildInputs = [\n"
  "    " _ "\n"
  "  ];\n"
  "}\n"
  )

(define-skeleton nix-package-skeleton
  "" ""
  ;; "{ stdenv, fetchurl }:\n"
  "with import <nixpkgs> {};\n"
  "stdenv.mkDerivation {\n"
  "  name = \"example" _ "-1.0\";\n"
  "  # builder = ./builder.sh;\n"
  "  src = fetchurl {\n"
  "    url = https://example.com;\n"
  "    sha256 = \"\";\n"
  "  };\n"
  "\n"
  "  buildInputs = [\n"
  "    pkg-config\n"
  "  ];\n"
  "}\n"
  _
  )

(define-auto-insert "/shell.nix\\'" 'nix-shell-skeleton)
(define-auto-insert "/default.nix\\'" 'nix-package-skeleton)
