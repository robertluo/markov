{ pkgs, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [ 
    inputs.nix-claude-code.packages.${pkgs.system}.default
  ];

  # https://devenv.sh/languages/
  languages.clojure.enable = true;

  claude.code.enable = true;

  # https://devenv.sh/processes/
  # `devenv up`: renders every notebook, then again on each change under src/ or notebook/.
  processes.notebooks.exec = "clojure -M:dev:notebook -m robertluo.markov.notebooks";

  # https://devenv.sh/tests/
  enterTest = ''
    clojure -M:dev:test
  '';

  # https://devenv.sh/git-hooks/
  # git-hooks.hooks.shellcheck.enable = true;

  # See full reference at https://devenv.sh/reference/options/
}
