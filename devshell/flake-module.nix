{ inputs, lib, ... }:
{

  imports = [ inputs.treefmt-nix.flakeModule ];

  perSystem =
    {
      inputs',
      pkgs,
      ...
    }:
    {
      # Definitions like this are entirely equivalent to the ones
      # you may have directly in flake.nix.
      devShells.default = pkgs.mkShellNoCC {
        nativeBuildInputs = [
          pkgs.python3.pkgs.invoke
          pkgs.python3.pkgs.deploykit
          inputs'.clan-core.packages.default
          # FIXME: clan-app currently fails to build due to webview-nightly compilation issues
          # with the latest nixpkgs. Re-enable once the upstream issue is resolved.
          # inputs'.clan-core.packages.clan-app
        ]
        ++ lib.optionals (!pkgs.stdenv.isDarwin) [
          pkgs.bubblewrap
        ];
      };

      treefmt = {
        # Used to find the project root
        projectRootFile = ".git/config";

        programs.terraform.enable = true;
        programs.hclfmt.enable = true;
        programs.yamlfmt.enable = true;
        programs.actionlint.enable = true;
        programs.mypy.enable = true;
        programs.mypy.directories = {
          "machines/eva/modules/prometheus" = { };
          "openwrt" = { };
          "pkgs/buildbot-pr-check" = {
            extraPythonPackages = with pkgs.python3.pkgs; [
              pytest
              vcrpy
              pytest-vcr
            ];
          };
          "pkgs/claude-md" = { };
          "pkgs/merge-when-green" = { };
          "pkgs/systemctl" = { };
          "pkgs/vcal" = {
            extraPythonPackages = with pkgs.python3.pkgs; [
              icalendar
              python-dateutil
              pytz
              types-pytz
              types-python-dateutil
              (pkgs.callPackage ../pkgs/vcal/types-icalendar.nix { inherit python; })
              pytest
            ];
          };
          "pkgs/rbw_pinentry" = {
            extraPythonPackages = with pkgs.python3.pkgs; [
              keyring
            ];
          };
          "pkgs/calendar_bot" = {
            extraPythonPackages = with pkgs.python3.pkgs; [
              aiohttp
              mautrix
              asyncpg
              python-olm
              unpaddedbase64
              pycryptodome
              base58
            ];
          };
        };
        programs.deadnix.enable = true;
        programs.stylua.enable = true;
        programs.clang-format.enable = true;
        programs.deno.enable = true;
        programs.nixfmt.enable = true;
        programs.shellcheck.enable = true;

        settings.formatter.shellcheck.options = [
          "--external-sources"
          "--source-path=SCRIPTDIR"
        ];

        programs.shfmt.enable = true;
        programs.rustfmt.enable = true;
        settings.formatter.shfmt.includes = [
          "*.envrc"
          "*.envrc.private-template"
          "*.bashrc"
          "*.bash_profile"
          "*.bashrc.load"
        ];

        programs.ruff.format = true;
        programs.ruff.check = true;

        settings.formatter.ruff-check.excludes = [
          "gdb/*"
          "zsh/*"
          "home/.config/qtile/*"
          "home/.emacs/*"
          # bug in ruff
          "home/.config/shell_gpt/functions/execute_shell.py"
        ];
        settings.formatter.ruff-format.excludes = [
          "gdb/*"
          "zsh/*"
        ];
        settings.formatter.shfmt.excludes = [
          "gdb/*"
          "zsh/*"
        ];
        settings.formatter.shellcheck.excludes = [
          "gdb/*"
          "zsh/*"
        ];

        settings.global.excludes = [
          "sops/*"
          "vars/*"
          "zsh/*"
          "pkgs/buildbot-pr-check/tests/cassettes/*"
          "home/.zsh-*"
          "home/.fast-syntax-highlighting"
          "home/.config/nixpkgs"
          "home/.gef-*"
          "terraform.tfstate"
          "*.tfvars.sops.json"
          "gdb/*"
          "*nixos-vars.json"
          "*/secrets.yaml"
          "*/secrets.yml"
          "machines/*/facts/*"
          "darwin/*/*.yml"
          "*/facter.json"
          "*.pub"
          "*.pem"
          "*.conf"
          "*.sieve"
          "*.patch"
          "*.zone"
          "*.lock"
          "*.age"
          "*.fish"
          "*.txt"
          "*.toml"
          "*.vim"
          "*.el"
          "*.config"
          "*.png"
          "*.jpg"
          "*.jpeg"
          "*.gif"
          "*.svg"
          "*.ico"
          "home/.gdbinit-gef.py"
          "home/.emacs.d/templates/*"
          "home/.doom.d/snippets/*"
          "*/secrets.enc.json"
          "*/lazy-lock.json"

          "*.gitignore"
          "*.gitmodules"
          "home-manager/modules/waybar.css"
          "home/.Xresources"
          "home/.agignore"
          "home/.config/autorandr/*"
          "home/.config/bat/*"
          "home/.config/dunst/dunstrc"
          "home/.config/foot/foot.ini"
          "home/.config/htop/htoprc"
          "home/.config/kanshi/config"
          "home/.config/nvim/treesitter-rev"
          "home/.config/river/init"
          "home/.config/rofi/*"
          "home/.dircolors.*"
          "home/.direnvrc"
          "home/.gdbinit"
          "home/.gef.rc"
          "home/.gemrc"
          "home/.gitattributes"
          "home/.gitconfig"
          "home/.gitignore"
          "home/.hgrc"
          "home/.irbrc"
          "home/.mbsyncrc"
          "home/.mpv/config"
          "home/.ncmpcpp/config"
          "home/.ncmpcpp/keys"
          "home/.parallel/will-cite"
          "home/.pryrc"
          "home/.psqlrc"
          "home/.radare2rc"
          "home/.ruby-agignore"
          "home/.spacemacs"
          "home/.tigrc"
          "home/.vimrc"
          "home/.xinitrc"
          "home/.zsh-termsupport"
          "home/.zshrc"
          "home/.fish-pure"
          "fish/*"
          "home/bin/*"
          "machines/eve/pkgs/logo.png"
          "machines/turingmachine/modules/vpn-il1-standard.ovpn"
          "machines/turingmachine/thermal-conf.xml.auto"
          "openwrt/Justfile"
          "openwrt/bin/nix-uci"
          "openwrt/setup.cfg"
        ];
      };
    };
}
