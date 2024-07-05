{ inputs, lib, ... }:
{

  imports = [ inputs.treefmt-nix.flakeModule ];

  perSystem =
    { inputs', pkgs, ... }:
    {
      # Definitions like this are entirely equivalent to the ones
      # you may have directly in flake.nix.
      devShells.default = pkgs.mkShellNoCC {
        nativeBuildInputs = [
          inputs'.fast-flake-update.packages.default
          pkgs.python3.pkgs.invoke
          pkgs.python3.pkgs.deploykit
          inputs'.clan-core.packages.default
        ] ++ lib.optionals (!pkgs.stdenv.isDarwin) [ pkgs.bubblewrap ];
      };

      treefmt = {
        # Used to find the project root
        projectRootFile = ".git/config";

        programs.hclfmt.enable = true;
        programs.yamlfmt.enable = true;
        programs.mypy.directories = {
          "tasks" = {
            directory = ".";
            modules = [ ];
            files = [ "**/tasks.py" ];
            extraPythonPackages = [
              pkgs.python3.pkgs.deploykit
              pkgs.python3.pkgs.invoke
            ];
          };
          "nixos/eva/modules/prometheus" = { };
          "openwrt" = { };
          "home-manager/modules/neovim" = {
            options = [ "--ignore-missing-imports" ];
          };
        };
        programs.deadnix.enable = true;
        programs.stylua.enable = true;
        programs.clang-format.enable = true;
        programs.deno.enable = true;
        programs.nixfmt.enable = true;
        programs.nixfmt.package = pkgs.nixfmt-rfc-style;
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
          "terraform.tfstate"
          "*.tfvars.sops.json"
          "gdb/*"
          "*nixos-vars.json"
          "*/secrets.yaml"
          "*/secrets.yml"
          "machines/*/facts/*"
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
          "home/bin/*"
          "nixos/eve/modules/nginx/screenshare/index.html"
          "nixos/eve/pkgs/logo.png"
          "nixos/turingmachine/modules/vpn-il1-standard.ovpn"
          "nixos/turingmachine/thermal-conf.xml.auto"
          "openwrt/Justfile"
          "openwrt/bin/nix-uci"
          "openwrt/setup.cfg"
        ];
      };
    };
}
