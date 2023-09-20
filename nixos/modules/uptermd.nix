{ pkgs, ... }: {
  services.uptermd = {
    enable = true;
    openFirewall = true;
    port = 2323;
    extraFlags = [ "--hostname" "upterm.thalheim.io" ];
  };

  services.nginx.virtualHosts."upterm.thalheim.io" =
    let
      css = pkgs.fetchurl {
        url = "https://gist.githubusercontent.com/killercup/5917178/raw/40840de5352083adb2693dc742e9f75dbb18650f/pandoc.css";
        sha256 = "sha256-SzSvxBIrylxBF6B/mOImLlZ+GvCfpWNLzGFViLyOeTk=";
      };
    in
    {
      useACMEHost = "thalheim.io";
      forceSSL = true;
      locations."/" = {
        root = pkgs.runCommand "webroot" { nativeBuildInputs = [ pkgs.pandoc ]; } ''
          cat > index.md <<'EOF'
          # Usage
          On the machine where you want to share a tmux session with [upterm](https://github.com/owenthereal/upterm) first run:

          ```
          upterm host --github-user 'githubusername' --server ssh://upterm.thalheim.io:2323 --force-command 'tmux attach -t pair-programming' -- tmux new -t pair-programming
          ```

          Replace `githubusername` with the user you want to share the session with.

          This will output something like:

          ```
          === YXBRHY1BXARIWTSGXJ8L
          Command:                tmux new -t pair-programming
          Force Command:          tmux attach -t pair-programming
          Host:                   ssh://upterm.thalheim.io:2323
          SSH Session:            ssh yXbrhy1bXaRIwTsgxJ8l:Wzo6XToyMjIy@upterm.thalheim.io -p 2323
          Press enter to continue
          ```

          Copy the SSH Session url and press enter before sharing the url with your peer.

          You can also put the following snippet in your ~/.bashrc or ~/.zshrc:

          ```
          tmux-upterm() {
            if [ -z "$1" ]; then
              echo "Usage: tmux-upterm <github-username>"
              return 1
            fi
            upterm host --github-user "$1" --server ssh://upterm.thalheim.io:2323 \
              --force-command 'tmux attach -t pair-programming' \
              -- tmux new -t pair-programming
          }
          ```

          Source code of this service is [here](https://github.com/Mic92/dotfiles/blob/master/nixos/modules/uptermd.nix)
          EOF
          mkdir $out
          cp ${css} $out/pandoc.css
          pandoc --css="pandoc.css" index.md --to=html5 -s -f markdown+smart --metadata pagetitle="Upterm: Secure terminal sharing" -o $out/index.html
        '';
      };
    };
}
