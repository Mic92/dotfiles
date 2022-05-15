{ pkgs, ... }:
{
   services.uptermd = {
    enable = true;
    openFirewall = true;
    port = 2323;
  };

  services.nginx.virtualHosts."upterm.thalheim.io" = let
    css = pkgs.fetchurl {
      url = "https://gist.githubusercontent.com/killercup/5917178/raw/40840de5352083adb2693dc742e9f75dbb18650f/pandoc.css";
      sha256 = "sha256-SzSvxBIrylxBF6B/mOImLlZ+GvCfpWNLzGFViLyOeTk=";
    };
  in {
    useACMEHost = "thalheim.io";
    forceSSL = true;
    locations."/" = {
      root = pkgs.runCommand "webroot" { nativeBuildInputs = [ pkgs.pandoc ]; } ''
        cat > index.md <<'EOF'
        # Usage
        On the machine where you want to share a tmux session first run:

        ```
        upterm host --server ssh://upterm.thalheim.io:2323 --force-command 'tmux attach -t pair-programming' -- tmux new -t pair-programming
        ```

        Then in the tmux session run to get sharing command to send to your peer:

        ```
        upterm session info $(upterm session list | grep -o -E '| [a-z0-9A-z]{20} |')
        ```

        This will output something like:

        ```
        === YXBRHY1BXARIWTSGXJ8L
        Command:                tmux new -t pair-programming
        Force Command:          tmux attach -t pair-programming
        Host:                   ssh://upterm.thalheim.io:2323
        SSH Session:            ssh yXbrhy1bXaRIwTsgxJ8l:Wzo6XToyMjIy@upterm.thalheim.io -p 2323
        ```

        EOF
        mkdir $out
        cp ${css} $out/pandoc.css
        pandoc --css="pandoc.css" index.md --to=html5 -s -f markdown+smart --metadata pagetitle="Upterm: Secure terminal sharing" -o $out/index.html
      '';
    };
  };
}
