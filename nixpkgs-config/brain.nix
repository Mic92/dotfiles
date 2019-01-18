{ pkgs, ... }: {
  imports = [
    ./common.nix
  ];

  # HACK!!!
  home.packages = with pkgs; [ zsh emacs ];
  programs.emacs.socket-activation.enable = false;

  ## ~/.config/systemd/emacs-daemon.service
  #ExecStart=/local/%u/nix-user-chroot/nix-user-chroot /local/%u/nix zsh -c 'export TMUX=1; . ~/.zshrc; exec emacs --daemon'
  #ExecStop=/local/%u/nix-user-chroot/nix-user-chroot /local/%u/nix %h/.nix-profile/bin/emacsclient --eval (kill-emacs)
  #Restart=always
  #Type=forking

  #[Unit]
  #RefuseManualStart=true

  ## ~/.config/systemd/emacs-daemon.service
  #[Install]
  #WantedBy=sockets.target
  #[Socket]
  #ListenStream=%t/emacs
}
