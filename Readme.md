# My personal bootstrap process

**Radicle**: https://radicle.thalheim.io/nodes/radicle.thalheim.io/rad:z2dqRKkK5yu89w3CMX2dVsYrRwvFk

For bootstrapping on
[legacy operating systems](https://github.com/Mic92/dotfiles/wiki#bootstrap-for-legacy-operating-systems)

# Bootstrap dotfiles when having nix

```console
$ nix run github:Mic92/dotfiles
```

# Bootstrap homeshick without nix

```console
$ nix-shell -p git
nix-shell> git clone --depth=1 https://github.com/andsens/homeshick.git $HOME/.homesick/repos/homeshick
nix-shell> alias homeshick="$HOME/.homesick/repos/homeshick/bin/homeshick"
nix-shell> homeshick clone https://github.com/Mic92/dotfiles.git
```

# Bootstrap single user nix:

```console
$ install -d -m755 -o joerg -g joerg /nix
$ curl https://nixos.org/nix/install | sh
```

# Bootstrap multi-user nix:

```console
$ curl https://nixos.org/nix/install -o install
$ sh ./install --daemon
```

# Bootstrap home-manager

```console
$ hm switch
```

# Use my nvim configuration as a standalone without installing dotfiles

```console
$ nix run 'github:Mic92/dotfiles#nvim'
```

# Need help with nix?

Drop me an email at joerg@thalheim.io to book consultation on Nix/NixOS/DevOps
related topics
