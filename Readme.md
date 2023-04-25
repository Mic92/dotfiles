# My personal bootstrap process

For bootstraping on [legacy operating systems](https://github.com/Mic92/dotfiles/wiki#bootstrap-for-legacy-operating-systems)

# Boostrap homeshick

```
$ nix-shell -p git
nix-shell> git clone --depth=1 https://github.com/andsens/homeshick.git $HOME/.homesick/repos/homeshick
nix-shell> alias homeshick="$HOME/.homesick/repos/homeshick/bin/homeshick"
nix-shell> homeshick clone https://github.com/Mic92/dotfiles.git
```

# Boostrap single user nix:

```console
$ install -d -m755 -o joerg -g joerg /nix
$ curl https://nixos.org/nix/install | sh
```

# Boostrap multi-user nix:

```console
$ curl https://nixos.org/nix/install -o install
$ sh ./install --daemon
```

# Boostrap home-manager

```console
$ hm switch
```

# Articles about my dotfile repo

- Explains basic structure: https://samleathers.com/posts/2022-02-03-my-new-network-and-deploy-rs.html

# Need professional help with nix?

Drop me an email at joerg@thalheim.io for consultation.
