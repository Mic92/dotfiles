# My personal bootstrap process

```console
$ sudo pacman -S git
```

    or

```console
$ sudo apt-get install git-core

$ git clone --depth=1 https://github.com/andsens/homeshick.git $HOME/.homesick/repos/homeshick
$ alias homeshick="$HOME/.homesick/repos/homeshick/bin/homeshick"
$ homeshick clone https://github.com/Mic92/dotfiles.git
```

Essential packages:

arch:

```console
$ pacman -S base-devel the_silver_searcher htop git tig zsh tmux vim ruby strace tcpdump lsof rsync sudo
```

freebsd:

```console
$ pkg install the_silver_searcher git tmux zsh vim-lite ruby tcpdump lsof rsync sudo bash
```

debian/ubuntu:

```console
$ apt-get install build-essential silversearcher-ag htop git-core tig zsh tmux vim-nox ruby strace tcpdump lsof rsync sudo
```

or minimal:

```console
$ sudo apt-get install htop git-core zsh tmux vim-nox
```

Bootstrap yaourt

```console
$ cat > aur.sh <<'EOF'
#!/bin/bash
d=${BUILDDIR:-$PWD}
for p in ${@##-*}
do
cd "$d"
curl "https://aur.archlinux.org/cgit/aur.git/snapshot/${p}.tar.gz"|tar xz
cd "$p"
makepkg ${@##[^\-]*}
done
EOF
```

```console
$ bash aur.sh -si package-query yaourt
```

Bootstrap user:

```console
$ useradd -m -s /bin/zsh joerg
$ gpasswd -a joerg wheel # sudo on debian
$ passwd joerg
$ install -d -m 700 -o joerg -g joerg ~joerg/.ssh
$ cat >> /tmp/authorized_keys <<'EOF'
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine
EOF
$ install -m 400 -o joerg -g joerg /tmp/authorized_keys ~joerg/.ssh/authorized_keys && rm /tmp/authorized_keys
```

Boostrap nix-user:

```console
$ install -d -m755 -o joerg -g joerg /nix
$ curl https://nixos.org/nix/install | sh
```

Boostrap nix-multiuser:

```console
$ curl https://nixos.org/nix/install -o install
$ sh ./install --daemon
```

Boostrap nixpkgs:

```console
$ git clone https://github.com/Mic92/nixpkgs/ ~/git/nixpkgs
$ (cd ~/git/nixpkgs && git remote add upstream https://github.com/NixOS/nixpkgs.git)
$ nix-shell https://github.com/rycee/home-manager/archive/master.tar.gz -A install
$ home-manager switch
```

or in zsh

```console
$ boostrap-home-manager
```


Bootstrap system:

```console
sed -i '/^#.*de_DE.UTF-8/s/^#//g;/^#.*en_DK.UTF-8/s/^#//g' /etc/locale.gen && locale-gen
```
