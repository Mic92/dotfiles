# My personal bootstrap process

[![Build Status](https://drone.thalheim.io/api/badges/Mic92/dotfiles/status.svg)](https://drone.thalheim.io/Mic92/dotfiles)


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
$ pacman -S base-devel ripgrep htop git tig zsh tmux vim ruby strace tcpdump lsof rsync sudo
```

freebsd:

```console
$ pkg install ripgrep git tmux zsh vim ruby tcpdump lsof rsync sudo bash
```

debian/ubuntu:

```console
$ apt-get install build-essential ripgrep htop git-core tig zsh tmux vim-nox ruby strace tcpdump lsof rsync sudo
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

Boostrap nixpkgs (in zsh):

```console
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
