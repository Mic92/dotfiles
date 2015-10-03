# My personal bootstrap process

    $ sudo pacman -S git

    or

    $ sudo apt-get install git-core

    $ git clone --depth=1 git://github.com/andsens/homeshick.git $HOME/.homesick/repos/homeshick
    $ alias homeshick="source $HOME/.homesick/repos/homeshick/bin/homeshick"
    $ homeshick clone git://github.com/Mic92/dotfiles.git
    $ ( cd ~/.homesick/repos/dotfiles && git submodule foreach --recursive "git pull origin master" )

    $ homeshick clone git://github.com/Mic92/scripts.git
    $ vim +PlugInstall +qall

Essential packages:

arch:

    $ pacman -S base-devel the_silver_searcher htop git tig zsh tmux vim ruby strace tcpdump lsof rsync sudo

freebsd:

    $ pkg install the_silver_searcher git tmux zsh vim-lite ruby tcpdump lsof rsync sudo bash

debian:

    $ apt-get install build-essentials silversearcher-ag htop git-core tig zsh tmux vim-nox ruby strace tcpdump lsof rsync sudo

Bootstrap yaourt

```
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

```
$ bash aur.sh -si package-query yaourt
```

Bootstrap user:

```
$ useradd -m -s /bin/zsh joerg

$ gpasswd -a joerg wheel # sudo on debian

$ passwd joerg

$ install -d -m 700 -o joerg -g joerg ~joerg/.ssh

$ cat >> /tmp/authorized_keys <<'EOF'
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKbBp2dH2X3dcU1zh+xW3ZsdYROKpJd3n13ssOP092qE joerg@turingmachine
EOF

$ install -m 400 -o joerg -g joerg /tmp/authorized_keys ~joerg/.ssh/authorized_keys && rm /tmp/authorized_keys
```

Bootstrap system:

```
sed -i '/^#.*de_DE.UTF-8/s/^#//g;/^#.*en_DK.UTF-8/s/^#//g' /etc/locale.gen && locale-gen
```
