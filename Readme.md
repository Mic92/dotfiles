# My personal bootstrap process

    $ sudo pacman -S git

    or

    $ sudo apt-get install git-core


    $ git clone git://github.com/andsens/homeshick.git $HOME/.homesick/repos/homeshick

    $ alias homeshick="source $HOME/.homesick/repos/homeshick/bin/homeshick"

    $ homeshick clone git://github.com/Mic92/dotfiles.git

    $ homeshick clone git://github.com/Mic92/scripts.git

    $ (cd ~/.homesick/repos/dotfiles && git submodule foreach --recursive "git pull -u origin master")

    $ homeshick symlink

    $ vim +BundleInstall +qall

Essential packages:

    $ pacman -S htop git tig zsh tmux vim ruby strace tcpdump lsof rsync sudo
    
    $ pkg install git tmux zsh vim-lite ruby tcpdump lsof rsync sudo

Bootstrap yaourt

    $ pacman -S --needed base-devel

    $ cat > aur.sh <<'EOF'
#!/bin/bash
d=${BUILDDIR:-$PWD}
for p in ${@##-*}
do
cd "$d"
curl "https://aur.archlinux.org/packages/${p:0:2}/$p/$p.tar.gz" |tar xz
cd "$p"
makepkg ${@##[^\-]*}
done
EOF

    $ bash aur.sh -si yaourt

Bootstrap user:

    $ useradd -m -s /bin/zsh joerg

    $ gpasswd -a joerg wheel # sudo on debian

    $ passwd joerg
