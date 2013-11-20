# Install with homeshick

    $ sudo pacman -S git

    or

    $ sudo apt-get install git-core

    $ git clone git://github.com/andsens/homeshick.git $HOME/.homesick/repos/homeshick

    $ alias homeshick="source $HOME/.homesick/repos/homeshick/bin/homeshick.sh"

    $ homeshick clone git://github.com/Mic92/dotfiles.git

    $ homeshick clone git://github.com/Mic92/scripts.git

    $ (cd ~/.homesick/repos/dotfiles && git submodule foreach --recursive "git pull -u origin master")

    $ homeshick symlink

    $ curl -L https://get.rvm.io | bash -s stable --ruby

    $ vim +BundleInstall +qall
